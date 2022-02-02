# Targets rakefile script.
require 'rake/clean'
require 'rake/packagetask'

[CLEAN, CLOBBER, Rake::FileList::DEFAULT_IGNORE_PATTERNS].each{|a| a.clear}
CLEAN.include('**/*.o', '*.log', '**/.coverage')
CLOBBER.include('build/*', 'build/.??*')

desc 'Help info'
task :help do
  puts "===== subproject: #{VARS.proj} =====\nHelp: #{RAKE} [SCHEME=\"$(SCHEME}\"] [task]"
  sh "#{RAKE} -T"
end

desc 'Run tests: rake test\[topt1,topt2\]'
task :test, [:topt1] => 'tests/ts_main.scm' do |t, topts|
#	export [DY]LD_LIBRARY_PATH=. # ([da|ba|z]sh Linux)
#	setenv [DY]LD_LIBRARY_PATH . # (tcsh FreeBSD)
  sh "cd build ; env LD_LIBRARY_PATH=#{ENV['LD_LIBRARY_PATH']}:lib #{SCHEME} -A#{SITELIBDIR} -A. -A.. -A../tests ../#{t.source} #{topts[:topt1]} #{topts.extras.join(' ')} || true"
end

#----------------------------------------
desc 'Uninstall artifacts'
task :uninstall do
  installfiles = FileList["#{SITELIBDIR}/*#{VARS.parent}*"]  
  rm_rf(installfiles) || true
end

desc 'Install artifacts'
task :install do
  mkdir_p("#{SITELIBDIR}")
  filelst = FileList["#{VARS.parent}"]
  cp_r(filelst, "#{SITELIBDIR}") || true
  sh "ls #{SITELIBDIR} | grep #{VARS.parent} ; sleep 3"
end

file "build/#{VARS.proj}-#{VARS.version}" do |p|
  mkdir_p(p.name)
  # sh "zip -9 -q -x @exclude.lst -r - . | unzip -od #{p.name} -"
  sh "tar --posix -L -X exclude.lst -cf - . | tar -xpf - -C #{p.name}"
end
if defined? Rake::PackageTask
  Rake::PackageTask.new(VARS.proj, VARS.version) do |p|
    # task("build/#{VARS.proj}-#{VARS.version}").invoke
    
    ENV.fetch('FMTS', 'tar.gz,zip').split(',').each{|fmt|
      if p.respond_to? "need_#{fmt.tr('.', '_')}="
        p.send("need_#{fmt.tr('.', '_')}=", true)
      else
        p.need_tar_gz = true
      end
    }
    task(:package).add_description "[FMTS=#{ENV.fetch('FMTS', 'tar.gz,zip')}]"
    task(:repackage).add_description "[FMTS=#{ENV.fetch('FMTS', 'tar.gz,zip')}]"
  end
else
  desc "[FMTS=#{ENV.fetch('FMTS', 'tar.gz,zip')}] Package project distribution"
  task :package => ["#{VARS.proj}-#{VARS.version}"] do |t|
    distdir = "#{VARS.proj}-#{VARS.version}"
    
    ENV.fetch('FMTS', 'tar.gz,zip').split(',').each{|fmt|
      case fmt
      when '7z'
        rm_rf("#{distdir}.7z") || true
        sh "7za a -t7z -mx=9 #{distdir}.7z #{distdir}" || true
      when 'zip'
        rm_rf("#{distdir}.zip") || true
        sh "zip -9 -q -r #{distdir}.zip #{distdir}" || true
      else
        # tarext = `echo #{fmt} | grep -e '^tar$' -e '^tar.xz$' -e '^tar.zst$' -e '^tar.bz2$' || echo tar.gz`.chomp
        tarext = fmt.match(%r{(^tar$|^tar.xz$|^tar.zst$|^tar.bz2$)}) ? fmt : 'tar.gz'
        rm_rf("#{distdir}.#{tarext}") || true
        sh "tar --posix -L -caf #{distdir}.#{tarext} #{distdir}" || true
      end
    }
  end
end

desc 'Generate API documentation'
task :doc do
  rm_rf("build/share/doc/#{VARS.proj}/html")
  mkdir_p('build/html')
#  cp("#{VARS.parent}/asciidoc_hdr.skel", "build/#{VARS}.proj}.txt")
#  sh "grep -he '^[[:space:]]*;;;' #{VARS.parent}/util.scm #{VARS.parent}/_util.scm | sed G >> build/html/#{VARS.proj}.txt || true"
#  sh "asciidoc -n -a toc -a toclevels=2 build/html/#{VARS.proj}.txt || true"
  sh "asciidoctor -n -a toc -a toclevels=2 -D build/html #{VARS.parent}/#{VARS.proj}.adoc || true"
  mv('html', "share/doc/#{VARS.proj}/html", :force => true) || true
end

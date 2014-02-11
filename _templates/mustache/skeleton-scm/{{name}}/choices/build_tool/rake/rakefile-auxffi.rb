# FFI auxiliary rakefile script
FFI_LIBDIR = `#{PKG_CONFIG} --variable=libdir intro_c-practice || echo .`
FFI_INCDIR = `#{PKG_CONFIG} --variable=includedir intro_c-practice || echo .`
ENV['LD_LIBRARY_PATH'] = "#{ENV['LD_LIBRARY_PATH']}:#{FFI_LIBDIR}"
sh "export LD_LIBRARY_PATH"

CC = 'clang'

if 'Darwin' == `sh -c 'uname -s 2>/dev/null || echo not'`.chomp
  SHLIBEXT = 'dylib'
else
  SHLIBEXT = 'so'
  ENV['LDFLAGS'] = "#{ENV['LDFLAGS']} -Wl,--enable-new-dtags"
end

if '1' == DEBUG
  ENV['CPPFLAGS'] = "#{ENV['CPPFLAGS']} -DDEBUG -UNDEBUG "
  ENV['LDFLAGS'] = "#{ENV['LDFLAGS']} --coverage "
  ENV['CFLAGS'] = "#{ENV['CFLAGS']} -g3 -O0 --coverage "
else
  ENV['CPPFLAGS'] = "#{ENV['CPPFLAGS']} -DNDEBUG -UDEBUG "
  ENV['CFLAGS'] = "#{ENV['CFLAGS']} -O3 "
end

# using gauche-package
VARS.cppflags = "#{ENV['CPPFLAGS']} -Iinclude -I#{FFI_INCDIR} -I `gauche-config --incdirs | sed 's|:| -I|g'`"
VARS.ldflags = "#{ENV['LDFLAGS']} -Wl,-rpath,'$ORIGIN/:#{FFI_LIBDIR}' -Lbuild/lib -L `gauche-config --archdirs | sed 's|:| -L|g'`"
VARS.cflags = "#{ENV['CFLAGS']} -Wall -pedantic -std=c99 -m64"
VARS.ldlibs = "#{ENV['LDLIBS']} `gauche-config -l` -L#{FFI_LIBDIR} -lintro_c-practice"
# using sagittarius-package
#VARS.cppflags = "#{ENV['CPPFLAGS']} -Iinclude -I#{FFI_INCDIR} -I `sagittarius-config -I`"
#VARS.ldflags = "#{ENV['LDFLAGS']} -Wl,-rpath,'$ORIGIN/:#{FFI_LIBDIR}' -Lbuild/lib `sagittarius-config -L` `#{PKG_CONFIG} --libs bdw-gc`"
#VARS.cflags = "#{ENV['CFLAGS']} -Wall -pedantic -std=c99 -m64 `sagittarius-config --c-flags` `#{PKG_CONFIG} --cflags bdw-gc`"
#VARS.ldlibs = "#{ENV['LDLIBS']} `sagittarius-config -l` -L#{FFI_LIBDIR} -lintro_c-practice"

desc 'Compile FFI extension'
task :auxffi do |t|
# using gauche-package
  cd('build') { 
    sh "gauche-package compile -c -n ../#{VARS.parent}/classic_stubslib.stub || true"
    #sh "#{CC} #{VARS.cppflags} #{VARS.ldflags} #{VARS.cflags} -fPIC -shared classic_stubslib.c ../#{VARS.parent}/classic_stubs.c -o #{VARS.parent}-classic_stubs.#{SHLIBEXT} #{VARS.ldlibs} || true"
    sh "gauche-package compile -v --cppflags=\"-I.. -I#{FFI_INCDIR}\" --ldflags=\"-L#{FFI_LIBDIR} -L.\" --libs=\"-lintro_c-practice\" #{VARS.parent}-classic_stubs classic_stubslib.c ../#{VARS.parent}/classic_stubs.c || true"
     }
# using sagittarius-package
#  cd('build') {
#    sh "sagittarius-package .. . #{VARS.parent}/classic_stubslib.stub || true"
#    sh "#{CC} #{VARS.cppflags} #{VARS.ldflags} #{VARS.cflags} -shared classic_stubslib.c ../#{VARS.parent}/classic_stubs.c -o #{VARS.parent}-classic_stubs.#{SHLIBEXT} #{VARS.ldlibs} || true"
#    }
end

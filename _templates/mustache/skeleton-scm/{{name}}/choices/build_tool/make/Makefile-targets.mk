# Targets Makefile script.
#----------------------------------------
# Common automatic variables legend (GNU make: make (Linux) gmake (FreeBSD)):
# $* - basename (cur target)  $^ - name(s) (all depns)  $< - name (1st depn)
# $@ - name (cur target)      $% - archive member name  $? - changed depns

FMTS ?= tar.gz,zip
distdir = $(proj)-$(version)

build/$(distdir) : 
	-@mkdir -p build/$(distdir) ; cp -f exclude.lst build/
#	#-zip -9 -q --exclude @exclude.lst -r - . | unzip -od build/$(distdir) -
	-tar --format=posix --dereference --exclude-from=exclude.lst -cf - . | tar -xpf - -C build/$(distdir)

.PHONY: help clean test uninstall install dist doc
help: ## help
	@echo "##### subproject: $(proj) #####"
	@echo "Usage: $(MAKE) [SCHEME="$(SCHEME)"] [target] -- some valid targets:"
#	-@for fileX in $(MAKEFILE_LIST) `if [ -z "$(MAKEFILE_LIST)" ] ; then echo Makefile Makefile-targets.mk ; fi` ; do \
#		grep -ve '^[A-Z]' $$fileX | awk '/^[^.%][-A-Za-z0-9_]+[ ]*:.*$$/ { print "...", substr($$1, 1, length($$1)) }' | sort ; \
#	done
	-@for fileX in $(MAKEFILE_LIST) `if [ -z "$(MAKEFILE_LIST)" ] ; then echo Makefile Makefile-targets.mk ; fi` ; do \
		grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $$fileX | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "%-25s%s\n", $$1, $$2}' ; \
	done
clean: ## clean build artifacts
	-rm -rf build/* build/.??*
test: tests/ts_main.scm ## run test [TOPTS=""]
#	export [DY]LD_LIBRARY_PATH=. # ([da|ba|z]sh Linux)
#	setenv [DY]LD_LIBRARY_PATH . # (tcsh FreeBSD)
	-cd build ; LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):lib $(SCHEME) -A$(sitelibdir) -A. -A.. -A../tests ../tests/ts_main.scm $(TOPTS)
uninstall install: ## [un]install artifacts
	-@if [ "uninstall" = "$@" ] ; then \
		ls $(parent) | xargs -I % rm -ir $(sitelibdir)/$(parent)/% ; \
	else \
		mkdir -p $(sitelibdir) ; (cp -fR $(parent) $(sitelibdir)/) ; \
		ls $(sitelibdir)/$(parent)/$(proj).adoc ; sleep 3 ; \
	fi
dist: | build/$(distdir) ## [FMTS="tar.gz,zip"] archive source code
	-@for fmt in `echo $(FMTS) | tr ',' ' '` ; do \
		case $$fmt in \
			7z) echo "### build/$(distdir).7z ###" ; \
				rm -f build/$(distdir).7z ; \
				(cd build ; 7za a -t7z -mx=9 $(distdir).7z $(distdir)) ;; \
			zip) echo "### build/$(distdir).zip ###" ; \
				rm -f build/$(distdir).zip ; \
				(cd build ; zip -9 -q -r $(distdir).zip $(distdir)) ;; \
			*) tarext=`echo $$fmt | grep -e '^tar$$' -e '^tar.xz$$' -e '^tar.zst$$' -e '^tar.bz2$$' || echo tar.gz` ; \
				echo "### build/$(distdir).$$tarext ###" ; \
				rm -f build/$(distdir).$$tarext ; \
				(cd build ; tar --posix -h -caf $(distdir).$$tarext $(distdir)) ;; \
		esac \
	done
	-@rm -r build/$(distdir)
doc: ## generate documentation
	-rm -fr build/share/doc/$(proj)/html ; mkdir -p build/html
#	-cp $(parent)/asciidoc_hdr.skel build/$(proj).txt
#	-grep -he '^[[:space:]]*;;;' $(parent)/util.scm $(parent)/_util.scm | \
#		sed G >> build/html/$(proj).txt
#	-asciidoc -n -a toc -a toclevels=2 build/html/$(proj).txt
	-asciidoctor -n -a toc -a toclevels=2 -D build/html $(parent)/$(proj).adoc
	-mv -f build/html build/share/doc/$(proj)/html

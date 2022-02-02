# FFI auxiliary makefile script

ffi_libdir = $(shell $(PKG_CONFIG) --variable=libdir intro_c-practice || echo .)
ffi_incdir = $(shell $(PKG_CONFIG) --variable=includedir intro_c-practice || echo .)
LD_LIBRARY_PATH := $(LD_LIBRARY_PATH):$(ffi_libdir)
export LD_LIBRARY_PATH

ifeq ($(shell sh -c 'uname -s 2>/dev/null || echo not'),Darwin)
sosuffix = dylib
else
sosuffix = so
LDFLAGS := $(LDFLAGS) -Wl,--enable-new-dtags
endif

ifdef DEBUG
CPPFLAGS := $(CPPFLAGS) -DDEBUG -UNDEBUG
CFLAGS := $(CFLAGS) -g3 -O0 --coverage
LDFLAGS := $(LDFLAGS) --coverage
else
CPPFLAGS := $(CPPFLAGS) -DNDEBUG -UDEBUG
CFLAGS := $(CFLAGS) -O3
endif

CC = clang		# clang | gcc
# using gauche-package
CPPFLAGS := $(CPPFLAGS) -Iinclude -I$(ffi_incdir) -I `gauche-config --incdirs | sed 's|:| -I|g'`
LDFLAGS := $(LDFLAGS) -Wl,-rpath,'$$ORIGIN/:$(ffi_libdir)' -Lbuild/lib -L `gauche-config --archdirs | sed 's|:| -L|g'`
CFLAGS := $(CFLAGS) -Wall -pedantic -std=c99 -m64
LDLIBS := $(LDLIBS) `gauche-config -l` -L$(ffi_libdir) -lintro_c-practice
# using sagittarius-package
#CPPFLAGS := $(CPPFLAGS) -I.. -Iinclude -I$(ffi_incdir) `sagittarius-config -I`
#LDFLAGS := $(LDFLAGS) -Wl,-rpath,'$$ORIGIN/:$(ffi_libdir)' -Lbuild/lib `sagittarius-config -L` `$(PKG_CONFIG) --libs bdw-gc`
#CFLAGS := $(CFLAGS) -Wall -pedantic -std=c99 -m64 `sagittarius-config --c-flags` `$(PKG_CONFIG) --cflags bdw-gc`
#LDLIBS := $(LDLIBS) `sagittarius-config -l` -L$(ffi_libdir) -lintro_c-practice

auxffi: ## compile FFI extension
#	using gauche-package
	-cd build ; gauche-package compile -c -n ../$(parent)/classic_stubslib.stub
#	-cd build ; $(CC) $(CPPFLAGS) $(LDFLAGS) $(CFLAGS) -fPIC -shared \
#		classic_stubslib.c ../$(parent)/classic_stubs.c \
#		-o classic_stubs.$(sosuffix) $(LDLIBS)
	-cd build ; gauche-package compile -v --cppflags="-I.. -I$(ffi_incdir)" \
		--ldflags="-L$(ffi_libdir) -L." --libs="-lintro_c-practice" \
		classic_stubs classic_stubslib.c ../$(parent)/classic_stubs.c
#	using sagittarius-package
#	-cd build ; sagittarius-package genstub .. . $(parent)/classic_stubslib.stub
#	-cd build ; $(CC) $(CPPFLAGS) $(LDFLAGS) $(CFLAGS) -shared \
#		$(parent)/classic_stubslib.c ../$(parent)/classic_stubs.c \
#		-o classic_stubs.$(sosuffix) $(LDLIBS)

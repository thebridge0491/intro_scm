.POSIX:
.SUFFIXES:
PREFIX ?= /usr/local

DEBUG = 0

# Single package Makefile script.
#.SUFFIXES: .s .o .a .h .c
help:

PKG_CONFIG = pkg-config --with-path=$(PREFIX)/lib/pkgconfig

# SCHEME=[gosh -r7 | sash -d -r7]
SCHEME = gosh -r7
sitelibdir = $(PREFIX)/share/scheme-r7rs/sitelib

pkg = practice
parent = intro_scm
proj = $(parent).$(pkg)
namespace_path = $(shell echo $(parent) | sed 'y|.|/|')
version = 0.1.0

build/.depend:
	-@cd build ; mkdir -p bin lib/pkgconfig share/doc/$(proj)
	-@find $(parent) tests -type d -exec mkdir -p build/{} \;
	-cp -fR resources build
	-@echo '' > build/.depend

.PHONY: all

#all: build/.depend auxffi ## compile
all: build/.depend ## compile

-include build/.depend Makefile-targets.mk
#-include Makefile-auxffi.mk

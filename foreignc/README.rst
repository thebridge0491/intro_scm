Intro_scm.Foreignc
===========================================
.. .rst to .html: rst2html5 foo.rst > foo.html
..                pandoc -s -f rst -t html5 -o foo.html foo.rst

FFI sub-package for Scheme Intro examples project.

Installation
------------
source code tarball download:
    
        # [aria2c --check-certificate=false | wget --no-check-certificate | curl -kOL]
        
        FETCHCMD='aria2c --check-certificate=false'
        
        $FETCHCMD https://bitbucket.org/thebridge0491/intro_scm/[get | archive]/master.zip

version control repository clone:
        
        git clone https://bitbucket.org/thebridge0491/intro_scm.git

build example with rake:
cd <path>/build ; [sh] ./configure.sh [--prefix=$PREFIX] [--help]

rake all [test]

[sudo] rake install

build example with make:
cd <path>/build ; [sh] ./configure.sh [--prefix=$PREFIX] [--help]

make all [check]

[sudo] make install

Usage
-----
        // SCHEME='gosh -r7'  // [gosh -r7 | sash -d -r7]
        
        [env LD_LIBRARY_PATH=$PREFIX/lib] $SCHEME -A. -Abuild -l intro_scm/classic.scm

        > (import (prefix (intro_scm classic) Classic:))

        > (Classic:fact_i 5)

Author/Copyright
----------------
Copyright (c) 2014 by thebridge0491 <thebridge0491-codelab@yahoo.com>

License
-------
Licensed under the Apache-2.0 License. See LICENSE for details.

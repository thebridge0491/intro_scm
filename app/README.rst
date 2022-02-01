Intro_scm.Intro
===========================================
.. .rst to .html: rst2html5 foo.rst > foo.html
..                pandoc -s -f rst -t html5 -o foo.html foo.rst

Main app sub-package for Scheme Intro examples project.

Installation
------------
source code tarball download:
    
        # [aria2c --check-certificate=false | wget --no-check-certificate | curl -kOL]
        
        FETCHCMD='aria2c --check-certificate=false'
        
        $FETCHCMD https://bitbucket.org/thebridge0491/intro_scm/[get | archive]/master.zip

version control repository clone:
        
        git clone https://bitbucket.org/thebridge0491/intro_scm.git

build example with rake:

        rake all [test]

        [sudo] rake install

build example with ninja:

        ninja [test]

        [sudo] ninja install

build example with make:

        make all [test]

        [sudo] make install

Usage
-----
        // SCHEME='gosh -r7'  // [gosh -r7 | sash -d -r7]
        
        [env RSRC_PATH=<path>/resources] $SCHEME -A. intro_scm/main.scm [-h]

or
        [env RSRC_PATH=<path>/resources] sh intro_scm/main.scm [-h]

or
        [env RSRC_PATH=<path>/resources] $SCHEME

        > (import (prefix (intro_scm main) Intro:))

        > (Intro:main '())

Author/Copyright
----------------
Copyright (c) 2014 by thebridge0491 <thebridge0491-codelab@yahoo.com>

License
-------
Licensed under the Apache-2.0 License. See LICENSE for details.

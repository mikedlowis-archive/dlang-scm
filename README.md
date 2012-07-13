DLang - Scheme
==============================================

    Version:    0.1
    Created By: Michael D. Lowis
    Email:      mike@mdlowis.com

About This Project
----------------------------------------------

This project is a Scheme port of the DLang compiler. It aims to parse and
compile an experimanetal functional language to an executable binary.

License
----------------------------------------------
Unless explicitly stated otherwise, all code and documentation contained within
this repository is released under the BSD 2-Clause license. The text for this
license can be found in the LICENSE.md file.

Requirements For Building
----------------------------------------------

* The Chicken Scheme Compiler
* The vector-lib egg for Chicken Scheme
* A C compiler (Tested with GCC)

Build Instructions
----------------------------------------------

You can build the project and run all unit tests with the following command:

    make

You can build just the release binaries with the following command:

    make release

You can execute just the unit tests with the following command:

    make test

Project Files and Directories
----------------------------------------------

    deps/          Libraries and any external build dependencies.
    docs/          Documentation for the project.
    inc/           Directory containing include files.
    res/           Miscellaneuos files needed for the project.
    source/        The source for the DLang parser.
    tests/         Unit test and mock files.
    tools/         Tools required by the build system.
    LICENSE.md     The software license notification.
    Makefile       File containing rules for building the project.
    project.vim    A VIM script with project specific configurations.
    README.md      You're reading this file right now!


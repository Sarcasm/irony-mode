============
irony-server
============

----------------------------------------
The back-end server for Emacs irony-mode
----------------------------------------

:Author: Nicholas D Steeves <nsteeves@gmail.com>
:Date: 2017-10-01
:Version: 1.1.0
:Manual section: 1


Synopsis
========

irony-server [*options*] [*command*] [<*args*>]

Description
===========

``irony-server`` provides the libclang interface to irony-mode. It uses a
simple protocol based on S-expression. This server is written in C++ and
requires the following packages to be installed on your system: CMake >=
2.8.3, and libclang

Options
=======

-v, --version

 Show version and exit.

-h, --help

 Show more detailed command help.

-i, --interactive

-d, --debug

--log-file PATH

Commands
========

**candidates**

 PREFIX STYLE - print completion candidates (require previous
 complete). STYLE is "exact", "case-insensitive" or "smart-case".
        
**complete**

 FILE LINE COL [-- [COMPILE_OPTIONS...]] - perform code completion at a given
 location.
        
**completion-diagnostics**

 Print the diagnostics generated during complete.
        
**diagnostics**

 Print the diagnostics of the last parse.
        
**exit**

 Exit interactive mode, print nothing.
        
**get-compile-options**

 BUILD_DIR FILE - get compile options for FILE from JSON database in
 PROJECT_ROOT.
        
**get-type**

 LINE COL - get type of symbol at a given location.
        
**help**

 Show this message.
        
**parse**

 FILE [-- [COMPILE_OPTIONS...]] - parse the given file.
        
**reset-unsaved**

 FILE - reset FILE, its content is up to date.
        
**set-debug**

 [on|off] - enable or disable verbose logging.
        
**set-unsaved**

 FILE UNSAVED-CONTENT-FILE - tell irony-server that UNSAVED-CONTENT-FILE
 contains the effective content of FILE.

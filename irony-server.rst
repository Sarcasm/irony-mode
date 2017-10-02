============
irony-server
============

----------------------------------------
The back-end server for Emacs irony-mode
----------------------------------------

:Author: Nicholas D Steeves <nsteeves@gmail.com>
:Date: 2017-10-01
:Version: 1.2.0
:Manual section: 1


Synopsis
========

irony-server [*options*] [*command*] [<*args*>]

Description
===========

``irony-server`` provides the libclang interface to irony-mode. It uses a
simple protocol based on S-expression. This server is written in C++ and
requires the following packages to be installed on your system: CMake >=
2.8.3, and libclang.

Options
=======

-v, --version

 Show version and exit.

-h, --help

 Show more detailed command help.

-i, --interactive

 Run an interactive command loop. It accepts only the ``Commands``
 listed below. This is primarily a machine interface that the user
 interacts with via irony.el.

-d, --debug

 Write debug info to log file.

--log-file PATH

 Write logs to path instead of standard error.

Commands
========

**candidates** PREFIX STYLE

 Print completion candidates (require previous complete). STYLE is
 "exact", "case-insensitive" or "smart-case".
        
**complete** FILE LINE COL [-- [COMPILE_OPTIONS...]]

 Perform code completion at a given location.
        
**completion-diagnostics**

 Print the diagnostics generated during complete.
        
**diagnostics**

 Print the diagnostics of the last parse.
        
**exit**

 Exit interactive mode, print nothing.
        
**get-compile-options** BUILD_DIR FILE

 Get compile options for FILE from JSON database in PROJECT_ROOT.
        
**get-type** LINE COL

 Get type of symbol at a given location.
        
**help**

 Show this message.
        
**parse** FILE [-- [COMPILE_OPTIONS...]]

 Parse the given file.
        
**reset-unsaved** FILE

 Reset FILE, its content is up to date.
        
**set-debug** [on|off]

 Enable or disable verbose logging.
        
**set-unsaved** FILE UNSAVED-CONTENT-FILE

 Tell irony-server that UNSAVED-CONTENT-FILE contains the effective
 content of FILE.

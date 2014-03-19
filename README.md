# Irony-Mode
## A C/C++ minor mode powered by [libclang][libclang-ref]

[![Build Status](https://api.travis-ci.org/Sarcasm/irony-mode.png?branch=develop)](https://travis-ci.org/Sarcasm/irony-mode)

*Note:* This is a work in progress:

* documentation might not be up-to-date
* changes can break compability with earlier versions
* use at your own risk!
* open issues, fork-it and create pull-requests!


## Features

* Semantic completion with [ac-irony][ac-irony-ref]


## Dependencies

This package depends on:

| Package               | Version  | Status       | Comment                                               |
| --------------------- | -------- | ------------ | ----------------------------------------------------- |
| [YASnippet][yas-ref]  | All      | recommended  | Used only when available to provide dynamic snippets  |
| [JSON][json-el-ref]   | All      | required     | Built-in since Emacs 23                               |


## Installation

The recommended way to install `irony-mode` and its dependencies is to use a
package manager.

* Using [el-get](https://github.com/dimitri/el-get)

        M-x el-get-install RET irony-mode RET

* Using [MELPA](http://melpa.milkbox.net/)

        M-x package-install RET irony-mode RET


## Configuration

~~~el
(require 'irony-mode)

(defun my-irony-mode-enable ()
  ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: php-mode
  (when (member major-mode irony-known-modes)
    (irony-mode 1)))

(add-hook 'c++-mode-hook 'my-irony-mode-enable)
(add-hook 'c-mode-hook 'my-irony-mode-enable)
(add-hook 'objc-mode-hook 'my-irony-mode-enable)
~~~


## Usage

If you want the completion to work on a project you will probably need give some
information about the flags necessary to compile a file. The best way to achieve
this is to use a [Compilation Database](#compilation-database). Otherwise you
can always use the customizable variables: `M-x customize-group RET irony RET`.

**Note:** If you want to force the reload of the flags on the server, you can
use the command `M-x irony-reload-flags`. This shouldn't be necessary if you use
the compilation database plugin.


## Compilation Database

In order to work correctly, `irony-mode` needs to know the compile flags. This
plugin allow aims to provide *as automatic as possible* compile flags discovery,
with minimum user input.

Just hit `C-c C-b` (aka `irony-cdb-menu`) to display the build configuration
menu.

The menu should be self explanatory, if it's not the case please open an issue.

This plugin works great with the following tools:

- [CMake][cmake-ref] >= 2.8.5

- [Ninja][ninja-ref] >= 1.2 - Use `ninja -t compdb` to generate a compilation
  database for your project.

- [Bear][bear-ref] - Bear is a tool that can generate a
  `compile_commands.json` file by "monitoring" the build of a project.
  The typical usage for a `make` based project will be `bear -- make -B`.

- [.clang_complete][clang_complete-doc-ref] - A file at the root of your project
  containing the compilation flags, one per line. This is compatible with the
  with plugin [Rip-Rip/clang_complete][clang_complete-vim-ref]. If you want to
  generate the `.clang_complete` automatically, take a look there:
  [cc_args.py documentation][cc_args-py-doc-ref].


The [JSON Compilation Database Format Specification][clang-compile-db-ref] page
may reference some new tools in the future that supports the
`compile_commands.json` format. `irony-mode` support that file format and
hopefully it should work *out-of-the-box* for any of such tools.

![Compilation DB demo](screenshots/cdb.gif)


## FAQ

__It's slow, why?__

A bug in old version of Clang (at least '3.1-8') caused the completion to fail
on the standard library types. To eliminate this bug an optimisation has been
disabled in the parsing of a translation unit. This result in a slower parsing.

This only affect old versions of Clang (< 3.2), it is suggested to update your
libclang installation if you want to take advantage of the optimizations.

__How do I use a custom version of libclang?__

On a system with multiple versions of libclang installed, it is possible to
*hint* CMake in order to pick-up the preferred one. Two variables can be
specified when invoking CMake:

1. `LIBCLANG_INCLUDE_PATH=/path/to/libclang/include-dir`: The path to the
   include directory that contains the header file *clang-c/Index.h*.

2. `LIBCLANG_LIBRARY_PATH=/path/to/libclang/libraries`: The path to the
   directory that contains the libclang library, e.g: *libclang.so* on Linux.

Example of a CMake invocation using a custom libclang installation:

    mkdir build
    cd build
    cmake -DLIBCLANG_INCLUDE_PATH=~/llvm-3.4/include/ \
          -DLIBCLANG_LIBRARY_PATH=~/llvm-3.4/lib/ ..


__CMake doesn't find libclang...__

When libclang is installed in a non-standard path CMake may produce the
following error:

    CMake Error at /usr/share/cmake-2.8/Modules/FindPackageHandleStandardArgs.cmake:108 (message):
    Could NOT find LibClang (missing: LibClang_LIBRARY LibClang_INCLUDE_DIR)
    Call Stack (most recent call first):
    /usr/share/cmake-2.8/Modules/FindPackageHandleStandardArgs.cmake:315 (FPHSAFAILURE_MESSAGE)
    cmake/FindLibClang.cmake:35 (find_package_handle_standard_args)
    server/CMakeLists.txt:4 (find_package)

This can be fixed by using the same method as described above, using a custom
version of libclang.

Please report an issue if the include and library paths are the standard
libclang installation paths on your system. Other users may benefit from these
paths being handled *natively* by irony-mode.


__I got an error due to 'stdarg.h'...__

If by looking into the log file (`/tmp/irony.$$PID.log` on Linux) you
can have the following error:

    'stdarg.h' file not found

Assuming (otherwise it might be necessary to adjust the code):

* Clang version is 3.2
* Clang is installed in `/usr/`

You can use the following configuration:

```el
;; The Clang installation missed the system include directory
;; "/usr/lib/clang/3.2/include/"
(when (file-exists-p "/usr/lib/clang/3.2/include/")
  (setq irony-libclang-additional-flags
        '("-isystem" "/usr/lib/clang/3.2/include/")))
```

This is issue is a known problem with libclang:

* http://lists.cs.uiuc.edu/pipermail/cfe-dev/2012-July/022893.html

> Make sure that Clang is using its own <stddef.h>. It will be in a
> directory ending in `clang/3.2/include/` where 3.2 is the version of
> clang that you are using. You may need to explicitly add it to your
> header search. Usually clang finds this directory relative to the
> executable with CompilerInvocation::GetResourcesPath(Argv0,
> MainAddr), but using just the libraries, it can't automatically find
> it.


__libclang.so: cannot open shared object file...__

Compiling `irony-server` succeed but you have the following message when you try
to run the `irony-server` executable:

    'irony-server: error while loading shared libraries: libclang.so: cannot open shared object file: No such file or directory

Maybe it's due to a non-standard location for your installation of `libclang`. A
path such as `/usr/local/lib` might not be in the path list of the dynamic
loader (see ld.so.conf).

To solve this issue you can try to build `irony-server` with the following
command:

    cmake -DUSE_RPATH=ON ..


[libclang-ref]: http://clang.llvm.org/doxygen/group__CINDEX.html "libclang: C Interface to Clang"
[ac-irony-ref]: https://github.com/Sarcasm/ac-irony "AC Irony"
[yas-ref]: https://github.com/capitaomorte/yasnippet "YASnippet"
[json-el-ref]: http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/lisp/json.el?root=emacs "json.el"
[clang-compile-db-ref]: http://clang.llvm.org/docs/JSONCompilationDatabase.html "Clang: JSONCompilationDatabase"
[cmake-ref]: http://www.cmake.org "CMake"
[ninja-ref]: http://martine.github.io/ninja/ "Ninja"
[bear-ref]: https://github.com/rizsotto/Bear "Bear"
[clang_complete-vim-ref]: https://github.com/Rip-Rip/clang_complete "clang_complete Vim plugin"
[clang_complete-doc-ref]: https://github.com/Rip-Rip/clang_complete/blob/c8673142759b87316265eb0edd1f620196ec1fba/doc/clang_complete.txt#L55 ".clang_complete"
[cc_args-py-doc-ref]: https://github.com/Rip-Rip/clang_complete/blob/c8673142759b87316265eb0edd1f620196ec1fba/doc/clang_complete.txt#L270 "cc_args.py documentation"

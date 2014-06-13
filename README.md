# Irony-Mode
## A C/C++ minor mode powered by [libclang][libclang-ref]

*Note:* This is a work in progress:

* **Right now the package is not yet installable using a package manager, even if
  the doc says so.** This will come soon in the meantime, be patient.
* Documentation might not be up-to-date.
* Use at your own risk!
* Open issues, fork-it and create pull-requests!


## Features

* Code completion:
  * Using `completion-at-point-functions`
  * Using [company-irony][company-irony-ref]
  * Using [ac-irony][ac-irony-ref]


## Dependencies

### Elisp dependencies

These dependencies will be installed automatically when using the
[standard installation][#installation] described below.

| Package               | Comment                                                                                  |
| --------------------- | ---------------------------------------------------------------------------------------- |
| [cl-lib][cl-lib-ref]  | Built-in since Emacs 24.3                                                                |
| [YASnippet][yas-ref]  | Optional. Used only when available to provide snippets expansion of completion arguments |


### Irony-Server prerequisites

`irony-server` is an executable that provides an interface to
[libclang][libclang-ref] to `irony-mode`. This server, written in C++, requires
the following system packages to be installed on your system:

* [CMake][cmake-ref] >= 2.8.3
* [libclang][libclang-ref]


## Installation

The recommended way to install `irony-mode` and its dependencies is to use a
package manager.

* Using [MELPA](http://melpa.milkbox.net/)

        M-x package-install RET irony-mode RET

* Using [el-get](https://github.com/dimitri/el-get)

        M-x el-get-install RET irony-mode RET


## Configuration

~~~el
(defun my-irony-mode-enable ()
  ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: php-mode
  (when (memq major-mode irony-supported-major-modes)
    (irony-mode 1)))

(add-hook 'c++-mode-hook 'my-irony-mode-enable)
(add-hook 'c-mode-hook 'my-irony-mode-enable)
(add-hook 'objc-mode-hook 'my-irony-mode-enable)
~~~


## Usage

On the first run, `irony-mode` will ask you to build and install `irony-server`
using: `M-x irony-install-server RET`.

In order to provide context sensitive and accurate information, `irony-mode`
needs to know about the compiler flags used to parse the current buffer. The
best way to achieve this is to use a
[Compilation Database](#compilation-database).

To tune `irony-mode`, use `customize`:


    M-x customize-group RET irony RET


## Compilation Database

In order to work correctly, `irony-mode` needs to know the compile flags. This
plugin allow aims to provide *as automatic as possible* compile flags discovery,
with minimum user input.

Just hit `M-x irony-cdb-menu RET`) to display the build configuration menu.

The menu should be self explanatory, if it's not the case please open an issue.

This plugin works great with the following tools:

- [.clang_complete][clang_complete-doc-ref] - A file at the root of your project
  containing the compilation flags, one per line. This is compatible with the
  with plugin [Rip-Rip/clang_complete][clang_complete-vim-ref]. If you want to
  generate the `.clang_complete` automatically, take a look there:
  [cc_args.py documentation][cc_args-py-doc-ref].

<!---

- [CMake][cmake-ref] >= 2.8.5

- [Ninja][ninja-ref] >= 1.2 - Use `ninja -t compdb` to generate a compilation
  database for your project.

- [Bear][bear-ref] - Bear is a tool that can generate a
  `compile_commands.json` file by "monitoring" the build of a project.
  The typical usage for a `make` based project will be `bear -- make -B`.

The [JSON Compilation Database Format Specification][clang-compile-db-ref] page
may reference some new tools in the future that supports the
`compile_commands.json` format. `irony-mode` support that file format and
hopefully it should work *out-of-the-box* for any of such tools.
-->

![Compilation DB demo](screenshots/cdb.gif)


## FAQ

__It's slow, why?__

A bug in old version of Clang (at least '3.1-8') caused the completion to fail
on the standard library types. To eliminate this bug an optimisation has been
disabled in the parsing of a translation unit. This result in a slower parsing.

This only affect old versions of Clang (< 3.2), it is suggested to update your
libclang installation if you want to take advantage of the optimizations.

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
[company-irony-ref]: https://github.com/Sarcasm/company-irony "Company Irony"
[ac-irony-ref]: https://github.com/Sarcasm/ac-irony "AC Irony"
[yas-ref]: https://github.com/capitaomorte/yasnippet "YASnippet"
[cl-lib-ref]: http://elpa.gnu.org/packages/cl-lib.html "cl-lib"
[clang-compile-db-ref]: http://clang.llvm.org/docs/JSONCompilationDatabase.html "Clang: JSONCompilationDatabase"
[cmake-ref]: http://www.cmake.org "CMake"
[ninja-ref]: http://martine.github.io/ninja/ "Ninja"
[bear-ref]: https://github.com/rizsotto/Bear "Bear"
[clang_complete-vim-ref]: https://github.com/Rip-Rip/clang_complete "clang_complete Vim plugin"
[clang_complete-doc-ref]: https://github.com/Rip-Rip/clang_complete/blob/c8673142759b87316265eb0edd1f620196ec1fba/doc/clang_complete.txt#L55 ".clang_complete"
[cc_args-py-doc-ref]: https://github.com/Rip-Rip/clang_complete/blob/c8673142759b87316265eb0edd1f620196ec1fba/doc/clang_complete.txt#L270 "cc_args.py documentation"

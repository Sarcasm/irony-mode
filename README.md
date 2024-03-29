# Irony-Mode

## A C/C++ minor mode powered by [libclang][libclang-ref]

`irony-mode` is an Emacs minor-mode that aims at improving the editing
experience for the C, C++ and Objective-C languages. It works by using a
combination of an Emacs package and a C++ program (`irony-server`) exposing
[libclang][libclang-ref].

**Features:**

* Code completion:
  * With Emacs' built-in `completion-at-point-functions`
  * With [company-mode][company-ref]'s backend: [company-irony][company-irony-ref]

* Syntax checking:
  * With [flycheck][flycheck-ref]'s checker: [flycheck-irony][flycheck-irony-ref]

* `eldoc` integration: [irony-eldoc][irony-eldoc-ref]

* [counsel][counsel-ref] integration: https://oremacs.com/2017/03/28/emacs-cpp-ide/

## Dependencies

### Elisp dependencies

These dependencies will be installed automatically when using the
[standard installation](#installation) procedure described below.

| Package              | Comment                                                                          |
| -------------------- | -------------------------------------------------------------------------------- |
| [cl-lib][cl-lib-ref] | Built-in since Emacs 24.3                                                        |
| [json][json-el-ref]  | Built-in since Emacs 23.1                                                        |
| [YASnippet][yas-ref] | Optional. May be used to provide post-completion expansion of function arguments |


### Irony-Server prerequisites

`irony-server` provides the [libclang][libclang-ref] interface to `irony-mode`.
It uses a simple protocol based on S-expression. This server, written in C++ and
requires the following packages to be installed on your system:

* [CMake][cmake-ref] >= 2.8.3
* [libclang][libclang-ref]


## Installation

The recommended way to install `irony-mode` and its dependencies is to use a
package manager.

* Using [MELPA](https://melpa.org/#/)

        M-x package-install RET irony RET

* Using `apt` on Debian ≥10 and derivatives

        sudo apt install elpa-irony

Exactly one package manager should manage `irony-mode`. If using apt,
but the MELPA package is desired, uninstall the version managed by
apt; Likewise, installing from both MELPA and `straight.el` may result
in a state that requires a manual workaround.

#### To install the latest Debian-supported `irony-mode ` and Clang
The [backports](https://backports.debian.org/) mechanism is the
recommended and officially supported method of accessing newer
versions than `Debian stable` provides. For example on Debian 10 (buster):

        sudo apt install -t buster-backports elpa-irony

If one requires a newer version of Clang/LLVM than the one provided in
backports, the following repository is available: [LLVM Debian/Ubuntu
nightly packages.](https://apt.llvm.org) This unofficial repository is
maintained by Sylvestre Ledru, who is responsible for the official
Debian package.  His repository also supports Ubuntu and derivatives.
When using this unofficial repository, it is recommended to use the
_MELPA package_ of irony-mode and to uninstall `elpa-irony`. Finally,
when switching from apt to MELPA, the following action is required:
`M-x irony-install-server RET`.


## Configuration

~~~el
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
~~~


## Windows considerations

`irony-mode` should work fine on Windows but there are some things to take care
of first.

* `libclang.dll` is expected to be available in the `PATH` or in Emacs'
  `exec-path`.

* **Emacs >= 24.4 is required.** A bug in previous versions makes irony
  unuseable (Emacs bug #18420).

* `w32-pipe-read-delay` default value of `50` should be changed.
  This should not cause any issue on today's version of Windows.
  The default value of `50` may be lowered in mainline Emacs in future versions,
  until then, I suggest to set it to `0`.

* `w32-pipe-buffer-size`, introduced by Emacs 25,
  can be set to a larger value than the default to improve
  `irony-server` communication performances
  (c.f. https://github.com/Sarcasm/irony-mode/issues/321).
  The variable to customize is `irony-server-w32-pipe-buffer-size`.


Windows configuration tweaks to add to your Emacs configuration:

```el
;; Windows performance tweaks
;;
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
```


## Usage

On the first run, `irony-mode` will ask you to build and install `irony-server`.
To do so, type `M-x irony-install-server RET`.

To tune `irony-mode`, use `customize`:


    M-x customize-group RET irony RET

In order to provide context sensitive and accurate information, `irony-mode`
needs to know about the compiler flags used to parse the current buffer. The
best way to achieve this is to use a
[Compilation Database](#compilation-database).


## Compilation Database

In order to work correctly, `irony-mode` needs to know the compile flags.
`irony-cdb` aims to provide *as automatic as possible* compile flags discovery,
with minimal user input.

Please refer to `irony-cdb-autosetup-compile-options` and
`irony-cdb-compilation-databases`.

Right now `irony-cdb` supports the following compilation databases:

* [JSON Compilation Database][clang-compile-db-ref] - A JSON formatted file
  generated by various build tools. The file is named `compile_commands.json`,
  it lists the compile options associated to each file in the project.

  * [CMake >= 2.8.5][cmake-ref] will generate a compilation database in the
    build directory when issuing the following command
    `cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON <...>`.

  * [ninja >= 1.2][ninja-ref] will generate a JSON Compilation Database when
    using the `compdb` tool.

  * [Bear][bear-ref] generates a JSON Compilation Database file by "monitoring"
    the build of a project. The typical usage for a `make`-based project will be
    `bear -- make -B`.

* [.clang_complete][clang_complete-doc-ref] - A file at the root of your project
  containing the compilation flags, one per line. This is compatible with the
  with plugin [Rip-Rip/clang_complete][clang_complete-vim-ref]. If you want to
  generate the `.clang_complete` automatically, take a look at the
  [cc_args.py documentation][cc_args-py-doc-ref].

More detailed information on compilation database is available here:

* https://sarcasm.github.io/notes/dev/compilation-database.html

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

When `libclang` is installed in a non-standard location (one that is missing
from the path list of the dynamic loader, see *ld.so.conf*) you can tell CMake
to use the [rpath][rpath-ref] when installing the target `irony-server`. To
enable rpath in CMake use the following command:

    cmake -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON <args...>

If you're running OS X, you can also use `install_name_tool` to explicitly
tell `irony-server` where an appropriate version of `libclang.dylib` lives.
For example, Homebrew (with `brew install llvm --with-clang`) will install
a `libclang.dylib` library at `/usr/local/opt/llvm/lib/libclang.dylib`;
you can instruct `irony-server` to use this with something like:

    install_name_tool -change @rpath/libclang.dylib /usr/local/opt/llvm/lib/libclang.dylib irony-server

[ac-irony-ref]: https://github.com/Sarcasm/ac-irony "AC Irony"
[ac-ref]: https://github.com/auto-complete/auto-complete "Auto Complete"
[bear-ref]: https://github.com/rizsotto/Bear "Bear"
[cc_args-py-doc-ref]: https://github.com/Rip-Rip/clang_complete/blob/c8673142759b87316265eb0edd1f620196ec1fba/doc/clang_complete.txt#L270 "cc_args.py documentation"
[cl-lib-ref]: http://elpa.gnu.org/packages/cl-lib.html "cl-lib"
[clang-compile-db-ref]: http://clang.llvm.org/docs/JSONCompilationDatabase.html "Clang: JSONCompilationDatabase"
[clang_complete-doc-ref]: https://github.com/Rip-Rip/clang_complete/blob/c8673142759b87316265eb0edd1f620196ec1fba/doc/clang_complete.txt#L55 ".clang_complete"
[clang_complete-vim-ref]: https://github.com/Rip-Rip/clang_complete "clang_complete Vim plugin"
[cmake-ref]: http://www.cmake.org "CMake"
[counsel-ref]: https://github.com/abo-abo/swiper#counsel "Counsel on Github"
[company-irony-ref]: https://github.com/Sarcasm/company-irony "Company Irony"
[company-ref]: https://github.com/company-mode/company-mode "Company-Mode"
[flycheck-irony-ref]: https://github.com/Sarcasm/flycheck-irony "Flycheck Irony"
[flycheck-ref]: http://www.flycheck.org "Flycheck -- Modern Emacs syntax checking"
[irony-eldoc-ref]: https://github.com/ikirill/irony-eldoc "irony-eldoc -- irony-mode support for eldoc-mode"
[json-el-ref]: http://edward.oconnor.cx/2006/03/json.el "Introducing json.el"
[libclang-ref]: http://clang.llvm.org/doxygen/group__CINDEX.html "libclang: C Interface to Clang"
[ninja-ref]: https://ninja-build.org "Ninja"
[rpath-ref]: http://en.wikipedia.org/wiki/Rpath "rpath Wikipedia article"
[yas-ref]: https://github.com/capitaomorte/yasnippet "YASnippet"

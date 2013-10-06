# Status

[![Build Status](https://api.travis-ci.org/Sarcasm/irony-mode.png?branch=develop)](https://travis-ci.org/Sarcasm/irony-mode)

Note: This is a work in progress:

* documentation might not be up-to-date
* changes can break compability with earlier versions
* use at your own risk!
* open issues, fork-it and create pull-request!


# Screenshots

![Optional Parameters](https://raw.github.com/Sarcasm/irony-mode/develop/screenshots/optional-parameters.png)

![Boost](https://raw.github.com/Sarcasm/irony-mode/develop/screenshots/boost-example.png)


# Download and compilation

Download with `git >= 1.6.5` and later

    git clone --recursive git://github.com/Sarcasm/irony-mode.git

Download with `git < 1.6.5`

    git clone git://github.com/Sarcasm/irony-mode.git
    git submodule update --init

Finally:

    cd irony-mode
    mkdir -p build
    cd build
    cmake .. && make -j 4
    make install

Note: It is recommended to proceed to the installation step. This is
not a "system-wide" installation, it shouldn't require any privilege.
The installation step will help `irony.el` to find the binary without
additional configuration.


# Emacs configuration

Recommended packages and versions:

| Package                           | Version   | Status        | Comment                                                                                                 |
| --------------------------------- | --------- | ------------- | ------------------------------------------------------------------------------------------------------- |
| [auto-complete][ac-ref]           | 1.4       | recommended   | you can check the version in the auto-complete.el header                                                |
| [auto-complete fork][ac-fork-ref] | 1.4       | as-you-wish   | conflicts w/ auto-complete AND popup, able to display detailed completions such as overloaded functions |
| [YASnippet][yasnippet-ref]        | All       | recommended   | `yas--version` or `yas/version`                                                                         |

[ac-ref]:        https://github.com/auto-complete/auto-complete "Auto Complete"
[ac-fork-ref]:   https://github.com/Sarcasm/auto-complete       "Auto Complete Sarcasm fork"
[yasnippet-ref]: https://github.com/capitaomorte/yasnippet      "YASnippet"

[el-get](https://github.com/dimitri/el-get) help a lot for package management in
Emacs. You can take a look at
[my configuration](https://github.com/Sarcasm/.emacs.d/blob/master/sarcasm-elisp/sarcasm-el-get.el).

Add irony-mode to the load `load-path`:

```el
(add-to-list 'load-path (expand-file-name "~/IRONY/MODE/PATH/elisp/"))
```

And fill in a basic configuration as shown below in
[Auto Complete](#auto-complete).

If you want the completion to work on a project you will probably need give some
information about the flags necessary to compile a file. The best way to achieve
that is probably to use the [Compilation Database](#compilation-database)
plugin.

**Note:** If you want to force the reload of the flags on the server, you can
use the command `M-x irony-reload-flags`. This shouldn't be necessary if you use
the compilation database plugin.


# Plugins

To enable one plugin call `(irony-enable 'plugin-name)`, to enable
more than one plugin at once call the same function with a list
`(irony-enable '(plugin-1 plugin-2))`.


## Auto Complete

Code completion with auto-complete.

Requires:
* [auto-complete][ac-ref]
* [yasnippet][yasnippet-ref] (optional)

Configuration, please use the init order: `yas`, then `ac`, then `irony` unless
you have a reason not to:

```el
(require 'auto-complete)
(require 'yasnippet)
(require 'irony) ;Note: hit `C-c C-b' to open build menu

;; the ac plugin will be activated in each buffer using irony-mode
(irony-enable 'ac)             ; hit C-RET to trigger completion

(defun my-c++-hooks ()
  "Enable the hooks in the preferred order: 'yas -> auto-complete -> irony'."
  ;; if yas is not set before (auto-complete-mode 1), overlays may persist after
  ;; an expansion.
  (yas/minor-mode-on)
  (auto-complete-mode 1)
  (irony-mode 1))

(add-hook 'c++-mode-hook 'my-c++-hooks)
(add-hook 'c-mode-hook 'my-c++-hooks)
```

**Note:** If my fork of `auto-complete` is used you should be careful
to not have another installation of `auto-complete` or `popup` since
both of them are present in my fork.

You can eval these lines in the `*scratch*` buffer:

```el
(let ((requirements (or (require 'auto-complete nil t)
                        (require 'popup nil t))))
  (message "You should remove both auto-complete and popup before using my fork of auto-complete"))
```

Hit `C-x C-e` at the end of the expression to evaluate.


## Compilation Database

In order to work correctly, `irony-mode` needs to know the compile
flags. This plugin allow aims to provide *as automatic as possible*
compile flags discovery, with minimum user input.

It works great with the following tools:

- [CMake][cmake-ref] >= 2.8.5

- [Ninja][ninja-ref] >= 1.2 - Use `ninja -t compdb` to generate a compilation
  database for your project.

- [Bear][bear-ref] - Bear is a tool that can generate a
  `compile_commands.json` file by "monitoring" the build of a project.
  The typical usage for a `make` based project will be `bear -- make
  -B`.

- [.clang_complete][clang_complete-doc-ref] - A file at the root of your project
  containing the compilation flags, one per line. This is compatible with the
  with plugin [Rip-Rip/clang_complete][clang_complete-vim-ref]. If you want to
  generate the `.clang_complete` automatically, take a look there:
  [cc_args.py documentation][cc_args-py-doc-ref].


The
[JSON Compilation Database Format Specification][clang-compile-db-ref]
page might reference some new tools in the future supporting the
`compile_commands.json` format (such as `cmake` and `Bear` described
above). `irony-mode` support that file format and hopefully it should
work *out-of-the-box* for such tool.

![Compilation DB demo](https://raw.github.com/Sarcasm/irony-mode/develop/screenshots/cdb.gif)

Usage:

This is not really a plugin but something built-in irony-mode core, as
such it doesn't require any activation code. Just hit `C-c C-b` to
display the build configuration menu.

The menu should be self explanatory, if it's not the case open an
issue please.


[cmake-ref]: http://www.cmake.org "CMake"
[ninja-ref]: http://martine.github.io/ninja/ "Ninja"
[bear-ref]: https://github.com/rizsotto/Bear "Bear"
[clang-compile-db-ref]: http://clang.llvm.org/docs/JSONCompilationDatabase.html "Clang: JSONCompilationDatabase"
[clang_complete-doc-ref]: https://github.com/Rip-Rip/clang_complete/blob/2831a5040ee328103b941fcdbc3c8d6ef5593b59/doc/clang_complete.txt#L45 ".clang_complete"
[clang_complete-vim-ref]: https://github.com/Rip-Rip/clang_complete "clang_complete Vim plugin"
[cc_args-py-doc-ref]: https://github.com/Rip-Rip/clang_complete/blob/2831a5040ee328103b941fcdbc3c8d6ef5593b59/doc/clang_complete.txt#L271 "cc_args.py documentation"

# FAQ


## It's slow, why?

A bug in old version of Clang (at least '3.1-8') caused the completion
to fail on the standard library types. To eliminate this bug an
optimisation has been disabled in the parsing of a translation unit.
This result in a slower parsing.

This only affect old versions of Clang (< 3.2), it is suggested to
update your libclang installation if you want to take advantage of the
optimisations.


## I got an error due to 'stdarg.h', how to solve this?

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

This is issue is a known problem:

* http://lists.cs.uiuc.edu/pipermail/cfe-dev/2012-July/022893.html

> Make sure that Clang is using its own <stddef.h>. It will be in a
> directory ending in `clang/3.2/include/` where 3.2 is the version of
> clang that you are using. You may need to explicitly add it to your
> header search. Usually clang finds this directory relative to the
> executable with CompilerInvocation::GetResourcesPath(Argv0,
> MainAddr), but using just the libraries, it can't automatically find
> it.


## libclang.so: cannot open shared object file...

Compiling `irony-server` succeed but you have the following message
when you try to run the `irony-server` executable:

    'irony-server: error while loading shared libraries: libclang.so: cannot open shared object file: No such file or directory

Maybe it's due to a non-standard location for your installation of
`libclang`. A path such as `/usr/local/lib` might not be in the path
list of the dynamic loader (see ld.so.conf).

To solve this issue it is possible to build `irony-server` with the
following command:

    cmake -DUSE_RPATH=ON ..


## auto-complete acts strangely, it tries to complete inside string literals

In order to enable header completion, such as:

```cpp
#include "heade[COMP]
```

The `ac` plugin allows `auto-complete` to complete inside string
literals in `irony-ac-enable`.

```el
(setq ac-disable-faces (delq 'font-lock-string-face ac-disable-faces))
```

Please create an issue if you find this not unacceptable.

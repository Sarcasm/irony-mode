# Status

[![Build Status](https://api.travis-ci.org/Sarcasm/irony-mode.png?branch=develop)](https://travis-ci.org/Sarcasm/irony-mode)

Note: This is a work in progress:

* documentation might not be up-to-date
* changes can break compability with earlier versions
* use at your own risk!
* open issues, fork-it and create pull-request!

# Download and compilation

Download with `git` >= 1.6.5 and later

    git clone --recursive git://github.com/Sarcasm/irony-mode.git

Download with `Git` < 1.6.5

    git clone git://github.com/Sarcasm/irony-mode.git
    git submodule update --init

For Mac OS X a patch is necessary to fix the SimpleJSON compilation,
please read the comment [here](https://github.com/MJPA/SimpleJSON/commit/cf8aa3087747f76745fc30f38e6aff4af74e9cef#commitcomment-937703)

Finally:

    cd irony-mode
    mkdir -p build && cd build
    cmake .. && make -j 4
    make install

# Emacs configuration

Recommended packages and versions:

| Package                           | Version      | Status      | Comment                                             |
| --------------------------------- | ------------ | ----------- | --------------------------------------------------- |
| [auto-complete][ac-ref]           | 1.4          | recommended | version in auto-complete.el header                  |
| [YASnippet][yasnippet-ref]        | ALL          | recommended | `yas--version` or `yas/version`                     |
| [auto-complete fork][ac-fork-ref] | 1.4          | legacy      | detailed completion (e.g show overloaded functions) |
| [eproject][eproject-ref]          | ???          | legacy      |                                                     |

[ac-ref]:        https://github.com/auto-complete/auto-complete "Auto Complete"
[ac-fork-ref]:   https://github.com/Sarcasm/auto-complete       "Auto Complete Sarcasm fork"
[yasnippet-ref]: https://github.com/capitaomorte/yasnippet      "YASnippet"
[eproject-ref]:  https://github.com/jrockway/eproject           "Eproject"

[el-get](https://github.com/dimitri/el-get) help a lot for package
management in Emacs. You can take a look at
[my configuration](https://github.com/Sarcasm/.emacs.d/blob/master/sarcasm-elisp/sarcasm-el-get.el)).

Copy and paste in the \*scratch\* buffer:

~~~~~ el
(add-to-list 'load-path (expand-file-name "~/IRONY/MODE/PATH/elisp/"))
(add-to-list 'load-path (expand-file-name "~/IRONY/MODE/PATH/elisp/plugins/"))
(require 'irony)
(irony-enable '(eproject ac ac-header-comp))
(add-hook 'c++-mode-hook 'irony-mode)
~~~~~

Hit `C-x h M-x eval-buffer RET`, open a C++ file and try the auto
completion feature.

If you want the completion to work on a project you will probably need
give some informations:

There are 3 methods:

1. Setting `irony-header-directories`,
   `irony-header-directories-root`, `irony-config-commands`,
   `irony-extra-flags`. This can be done "natively" in Emacs by
   creating a `.dir-locals.el` file in your project containing
   something like:

~~~~~ el
((c++-mode
      (irony-header-directories-root . "/path/to/project/root")
      (irony-header-directories . ("utils" "some/include/patth"))))
~~~~~

2. Each variable `irony-header-directories`,
   `irony-header-directories-root`, `irony-config-commands` and
   `irony-extra-flags` can be an ELisp function that will find the
   relevants informations.

3. Using the `eproject` plugin, activated by the line `(irony-enable
'(eproject ...))`. Create a `.eproject` file at the root of your
project:

~~~~~ el
;; Irony mode configuration
:includes '("lib" "lib/Unix" "SFML/include")
:extra-flags '("-Weverything")
:config-commands '("pkg-config --cflags sdl")
~~~~~


Another method will probably appear soon, to be compatible with the
`.clang_complete` file of the
[clang_complete](http://www.vim.org/scripts/script.php?script_id=3302)
Vim plugin.

# Screenshots

![Optional Parameters](https://raw.github.com/Sarcasm/irony-mode/develop/screenshots/optional-parameters.png)

![Boost](https://raw.github.com/Sarcasm/irony-mode/develop/screenshots/boost-example.png)

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

```lisp
;; The Clang installation missed the system include directory
;; "/usr/lib/clang/3.2/include/", man clang said we can use the
;; environment variable CPATH.
(let ((old-cpath (getenv "CPATH")))
  (when (file-exists-p "/usr/lib/clang/3.2/include/")
    (setenv "CPATH" (if old-cpath
                        (concat old-cpath ":" "/usr/lib/clang/3.2/include/")
                      "/usr/lib/clang/3.2/include/"))))
```

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

```lisp
(setq ac-disable-faces (delq 'font-lock-string-face ac-disable-faces)))
```

Please create an issue if you find this not unacceptable.

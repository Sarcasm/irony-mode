# Status

Work in progress...


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

Assuming:

* [my fork](https://github.com/Sarcasm/auto-complete/) of
  [Auto Complete Mode](http://cx4a.org/software/auto-complete/) (it
  will be hopefully merged in the official auto-complete "soon")
* [YASnippet](https://github.com/capitaomorte/yasnippet)
* [eproject](https://github.com/jrockway/eproject)

are already loaded. [el-get](https://github.com/dimitri/el-get) help a
lot for package management in Emacs (see
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

# How to contribute

## File naming conventions

For C++:

* Templated functions and classes in file ending with the ".hpp"
  extension
* Functions and classes declaration in file ending with the ".h"
  extension
* Function and classes definitions in file ending with the ".cpp"
  extension

## Adding a plugin

Create a file named irony-PLUGIN_NAME.el

This file should contain 2 methods takind no argument:

1. `irony-PLUGIN_NAME-enable` called by `irony-enable module-names`
   usually in user configuration file. You can add a hook for on
   irony-mode if you need to be active in each irony-mode buffer.

2. `irony-PLUGIN_NAME-enable` called by `irony-disable module-names`
   usually in user configuration file. If a hook was added during
   `irony-PLUGIN_NAME-enable` you can remove it inside this function.


Example from the file *elisp/plugins/irony-ac.el*:

~~~~~ el
(defun irony-ac-setup ()
  "Hook to run for `auto-complete-mode' when `irony-mode' is
activated."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-irony)
  (define-key irony-mode-map [(control return)] 'ac-complete-irony))

(defun irony-ac-enable ()
  "Enable `auto-complete-mode' handling of `irony-mode'
completion results."
  (add-hook 'irony-mode-hook 'irony-ac-setup))

(defun irony-ac-disable ()
  "Disable `auto-complete-mode' handling of `irony-mode'
completion results."
  (remove-hook 'irony-mode-hook 'irony-ac-setup))
~~~~~

For more information look at the existing plugins or ask me.

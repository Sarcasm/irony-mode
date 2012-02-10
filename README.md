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

![Optional Parameters](./irony-mode/raw/master/screenshots/optional-parameters.png)

![Boost](./irony-mode/raw/master/screenshots/boost-example.png)


# Developpement conventions

## File naming conventions

For C++:

* Templated functions and classes in file ending with the ".hpp"
  extension
* Functions and classes declaration in file ending with the ".h"
  extension
* Function and classes definitions in file ending with the ".cpp"
  extension

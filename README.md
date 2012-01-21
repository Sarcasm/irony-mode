# Status

Work in progress...


# Download and compilation

Download with `git` >= 1.6.5 and later

    git clone --recursive git://github.com/Sarcasm/irony-mode.git

Download with `Git` < 1.6.5

    git clone git://github.com/Sarcasm/irony-mode.git
    git submodule update --init

Finally:

    cd irony-mode
    mkdir -p build && cd build
    cmake .. && make -j 4
    make install

# Emacs configuration

Assuming:

* [my fork](https://github.com/Sarcasm/auto-complete/) of
  [Auto Complete Mode](http://cx4a.org/software/auto-complete/)
* [YASnippet](http://code.google.com/p/yasnippet/)
* [eproject](https://github.com/jrockway/eproject)

are already loaded. [el-get](https://github.com/dimitri/el-get) help a
lot for package management in Emacs (see
[my configuration](https://github.com/Sarcasm/.emacs.d/blob/master/sarcasm-elisp/sarcasm-el-get.el)).

Copy and paste in the \*scratch\* buffer:

~~~~~ el
 (add-to-list 'load-path (expand-file-name "~/IRONY/MODE/PATH/elisp/"))
 (add-to-list 'load-path (expand-file-name "~/IRONY/MODE/PATH/elisp/plugins/"))
 (add-to-list 'load-path (expand-file-name "~/IRONY/MODE/PATH/elisp/plugins/posn-on-screen"))
 (add-to-list 'load-path (expand-file-name "~/IRONY/MODE/PATH/elisp/plugins/popup-frame"))
 (require 'irony)
 (irony-enable '(ac eproject))
 (add-hook 'c++-mode-hook 'irony-mode)
~~~~~

Hit `C-x h M-x eval-buffer RET`, open a C++ file and try the auto
completion feature.


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

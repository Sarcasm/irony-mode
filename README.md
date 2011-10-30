Status
======

Work in progress...

Compilation
===========

    git clone git@github.com:Sarcasm/irony-mode.git
    cd irony-mode        
    git submodule init
    git submodule update
    make

Emacs configuration
===================

Assuming:

* [my fork](https://github.com/Sarcasm/auto-complete/) of
  [Auto Complete Mode](http://cx4a.org/software/auto-complete/)
* [YASnippet](http://code.google.com/p/yasnippet/)
* [eproject](https://github.com/jrockway/eproject)

are already loaded. [el-get](https://github.com/dimitri/el-get) help a
lot for package management in Emacs (see
[my configuration](https://github.com/Sarcasm/.emacs.d/blob/master/sarcasm-elisp/sarcasm-el-get.el)).

Copy and paste in the \*scratch\* buffer:

~~~~~ lisp
 (add-to-list 'load-path (expand-file-name "~/IRONY/MODE/PATH/elisp/"))
 (add-to-list 'load-path (expand-file-name "~/IRONY/MODE/PATH/elisp/plugins/"))
 (require 'irony)
 (irony-enable '(ac eproject))
 (add-hook 'c++-mode-hook 'irony-mode)
~~~~~

Hit `C-x h M-x eval-buffer RET`, open a C++ file and try the auto completions.

Screenshots
===========

![Optional Parameters](./irony-mode/raw/master/screenshots/optional-parameters.png)

![Boost](./irony-mode/raw/master/screenshots/boost-example.png)

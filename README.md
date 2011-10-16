Status
======

Work in progress.

Note
====

It's not aimed to run on another machine than mine for the moment.

Compilation
===========

        git clone git@github.com:Sarcasm/irony-mode.git
        cd irony-mode        
        git submodule init
        git submodule update
        make

Emacs configuration
===================

Assuming [my fork](https://github.com/Sarcasm/auto-complete/) of
[Auto Complete Mode](http://cx4a.org/software/auto-complete/) and
[eproject](https://github.com/jrockway/eproject) are already loaded,
copy and paste the following and hit `C-x h M-x eval-buffer RET` in
the \*scratch\* buffer:

~~~~~ lisp
(add-to-list 'load-path (expand-file-name "~/pkg/irony-mode/elisp/"))
(add-to-list 'load-path (expand-file-name "~/pkg/irony-mode/elisp/plugins/"))
(require 'irony)
(irony-enable '(ac eproject))
(add-hook 'c++-mode-hook 'irony-mode)
~~~~~

Screenshots
===========

![Optional Parameters](./irony-mode/raw/master/screenshots/optional-parameters.png)

![Boost](./irony-mode/raw/master/screenshots/boost-example.png)

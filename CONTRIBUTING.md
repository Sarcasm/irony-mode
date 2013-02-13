# How to contribute

## File naming conventions

For C++:

* Templated functions and classes in file ending with the ".hpp"
  extension
* Functions and classes declaration in file ending with the ".h"
  extension
* Function and classes definitions in file ending with the ".cpp"
  extension

## C++ coding style

Please, before writing a new file take a look at how the existing
files are written. The license bit, the Doxygen header, the Emacs C++
modeline for headers `-*-C++-*-`.

```cpp
class CamelCaseClassName
{
  // types, constants, enums can be declared on top of the class
public:
  typedef void (*blah)(int);

private:
  enum
    {
      BOB,
      MICHELLE
    };

private:
  /**
   * member variables:
   * - declared before methods
   * - are aligned if possible, with `M-x align-region-or-current`
   */
  bool           camelCaseExceptFirstLetter_;
  float          endWithUnderscore_;
  // references have a space before and after '&'
  std::string &  ref_;
  // strangely pointers do not follow the same rules and require space
  // only before
  std::string   *ptr_;

  /**
   * Declare public stuff first, then protected/private at the end of
   * the file.
   *
   * Start with ctor/dtor/operator= then member functions
   */
public:
  /**
   * Constructors/init-list look like this.
   */
  CamelCaseClassName(bool camelCaseExceptFirstLetter)
    : camelCaseExceptFirstLetter_(camelCaseExceptFirstLetter)
    , endWithUnderscore_(3.0f)
    , ref_()
    , ptr_()
  { }

  /**
   * Empty body like this '{ }', but here it's a bad example because
   * we better let the compiler generate this destructor.
   */
  ~CamelCaseClassName()
  { }

  /**
   * Methods use this kind camelCase.
   *
   * For the comments, if possible follow how it's done in
   * `clang-c/Index.h`, with the exception that `\brief` doesn't need
   * to be everywhere, autobrief is fine.
   *
   * Exhaustive Doxygen document is not mandatory but it looks good,
   * and hopefully one day irony-mode will be able to show these kind
   * of documentations while working on the code, it would be nice to
   * have the sources compliant with this.
   *
   * \param bar
   *            pfiou.
   */
  void fooBar(int bar);

private:
  void barFoo();
};
```

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

# How to contribute

## C++ coding style

Please, before writing a new file take a look at how the existing files are
written. The license bit, the Doxygen header, the Emacs C++ modeline for headers
`-*-C++-*-`, ...

For formatting it is highly suggested to use
[Clang-Format](http://clang.llvm.org/docs/ClangFormat.html), a configuration
file is provided, the hidden file *.clang-format* at the root of the project.

Apart from some differences written in the *.clang-format* configuration file
and in the example that follows it's safe to follow the
[LLVM Coding Standards](http://llvm.org/docs/CodingStandards.html).

```cpp
/**
 * The order of declarations in the body of a class favor the public interface
 * first, e.g: public member functions first, then protected, then private, ...
 *
 * Member variables at the bottom of the class.
 */
class CamelCaseClassName {
  // types can be declared on the top of a class no matter of the
  // visibility if they are required by the rest of the class
public:
  typedef void (*blah)(int);

private:
  enum Name {
    BOB,
    MICHELLE
  };

  /**
   * Declare public stuff first, then protected/private at the end of the file.
   *
   * Start with ctor/dtor/operator= then member functions
   */
public:
  /**
   * The couple constructors/init-list looks like this.
   */
  CamelCaseClassName(bool camelCaseExceptFirstLetter)
    : camelCaseExceptFirstLetter_(camelCaseExceptFirstLetter)
    , endWithUnderscore_(3.0f)
    , ref_()
    , ptr_() {
  }

  /**
   * No one-line body, whether they are short or empty.
   */
  ~CamelCaseClassName() {
  }

  /**
   * Methods use this kind of camelCase.
   *
   * For the comments, if possible follow how it's done in `clang-c/Index.h`,
   * with the exception that `\brief` doesn't need to be everywhere, autobrief
   * is fine.
   *
   * Exhaustive Doxygen document is not mandatory but it looks good, and
   * hopefully one day irony-mode will be able to show these kind of
   * documentations while working on the code, it would be nice to have the
   * sources compliant with this.
   *
   * \param bar
   *            pfiou.
   */
  void fooBar(int bar);

private:
  Name barFoo();

private:
  /**
   * Member variables at the bottom.
   */
  bool camelCaseExceptFirstLetter_;
  float endWithUnderscore_;
  // references have a space before and after '&'
  std::string &ref_;
  // strangely pointers do not follow the same rules and require space
  // only before
  std::string *ptr_;
};
```

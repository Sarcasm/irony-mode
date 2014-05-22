#ifndef IRONY_MODE_SERVER_IRONY_H_
#define IRONY_MODE_SERVER_IRONY_H_

#include "TUManager.h"

#include <iosfwd>

class Irony {
public:
  Irony();

  /// \name Commands
  /// @{

  /// \brief Set or unset debugging of commands.
  void setDebug(bool enable) {
    if (enable)
      debug_ = true;
  }

  /// \brief Check that a given file with a set of flags compiles.
  ///
  /// Can be used as a hint to know whether or not the compile flags are
  /// correct.
  ///
  /// Example output:
  ///
  /// \verbatim
  ///   ()
  ///   (:fatals 1)
  ///   (:errors 3)
  ///   (:warnings 5)
  ///   (:errors 3 :warnings 5)
  /// \endverbatim
  void check(const std::string &file,
             const std::vector<std::string> &flags,
             const std::vector<CXUnsavedFile> &unsavedFiles);

  /// \brief Perform code completion at a given location.
  ///
  /// Print the list of candidate if any. The empty list is printed on error.
  ///
  /// Example output:
  ///
  /// \verbatim
  ///    (
  ///     ("foo")
  ///     ("bar")
  ///     ("baz")
  ///    )
  /// \endverbatim
  ///
  void complete(const std::string &file,
                unsigned line,
                unsigned col,
                const std::vector<std::string> &flags,
                const std::vector<CXUnsavedFile> &unsavedFiles);
  /// @}

private:
  TUManager tuManager_;
  bool debug_;
};

#endif // IRONY_MODE_SERVER_IRONY_H_

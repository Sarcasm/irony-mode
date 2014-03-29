#ifndef IRONY_MODE_SERVER_IRONY_H_
#define IRONY_MODE_SERVER_IRONY_H_

#include "TUManager.h"

#include <iosfwd>

class Irony {
public:
  Irony();

  /// Commands
  /// @{
  ///
  /// \brief Check if a file compiles. Can be used as a hint to know whether or
  /// not the compile flags are correct.
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
  void check(const std::string &file, const std::vector<std::string> &flags);
  /// }

private:
  TUManager tuManager_;
};

#endif // IRONY_MODE_SERVER_IRONY_H_

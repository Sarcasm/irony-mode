/**-*-C++-*-
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief JSONObjectWrapper class definition.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef IRONY_MODE_LIB_UTIL_JSONOBJECTWRAPPER_H_
#define IRONY_MODE_LIB_UTIL_JSONOBJECTWRAPPER_H_

#include "util/NonCopyable.h"

#include "JSON.h"
#include "JSONValue.h"

#include <memory>
#include <string>
#include <vector>

/**
 * This class aimed to be a wrapper for a JSONObject, it try to limit the check
 * needed by the user.
 *
 * \code
 * JSONObjectWrapper json(JSON::Parse("{ ... }"));
 *
 * if (json) {
 *   bool valid = true;
 *   double line = json.check(L"line", valid);
 *   double column = json.check(L"column", valid);
 *   std::vector<std::string> flags = parser.get(L"flags");
 *
 *   if (! valid)
 *     std::cerr << "invalid/incomplete data (line and/or column)."
 *               << std::endl;
 * }
 * \endcode
 *
 */
class JSONObjectWrapper : public util::NonCopyable {
public:
  class Checker;

public:
  /**
   * \brief Construct a \c JSONObjectWrapper for \p value.
   *
   * \attention The \p value argument is deleted in the destructor and
   *            should not be used outside of the class.
   *
   * \param value A JSON value, the value should be a JSON Object
   *              otherwise the \c good() method will return \c false.
   */
  explicit JSONObjectWrapper(JSONValue *value);

  /**
   * \brief Construct a \c JSONObjectWrapper without a value pointer
   *        but directly a JSONObject.
   *
   * \param object A JSON Object.
   */
  explicit JSONObjectWrapper(const JSONObject &object);

  /**
   * \brief Destructor, call \c delete on the stored value.
   *
   */
  virtual ~JSONObjectWrapper();

  /**
   * \brief Check the validity of the \c JSONObjectWrapper.
   *
   * \return \c true if the value is a \c JSONObject \c false
   *         otherwise.
   */
  bool good() const;

  /**
   * \see \c good().
   */
  operator bool() const;

  /**
   * \brief Get the value associated to \p key.
   *
   * If \p key is invalid or missing \p valid is set to \c false and
   * the default constructor of the value is returned (except for the
   * integer/unsigned where the \p valid can be false but the value
   * set to non 0, this is done when the value was not an integer but
   * a floating number). If not set to \c false \p valid it's left as
   * is (an \c false value will be returned even if the parameter was
   * good).
   *
   * \param key         The member of the object to check.
   * \param [out] valid Will be set to \c false if an error occur
   *                    otherwise left as is.
   * \param errorValue  The value to return in case of error.
   *
   * \return A \c Checker class that will do the conversion.
   */
  const Checker check(const std::wstring &key, bool &valid) const;

  /**
   * \see check(std::wstring key, bool & valid)
   *
   * \note The \c valid variable of \c check is missing that's all.
   *
   */
  const Checker get(const std::wstring &key) const;

  class Checker {
  public:
    Checker(const JSONObject &object,
            const JSONObject::const_iterator &key,
            bool &valid);
    Checker(const JSONObject &object, const JSONObject::const_iterator &key);

    // Basic conversions
    operator std::wstring() const;
    operator double() const;
    operator std::vector<std::string>() const;
    operator std::string() const;
    operator const JSONObject() const;

    /**
     * \brief Check if the number is really a unsigned int and if it's
     *        really a positive number (overflow not checked).
     *
     */
    operator unsigned() const;

    /**
     * \brief Check if the number is really an integer
     *        (overflow/underflow not checked).
     *
     */
    operator int() const;

  private:
    const JSONObject &object_;
    JSONObject::const_iterator key_;

    // note: when the check is not necessary 'valid_' is a reference
    // to 'dummy_'
    bool dummy_;
    bool &valid_;
  };

private:
  JSONValue *value_;
  bool isObject_;
  const JSONObject object_;
};

#endif /* !IRONY_MODE_LIB_UTIL_JSONOBJECTWRAPPER_H_ */

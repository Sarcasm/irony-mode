/**
 * \file   JSONObjectWrapper.hh
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Wed Aug 24 14:59:26 2011
 *
 * \brief  JSONObjectWrapper class definition.
 *
 *
 */

#ifndef _IRONY_UTILS_JSONOBJECTWRAPPER_HH_
#define _IRONY_UTILS_JSONOBJECTWRAPPER_HH_

#include <string>
#include <vector>
#include <memory>

// \c modf is used to check if numbers are integers.
// 'trick' found here:
// http://stackoverflow.com/questions/1521607/check-double-variable-if-it-contains-an-integer-and-not-floating-point/1521682#1521682
#include <cmath>

#include "JSON.h"
#include "JSONValue.h"

/**
 * This class aimed to be a wrapper for a JSONObject, it try to limit
 * the check needed by the user.
 *
 * \code
 * JSONObjectWrapper json(JSON::Parse("{ ... }"));
 *
 * if (json)
 *   {
 *     bool                     valid  = true;
 *     double                   line   = json.check(L"line", valid);
 *     double                   column = json.check(L"column", valid);
 *     std::vector<std::string> flags  = parser.get(L"flags");
 *
 *     if (! valid)
 *       std::cerr << "invalid/incomplete data (line and/or column)." << std::endl;
 *   }
 * \endcode
 *
 */
class JSONObjectWrapper
{
private:
  JSONValue        *value_;
  bool              isObject_;
  const JSONObject  object_;

public:
  class        Checker;

public:
  /**
   * Construct a \c JSONObjectWrapper, the \p value argument is
   * deleted in the destructor and should outside of the class.
   *
   * \param value A JSON value, the value should be a JSON Object
   *              otherwise the \code operator bool() \endcode will
   *              return false.
   */
  explicit JSONObjectWrapper(JSONValue *value);

  /**
   * Construct a \c JSONObjectWrapper without a value pointer but
   * directly a JSONObject.
   *
   * \param object A JSON Object.
   */
  explicit JSONObjectWrapper(const JSONObject & object);

  /**
   * Destructor, call \c delete on the value.
   *
   */
  virtual ~JSONObjectWrapper();

  /**
   * Check the validity of the \c JSONObjectWrapper.
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
   * Get the value associated to \p key. If \p key is invalid or
   * missing \p valid is set to \c false and the default constructor
   * of the value is returned (except for the integer/unsigned where
   * the \p valid can be false but the value set to non 0, this is
   * done when the value was not an integer but a floating number). If
   * not set to \c false \p valid it's left as is (an \c false value
   * will be returned even if the parameter was good).
   *
   * \param key         The member of the object to check.
   * \param [out] valid Will be set to \c false if an error occur
   *                    otherwise left as is.
   * \param errorValue  The value to return in case of error.
   *
   * \return A \c Checker class that will do the conversion.
   */
  const Checker check(const std::wstring & key, bool & valid) const;

  /**
   * \see check(std::wstring key, bool & valid)
   *
   * The \c valid variable of \c check is missing that's all.
   *
   */
  const Checker get(const std::wstring & key) const;

  class Checker {
  private:
    const JSONObject  &        object_;
    JSONObject::const_iterator key_;

    // note: when the check is not necessary 'valid_' is a reference
    // to 'dummy_'
    bool                dummy_;
    bool &              valid_;

  public:
    Checker(const JSONObject &                 object,
            const JSONObject::const_iterator & key,
            bool &                             valid);
    Checker(const JSONObject &                 object,
            const JSONObject::const_iterator & key);

    // Basic conversions
    operator std::wstring() const;
    operator double() const;
    operator std::vector<std::string>() const;
    operator std::string() const;
    operator const JSONObject () const;

    /**
     * Check if the number is really a unsigned int and if it's really
     * a positive number (overflow not checked).
     *
     */
    operator unsigned() const;

    /**
     * Check if the number is really an integer (overflow/underflow
     * not checked).
     *
     */
    operator int() const;
  };

private:
  JSONObjectWrapper(JSONObjectWrapper const &);
  JSONObjectWrapper& operator=(JSONObjectWrapper const &);
};

#endif /* !_IRONY_UTILS_JSONOBJECTWRAPPER_HH_ */

/**
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief JSONObjectWrapper implementation.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#include "util/JSONObjectWrapper.h"

#include "str/wstring_to_string.h"

// \c modf is used to check if numbers are integers.
//
// http://stackoverflow.com/questions/1521607/check-double-variable-if-it-contains-an-integer-and-not-floating-point/1521682#1521682
#include <cmath>

JSONObjectWrapper::JSONObjectWrapper(JSONValue *value)
  : value_(value)
  , isObject_(value ? value->IsObject() : false)
  , object_(value ? value->AsObject() : JSONObject()) {
}

JSONObjectWrapper::JSONObjectWrapper(const JSONObject &object)
  : value_(0), isObject_(true), object_(object) {
}

JSONObjectWrapper::~JSONObjectWrapper() {
  delete value_;
}

bool JSONObjectWrapper::good() const {
  return isObject_;
}
JSONObjectWrapper::operator bool() const {
  return isObject_;
}

const JSONObjectWrapper::Checker
JSONObjectWrapper::check(const std::wstring &key, bool &valid) const {
  return Checker(object_, object_.find(key), valid);
}

const JSONObjectWrapper::Checker
JSONObjectWrapper::get(const std::wstring &key) const {
  return Checker(object_, object_.find(key));
}

JSONObjectWrapper::Checker::Checker(const JSONObject &object,
                                    const JSONObject::const_iterator &key,
                                    bool &valid)
  : object_(object), key_(key), dummy_(true), valid_(valid) {
}

JSONObjectWrapper::Checker::Checker(const JSONObject &object,
                                    const JSONObject::const_iterator &key)
  : object_(object), key_(key), dummy_(true), valid_(dummy_) {
}

JSONObjectWrapper::Checker::operator std::wstring() const {
  if (key_ == object_.end() || !key_->second->IsString()) {
    valid_ = false;
    return std::wstring();
  }

  return key_->second->AsString();
}

JSONObjectWrapper::Checker::operator std::string() const {
  return str::wstring_to_string(this->operator std::wstring());
}

JSONObjectWrapper::Checker::operator std::vector<std::string>() const {
  if (key_ == object_.end() || !key_->second->IsArray()) {
    valid_ = false;
    return std::vector<std::string>();
  }

  const JSONArray &ary = key_->second->AsArray();

  // Check erroneous string object
  for (JSONArray::const_iterator it = ary.begin(), end = ary.end(); it != end;
       ++it) {
    if (!(*it)->IsString()) {
      valid_ = false;
      return std::vector<std::string>();
    }
  }

  std::vector<std::string> vect(ary.size());

  for (JSONArray::size_type i = 0, size = ary.size(); i < size; ++i)
    vect[i] = str::wstring_to_string(ary[i]->AsString());

  return vect;
}

JSONObjectWrapper::Checker::operator const JSONObject() const {
  if (key_ == object_.end() || !key_->second->IsObject()) {
    valid_ = false;
    return JSONObject();
  }

  return key_->second->AsObject();
}

JSONObjectWrapper::Checker::operator double() const {
  if (key_ == object_.end() || !key_->second->IsNumber()) {
    valid_ = false;
    return 0;
  }

  return key_->second->AsNumber();
}

JSONObjectWrapper::Checker::operator unsigned() const {
  double val = this->operator double();

  if (val < 0) {
    valid_ = false;
    return 0;
  }

  double intpart;

  if (std::modf(val, &intpart) != 0.0)
    valid_ = false;

  return static_cast<unsigned>(intpart);
}

JSONObjectWrapper::Checker::operator int() const {
  double val = this->operator double();
  double intpart;

  if (std::modf(val, &intpart) != 0.0)
    valid_ = false;

  return static_cast<int>(intpart);
}

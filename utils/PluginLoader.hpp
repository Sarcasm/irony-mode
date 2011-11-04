/**
 * @file   PluginLoader.hpp
 * @author Guillaume Papin <guillaume.papin@epitech.eu>
 * @date   Mon Jul 18 10:29:14 2011
 *
 * @brief  Shared libraries handling.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#ifndef _PLUGINLOADER_HPP_
#define _PLUGINLOADER_HPP_

#include <sys/types.h>
#include <dirent.h>

#include <cerrno>               // errno
#include <cstring>              // strerror

#include <stack>
#include <map>
#include <utility>

#include <algorithm>

#include "DLLoader.hpp"

/**
 * Exception for the class \c DLLoader.
 *
 */
class PluginLoaderException : public std::exception
{
private:
  std::string   msg_;

public:
  PluginLoaderException(const std::string & msg)
    : msg_(msg)
  { }
  virtual ~PluginLoaderException() throw() {}
  const char* what() const throw() { return msg_.c_str(); }
};

/**
 * This class handle the loading of plugins. Each plugin should
 * register a key \tparam K and a factory function \tparam Factory.
 *
 * \tparam K The factory key, a unique identifier.
 * \tparam Factory The factory associated to the key.
 */
template <typename K, typename Factory>
class PluginLoader
{
private:
  std::map<K, Factory>          plugins_;       /**< map of plugins */
  std::stack<DLLoader *>        libs_;          /**< list of loaded libraries */

public:
  /**
   * Create a new PluginLoader that register all shared libraries in
   * the directory \p directory.
   *
   * Each library found should provide a \c std::pair in the global
   * namespace called \p registeringName who register a key of type
   * \tparam K and a factory of type \tparam Factory.
   *
   * \throw PluginLoaderException When a problem occur while opening
   * the directory or the libraries.
   */
  PluginLoader(const std::string &      directory,
               const std::string &      registeringName)
    : plugins_(),
      libs_()
  {
    DIR *dp = opendir(directory.c_str());

    if (!dp)
      throw PluginLoaderException("can't open directory: " + directory + ": "
                                  + strerror(errno));
    while (struct dirent *ep = readdir(dp))
      {
        std::string            path(directory + "/" + ep->d_name);
        size_t                 dot = path.find_last_of('.');

        // Shared libraries (end with ".so")
        if (dot != std::string::npos && path.substr(dot) == ".so")
          {
            DLLoader              *lib;
            std::pair<K, Factory> *registeringPair;

            try {
              // Create a DLLoader and retrieve the pointer to the
              // global pair in the library.
              lib             = new DLLoader(path);
              registeringPair = lib->symbol(registeringName);
            } catch (const DLLoaderException & e) {
              destroyLibs();    // clean opened libraries
              throw PluginLoaderException(e.what());
            }

            // Register new plugin in the list
            plugins_.insert(*registeringPair);
            libs_.push(lib);
          }
      }
    closedir(dp);
  }

  virtual ~PluginLoader()
  {
    destroyLibs();
  }

  virtual const Factory operator[](K key) const
  {
    typename std::map<K, Factory> ::const_iterator      it = plugins_.find(key);
    if (it == plugins_.end())
      throw PluginLoaderException("Received an invalid key");
    return it->second;
  }

  virtual bool hasKey(K key) const
  {
    return plugins_.find(key) != plugins_.end();
  }

private:
  PluginLoader(PluginLoader const &);
  PluginLoader& operator=(PluginLoader const &);

  /**
   * Remove all created DLLoader.
   *
   */
  void destroyLibs()
  {
    while (!libs_.empty())
      {
        DLLoader        *lib = libs_.top();

        libs_.pop();
        delete lib;
      }
  }

};

#endif /* _PLUGINLOADER_HPP_ */

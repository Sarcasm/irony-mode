/**
 * \file   DLLoader.hpp
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Mon Jul 18 10:30:21 2011
 *
 * \brief  Shared library loader.
 *
 * \note: Compilation need the following flag for the linker:
 *      \verbatim LDFLAGS += -ldl \endverbatim .
 *
 */

#ifndef _UTILS_DLLOADER_HH_
#define _UTILS_DLLOADER_HH_

#include <string>
#include <cassert>

#include <dlfcn.h>

/**
 * Exception for the class \c DLLoader.
 *
 */
class DLLoaderException : public std::exception
{
private:
  std::string   msg_;

public:
  DLLoaderException()
    : msg_("Library loader error: (unknown)")
  { }

  DLLoaderException(const std::string & err_msg)
    : msg_("Library loader error: " + err_msg)
  { }
  virtual ~DLLoaderException() throw() {}
  const char* what() const throw() { return msg_.c_str(); }
};

/**
 * Shared library loader. This class try to follow the RAII
 * principles, when the class is destroyed the library is suppressed
 * from the memory so the libary loaded functions shouldn't be used
 * when the \c DLLoader is destroyed.
 *
 * Usage example:
 * \code
 * typedef void (*func_type)(type1, ..., typeN);
 *
 * try {
 *   DLLoader lib("lib.so");
 *   func_type func = lib.symbol("function_name");
 *   func(arg1, ..., argN);
 * // or
 * // lib.call<func_type>("function_name")(arg1, ..., argn);
 * } catch (const DLLoaderException & e) {
 *   std::cerr << e.what() << std::endl;
 * }
 * \endcode
 *
 * \note \c DLLoader objects aren't copyable.
 */
class DLLoader
{
private:
  void  *_handle;

  /**
   * Nested class that store a function pointer and can implicitly
   * convert the pointer to a function.
   *
   */
public:
  class Function {
  private:
    void *functionPtr_;

  public:
    Function(void *functionPtr) : functionPtr_(functionPtr)
    { }

    /**
     * Create a function pointer from the \c void*  member variable
     * \c functionPtr_.
     *
     * \return A function pointer of type \tparam FunctionPtr.
     */
    template <typename FunctionPtr> operator FunctionPtr()
    {
      return reinterpret_cast<FunctionPtr>(functionPtr_);
    }
  };

  /**
   * Create a DLLoader of the library \p library_file.
   *
   * \param library_file The library file to load.
   *
   * \throw DLLoaderException When the library is not
   */
  DLLoader(const std::string & library_file)
    : _handle(dlopen(library_file.c_str(), RTLD_LAZY))
  {
    if (_handle == NULL)
      {
        const char  *err_msg = dlerror();
        throw (err_msg ? DLLoaderException(err_msg) : DLLoaderException());
      }
  }

  /**
   * Extract a function named \p function_name in the library. This
   * function is handy for one-line call i.e.:
   * \code
   * try {
   *   DLLoader lib("libm.so");
   *   double cos_pi = lib.call<double (*)(double)>("cos")(3.14...);
   *   std::cout << "Cosine of pi is: " << cos_pi << std::endl;
   * } catch (const DLLoaderException & e) {
   *   std::cerr << e.what() << std::endl;
   * \endcode
   *
   * \param function_name
   *
   * \throw DLLoaderException In case \p function_name couldn't be
   * extracted.
   *
   * \return A pointer (to function) of the given type \c T.
   */
  template <typename T>
  T call(const std::string & function_name)
  {
    return T(symbol(function_name));
  }

  /**
   * Implicit conversion.
   * \see DLLoader::call()
   */
  Function symbol(const std::string & function_name)
  {
    (void) dlerror();

    void        *routine_ptr = dlsym(_handle, function_name.c_str());
    const char  *err_msg     = dlerror();

    if (err_msg  != NULL)
      throw DLLoaderException(err_msg);
    if (routine_ptr == NULL)
      throw DLLoaderException();

    return Function(routine_ptr);
  }

  /**
   * Destroy the DLLoader.
   *
   * \note After the destructor is called, any previous function
   *       retrieved with \c DLLoader::call() or \c
   *       DLLoader::operator() are invalid.
   */
  virtual ~DLLoader()
  {
    dlclose(_handle);
  }

private:
  DLLoader(const DLLoader &);
  DLLoader& operator=(DLLoader const &);
};

#endif // !_UTILS_DLLOADER_HH_

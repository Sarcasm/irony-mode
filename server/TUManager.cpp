/**
 * \file   TUManager.cpp
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 * \date   Wed Aug 24 01:16:23 2011
 *
 * \brief  See TUManager.hh
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#include <iostream>

#include "TUManager.h"

#include "str/wstring_to_string.h"

TUManager::TUManager()
  : index_(clang_createIndex(0, 0))
{ }

TUManager::~TUManager()
{
  clang_disposeIndex(index_);
}

CXTranslationUnit TUManager::parse(const std::string &              filename,
                                   const std::vector<std::string> & flags)
{
  CXTranslationUnit tu = translationUnits_[filename];

  if (! tu)
    {
      // FIXME: needed ?
      // Note: the -cc1 argument indicates the compiler front-end, and
      // not the driver, should be run. The compiler front-end has
      // several additional Clang specific features which are not
      // exposed through the GCC compatible driver interface.
      // from: http://clang.llvm.org/get_started.html
      // const_cast<std::vector<std::string> &>(flags).push_back("-cc1");

      std::size_t   nbArgs = flags.size();
      const char  **argv   = 0;

      if (nbArgs > 0)
        {
          argv         = new const char *[nbArgs + 1];
          argv[nbArgs] = 0;

          for (std::size_t i = 0; i < nbArgs; ++i)
            argv[i] = flags[i].c_str();
        }

      tu = clang_parseTranslationUnit(index_,
                                      filename.c_str(),
                                      argv, static_cast<int>(nbArgs),
                                      0, 0,
                                      clang_defaultEditingTranslationUnitOptions()
                                      | CXTranslationUnit_PrecompiledPreamble
                                      | CXTranslationUnit_CacheCompletionResults);
      delete [] argv;
      translationUnits_[filename] = tu;
    }

  if (! tu)
    {
      std::clog << "parsing \"" << filename << "\" failed." << std::endl;
      return 0;
    }

  // NOTE: Even at the first time the translation unit is reparsed,
  // because without this the completion is erroneous.

  // From the clang mailing list:
  // From: Douglas Gregor <dgregor-2kanFRK1NckAvxtiuMwx3w@public.gmane.org>
  // Subject: Re: Clang indexing library performance
  // Newsgroups: gmane.comp.compilers.clang.devel
  // ...
  // You want to use the "default editing options" when parsing the
  // translation unit
  //    clang_defaultEditingTranslationUnitOptions()
  // and then reparse at least once. That will enable the various
  // code-completion optimizations that should bring this time down
  // significantly.
  if (clang_reparseTranslationUnit(tu, 0, 0, clang_defaultReparseOptions(tu)))
    {
      // a 'fatal' error occur (even a diagnostic is impossible)
      clang_disposeTranslationUnit(tu);
      translationUnits_[filename] = 0;
    }

  return tu;
}

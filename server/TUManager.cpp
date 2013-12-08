/**
 * \file
 * \author Guillaume Papin <guillaume.papin@epitech.eu>
 *
 * \brief See TUManager.hh
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 *
 */

#include "TUManager.h"

#include <iostream>

typedef TUManager::SettingsID SettingsID;
typedef TUManager::Settings Settings;

Settings::Settings() : parseTUOptions(0) {
}

void Settings::merge(const Settings &other) {
  parseTUOptions |= other.parseTUOptions;
}

bool Settings::equal(const Settings &other) const {
  return parseTUOptions == other.parseTUOptions;
}

TUManager::TUManager()
  : index_(clang_createIndex(0, 0))
  , translationUnits_()
  , effectiveSettings_()
  , settingsList_() {
  effectiveSettings_ = computeEffectiveSettings();
}

TUManager::~TUManager() {
  clang_disposeIndex(index_);
}

CXTranslationUnit TUManager::parse(const std::string &filename,
                                   const std::vector<std::string> &flags) {
  CXTranslationUnit &tu = translationUnits_[filename];

  if (!tu) {
    std::size_t nbArgs = flags.size();
    const char **argv = 0;

    if (nbArgs > 0) {
      argv = new const char *[nbArgs + 1];

      for (std::size_t i = 0; i < nbArgs; ++i)
        argv[i] = flags[i].c_str();

      argv[nbArgs] = 0;
    }

    tu = clang_parseTranslationUnit(index_,
                                    filename.c_str(),
                                    argv,
                                    static_cast<int>(nbArgs),
                                    0,
                                    0,
                                    effectiveSettings_.parseTUOptions);
    delete[] argv;
  }

  if (!tu) {
    std::clog << "parsing \"" << filename << "\" failed." << std::endl;
    return 0;
  }

  // Reparsing is necessary to enable optimizations.
  //
  // From the clang mailing list (cfe-dev):
  // From: Douglas Gregor
  // Subject: Re: Clang indexing library performance
  // ...
  // You want to use the "default editing options" when parsing the translation
  // unit
  //    clang_defaultEditingTranslationUnitOptions()
  // and then reparse at least once. That will enable the various
  // code-completion optimizations that should bring this time down
  // significantly.
  if (clang_reparseTranslationUnit(tu, 0, 0, clang_defaultReparseOptions(tu))) {
    // a 'fatal' error occured (even a diagnostic is impossible)
    clang_disposeTranslationUnit(tu);
    tu = 0;
    return 0;
  }

  return tu;
}

SettingsID TUManager::registerSettings(const Settings &settings) {
  SettingsID settingsID = settingsList_.insert(settingsList_.end(), settings);

  onSettingsChanged();
  return settingsID;
}

void TUManager::unregisterSettings(SettingsID settingsID) {
  onSettingsChanged();
  settingsList_.erase(settingsID);
}

void TUManager::onSettingsChanged() {
  const Settings &newSettings = computeEffectiveSettings();

  if (newSettings.equal(effectiveSettings_))
    return;

  effectiveSettings_ = newSettings;
  invalidateAllCachedTUs();
}

Settings TUManager::computeEffectiveSettings() const {
  Settings settings;

  // XXX: A bug in old version of Clang (at least '3.1-8') caused the completion
  //      to fail on the standard library types when
  //      CXTranslationUnit_PrecompiledPreamble is used. We disable this option
  //      for old versions of libclang. As a result the completion will work but
  //      significantly slower.
  settings.parseTUOptions =
#if defined(CINDEX_VERSION_MAJOR) && defined(CINDEX_VERSION_MINOR) &&          \
    (CINDEX_VERSION_MAJOR > 0 || CINDEX_VERSION_MINOR >= 6)
      (clang_defaultEditingTranslationUnitOptions() |
       CXTranslationUnit_PrecompiledPreamble);
#else
      (clang_defaultEditingTranslationUnitOptions() &
       ~CXTranslationUnit_PrecompiledPreamble);
#endif

  for (std::list<Settings>::const_iterator it = settingsList_.begin(),
                                           end = settingsList_.end();
       it != end;
       ++it) {
    settings.merge(*it);
  }

  return settings;
}

void TUManager::invalidateCachedTU(const std::string &filename) {
  TranslationUnitsMap::iterator it = translationUnits_.find(filename);

  if (it != translationUnits_.end()) {
    if (CXTranslationUnit &tu = it->second)
      clang_disposeTranslationUnit(tu);

    translationUnits_.erase(it);
  }
}

void TUManager::invalidateAllCachedTUs() {
  TranslationUnitsMap::iterator it = translationUnits_.begin();

  while (it != translationUnits_.end()) {
    if (CXTranslationUnit &tu = it->second) {
      clang_disposeTranslationUnit(tu);
      translationUnits_.erase(it++); // post-increment keeps the iterator valid
    } else {
      ++it;
    }
  }
}

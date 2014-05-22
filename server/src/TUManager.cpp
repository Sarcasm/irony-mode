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

bool Settings::equals(const Settings &other) const {
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

CXTranslationUnit &TUManager::tuRef(const std::string &filename,
                                    const std::vector<std::string> &flags) {
  CXTranslationUnit &tu = translationUnits_[filename];

  // if the flags changed since the last time, invalidate the translation unit
  auto &flagsCache = flagsPerFileCache_[filename];
  if (flagsCache.size() != flags.size() ||
      !std::equal(flagsCache.begin(), flagsCache.end(), flags.begin())) {
    if (tu) {
      clang_disposeTranslationUnit(tu);
      tu = nullptr;
    }
    // remember the flags for the next parse
    flagsCache = flags;
  }
  return tu;
}

CXTranslationUnit
TUManager::parse(const std::string &filename,
                 const std::vector<std::string> &flags,
                 const std::vector<CXUnsavedFile> &unsavedFiles) {
  CXTranslationUnit &tu = tuRef(filename, flags);

  if (tu == nullptr) {
    std::size_t nbArgs = flags.size();
    std::vector<const char *> argv(nbArgs + 1);

    for (std::size_t i = 0; i < nbArgs; ++i) {
      argv[i] = flags[i].c_str();
    }
    // don't think the ending null is necessary but that's how the argument
    // vector from a main ends
    argv.at(nbArgs) = nullptr;

    tu = clang_parseTranslationUnit(
        index_,
        filename.c_str(),
        argv.data(),
        static_cast<int>(nbArgs),
        const_cast<CXUnsavedFile *>(unsavedFiles.data()),
        unsavedFiles.size(),
        effectiveSettings_.parseTUOptions);
  }

  if (tu == nullptr) {
    std::clog << "error: libclang couldn't parse '" << filename << "'\n";
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
  if (clang_reparseTranslationUnit(
          tu,
          unsavedFiles.size(),
          const_cast<CXUnsavedFile *>(unsavedFiles.data()),
          clang_defaultReparseOptions(tu))) {
    // a 'fatal' error occured (even a diagnostic is impossible)
    clang_disposeTranslationUnit(tu);
    std::clog << "error: libclang couldn't reparse '" << filename << "'\n";
    tu = 0;
    return 0;
  }

  return tu;
}

CXTranslationUnit
TUManager::getOrCreateTU(const std::string &filename,
                         const std::vector<std::string> &flags,
                         const std::vector<CXUnsavedFile> &unsavedFiles) {
  if (auto tu = tuRef(filename, flags))
    return tu;

  return parse(filename, flags, unsavedFiles);
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

  if (newSettings.equals(effectiveSettings_))
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
  // -- https://github.com/Sarcasm/irony-mode/issues/4
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

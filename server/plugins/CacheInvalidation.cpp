#include "CacheInvalidation.h"

#include <iostream>

CacheInvalidation::CacheInvalidation(TUManager & tuManager)
  : tuManager_(tuManager)
{ }

CacheInvalidation::~CacheInvalidation()
{ }

std::string CacheInvalidation::handleRequest(const JSONObjectWrapper & data,
                                             std::string &             buf)
{
  bool                valid = true;
  const std::string & file  = data.check(L"file", valid);

  buf += ":value ";
  if (! valid)
    {
      buf += ":error";
      std::clog << "invalid reload-flags request." << std::endl;
    } else {
    tuManager_.invalidateCachedTU(file);
    buf += ":success";
  }
  return ":status-code";
}

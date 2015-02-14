/**
 * \file
 * \brief Wrapper around Clang Indexing Public C Interface header.
 *
 * This file is distributed under the GNU General Public License. See
 * COPYING for details.
 */

#ifndef IRONY_MODE_SERVER_SUPPORT_CINDEXVERSION_H_
#define IRONY_MODE_SERVER_SUPPORT_CINDEXVERSION_H_

#include <clang-c/Index.h>

/// Use <tt>#if CINDEX_VERSION_VERSION > 10047</tt> to test for
/// CINDEX_VERSION_MAJOR = 1 and CINDEX_VERSION_MINOR = 47.
#ifndef CINDEX_VERSION
#define CINDEX_VERSION 0 ///< pre-clang 3.2 support
#endif

#endif /* !IRONY_MODE_SERVER_SUPPORT_CINDEXVERSION_H_ */

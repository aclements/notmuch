/* notmuch - Not much of an email library, (just index and search)
 *
 * Copyright © 2009 Carl Worth
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

/**
 * @defgroup notmuch The notmuch API
 *
 * Not much of an email library, (just index and search)
 *
 * @{
 */

#ifndef NOTMUCH_H
#define NOTMUCH_H

#ifndef __DOXYGEN__

#ifdef  __cplusplus
# define NOTMUCH_BEGIN_DECLS  extern "C" {
# define NOTMUCH_END_DECLS    }
#else
# define NOTMUCH_BEGIN_DECLS
# define NOTMUCH_END_DECLS
#endif

NOTMUCH_BEGIN_DECLS

#include <time.h>

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

/*
 * The library version number.  This must agree with the soname
 * version in Makefile.local.
 */
#define LIBNOTMUCH_MAJOR_VERSION	3
#define LIBNOTMUCH_MINOR_VERSION	1
#define LIBNOTMUCH_MICRO_VERSION	0

#endif /* __DOXYGEN__ */

/**
 * Check the version of the notmuch library being compiled against.
 *
 * Return true if the library being compiled against is of the
 * specified version or above. For example:
 *
 * @code
 * #if LIBNOTMUCH_CHECK_VERSION(3, 1, 0)
 *     (code requiring libnotmuch 3.1.0 or above)
 * #endif
 * @endcode
 *
 * LIBNOTMUCH_CHECK_VERSION has been defined since version 3.1.0; to
 * check for versions prior to that, use:
 *
 * @code
 * #if !defined(NOTMUCH_CHECK_VERSION)
 *     (code requiring libnotmuch prior to 3.1.0)
 * #endif
 * @endcode
 */
#define LIBNOTMUCH_CHECK_VERSION (major, minor, micro)			\
    (LIBNOTMUCH_MAJOR_VERSION > (major) ||					\
     (LIBNOTMUCH_MAJOR_VERSION == (major) && LIBNOTMUCH_MINOR_VERSION > (minor)) || \
     (LIBNOTMUCH_MAJOR_VERSION == (major) && LIBNOTMUCH_MINOR_VERSION == (minor) && \
      LIBNOTMUCH_MICRO_VERSION >= (micro)))

/**
 * Notmuch boolean type.
 */
typedef int notmuch_bool_t;

/**
 * Status codes used for the return values of most functions.
 *
 * A zero value (NOTMUCH_STATUS_SUCCESS) indicates that the function
 * completed without error. Any other value indicates an error.
 */
typedef enum _notmuch_status {
    /**
     * No error occurred.
     */
    NOTMUCH_STATUS_SUCCESS = 0,
    /**
     * Out of memory.
     */
    NOTMUCH_STATUS_OUT_OF_MEMORY,
    /**
     * An attempt was made to write to a database opened in read-only
     * mode.
     */
    NOTMUCH_STATUS_READ_ONLY_DATABASE,
    /**
     * A Xapian exception occurred.
     */
    NOTMUCH_STATUS_XAPIAN_EXCEPTION,
    /**
     * An error occurred trying to read or write to a file (this could
     * be file not found, permission denied, etc.)
     *
     * @todo We don't really want to expose this lame XAPIAN_EXCEPTION
     * value. Instead we should map to things like DATABASE_LOCKED or
     * whatever.
     */
    NOTMUCH_STATUS_FILE_ERROR,
    /**
     * A file was presented that doesn't appear to be an email
     * message.
     */
    NOTMUCH_STATUS_FILE_NOT_EMAIL,
    /**
     * A file contains a message ID that is identical to a message
     * already in the database.
     */
    NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID,
    /**
     * The user erroneously passed a NULL pointer to a notmuch
     * function.
     */
    NOTMUCH_STATUS_NULL_POINTER,
    /**
     * A tag value is too long (exceeds NOTMUCH_TAG_MAX).
     */
    NOTMUCH_STATUS_TAG_TOO_LONG,
    /**
     * The notmuch_message_thaw function has been called more times
     * than notmuch_message_freeze.
     */
    NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW,
    /**
     * notmuch_database_end_atomic has been called more times than
     * notmuch_database_begin_atomic.
     */
    NOTMUCH_STATUS_UNBALANCED_ATOMIC,
    /**
     * The operation is not supported.
     */
    NOTMUCH_STATUS_UNSUPPORTED_OPERATION,
    /**
     * The operation requires a database upgrade.
     */
    NOTMUCH_STATUS_UPGRADE_REQUIRED,
    /**
     * Not an actual status value. Just a way to find out how many
     * valid status values there are.
     */
    NOTMUCH_STATUS_LAST_STATUS
} notmuch_status_t;

/**
 * Get a string representation of a notmuch_status_t value.
 *
 * The result is read-only.
 */
const char *
notmuch_status_to_string (notmuch_status_t status);

/* Various opaque data types. For each notmuch_<foo>_t see the various
 * notmuch_<foo> functions below. */
#ifndef __DOXYGEN__
typedef struct _notmuch_database notmuch_database_t;
typedef struct _notmuch_query notmuch_query_t;
typedef struct _notmuch_threads notmuch_threads_t;
typedef struct _notmuch_thread notmuch_thread_t;
typedef struct _notmuch_messages notmuch_messages_t;
typedef struct _notmuch_message notmuch_message_t;
typedef struct _notmuch_tags notmuch_tags_t;
typedef struct _notmuch_directory notmuch_directory_t;
typedef struct _notmuch_filenames notmuch_filenames_t;
#endif /* __DOXYGEN__ */

/**
 * Create a new, empty notmuch database located at 'path'.
 *
 * The path should be a top-level directory to a collection of
 * plain-text email messages (one message per file). This call will
 * create a new ".notmuch" directory within 'path' where notmuch will
 * store its data.
 *
 * After a successful call to notmuch_database_create, the returned
 * database will be open so the caller should call
 * notmuch_database_destroy when finished with it.
 *
 * The database will not yet have any data in it
 * (notmuch_database_create itself is a very cheap function). Messages
 * contained within 'path' can be added to the database by calling
 * notmuch_database_add_message.
 *
 * In case of any failure, this function returns an error status and
 * sets *database to NULL (after printing an error message on stderr).
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Successfully created the database.
 *
 * NOTMUCH_STATUS_NULL_POINTER: The given 'path' argument is NULL.
 *
 * NOTMUCH_STATUS_OUT_OF_MEMORY: Out of memory.
 *
 * NOTMUCH_STATUS_FILE_ERROR: An error occurred trying to create the
 *	database file (such as permission denied, or file not found,
 *	etc.), or the database already exists.
 *
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION: A Xapian exception occurred.
 */
notmuch_status_t
notmuch_database_create (const char *path, notmuch_database_t **database);

/**
 * Database open mode for notmuch_database_open.
 */
typedef enum {
    /**
     * Open database for reading only.
     */
    NOTMUCH_DATABASE_MODE_READ_ONLY = 0,
    /**
     * Open database for reading and writing.
     */
    NOTMUCH_DATABASE_MODE_READ_WRITE
} notmuch_database_mode_t;

/**
 * Open an existing notmuch database located at 'path'.
 *
 * The database should have been created at some time in the past,
 * (not necessarily by this process), by calling
 * notmuch_database_create with 'path'. By default the database should be
 * opened for reading only. In order to write to the database you need to
 * pass the NOTMUCH_DATABASE_MODE_READ_WRITE mode.
 *
 * An existing notmuch database can be identified by the presence of a
 * directory named ".notmuch" below 'path'.
 *
 * The caller should call notmuch_database_destroy when finished with
 * this database.
 *
 * In case of any failure, this function returns an error status and
 * sets *database to NULL (after printing an error message on stderr).
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Successfully opened the database.
 *
 * NOTMUCH_STATUS_NULL_POINTER: The given 'path' argument is NULL.
 *
 * NOTMUCH_STATUS_OUT_OF_MEMORY: Out of memory.
 *
 * NOTMUCH_STATUS_FILE_ERROR: An error occurred trying to open the
 *	database file (such as permission denied, or file not found,
 *	etc.), or the database version is unknown.
 *
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION: A Xapian exception occurred.
 */
notmuch_status_t
notmuch_database_open (const char *path,
		       notmuch_database_mode_t mode,
		       notmuch_database_t **database);

/**
 * Close the given notmuch database.
 *
 * After notmuch_database_close has been called, calls to other
 * functions on objects derived from this database may either behave
 * as if the database had not been closed (e.g., if the required data
 * has been cached) or may fail with a
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION.
 *
 * notmuch_database_close can be called multiple times.  Later calls
 * have no effect.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Successfully closed the database.
 *
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION: A Xapian exception occurred; the
 *	database has been closed but there are no guarantees the
 *	changes to the database, if any, have been flushed to disk.
 */
notmuch_status_t
notmuch_database_close (notmuch_database_t *database);

/**
 * A callback invoked by notmuch_database_compact to notify the user
 * of the progress of the compaction process.
 */
typedef void (*notmuch_compact_status_cb_t)(const char *message, void *closure);

/**
 * Compact a notmuch database, backing up the original database to the
 * given path.
 *
 * The database will be opened with NOTMUCH_DATABASE_MODE_READ_WRITE
 * during the compaction process to ensure no writes are made.
 *
 * If the optional callback function 'status_cb' is non-NULL, it will
 * be called with diagnostic and informational messages. The argument
 * 'closure' is passed verbatim to any callback invoked.
 */
notmuch_status_t
notmuch_database_compact (const char* path,
			  const char* backup_path,
			  notmuch_compact_status_cb_t status_cb,
			  void *closure);

/**
 * Destroy the notmuch database, closing it if necessary and freeing
 * all associated resources.
 *
 * Return value as in notmuch_database_close if the database was open;
 * notmuch_database_destroy itself has no failure modes.
 */
notmuch_status_t
notmuch_database_destroy (notmuch_database_t *database);

/**
 * Return the database path of the given database.
 *
 * The return value is a string owned by notmuch so should not be
 * modified nor freed by the caller.
 */
const char *
notmuch_database_get_path (notmuch_database_t *database);

/**
 * Return the database format version of the given database.
 */
unsigned int
notmuch_database_get_version (notmuch_database_t *database);

/**
 * Does this database need to be upgraded before writing to it?
 *
 * If this function returns TRUE then no functions that modify the
 * database (notmuch_database_add_message, notmuch_message_add_tag,
 * notmuch_directory_set_mtime, etc.) will work unless the function
 * notmuch_database_upgrade is called successfully first.
 */
notmuch_bool_t
notmuch_database_needs_upgrade (notmuch_database_t *database);

/**
 * Upgrade the current database.
 *
 * After opening a database in read-write mode, the client should
 * check if an upgrade is needed (notmuch_database_needs_upgrade) and
 * if so, upgrade with this function before making any modifications.
 *
 * The optional progress_notify callback can be used by the caller to
 * provide progress indication to the user. If non-NULL it will be
 * called periodically with 'progress' as a floating-point value in
 * the range of [0.0 .. 1.0] indicating the progress made so far in
 * the upgrade process.  The argument 'closure' is passed verbatim to
 * any callback invoked.
 */
notmuch_status_t
notmuch_database_upgrade (notmuch_database_t *database,
			  void (*progress_notify) (void *closure,
						   double progress),
			  void *closure);

/**
 * Begin an atomic database operation.
 *
 * Any modifications performed between a successful begin and a
 * notmuch_database_end_atomic will be applied to the database
 * atomically.  Note that, unlike a typical database transaction, this
 * only ensures atomicity, not durability; neither begin nor end
 * necessarily flush modifications to disk.
 *
 * Atomic sections may be nested.  begin_atomic and end_atomic must
 * always be called in pairs.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Successfully entered atomic section.
 *
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION: A Xapian exception occurred;
 *	atomic section not entered.
 */
notmuch_status_t
notmuch_database_begin_atomic (notmuch_database_t *notmuch);

/**
 * Indicate the end of an atomic database operation.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Successfully completed atomic section.
 *
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION: A Xapian exception occurred;
 *	atomic section not ended.
 *
 * NOTMUCH_STATUS_UNBALANCED_ATOMIC: The database is not currently in
 *	an atomic section.
 */
notmuch_status_t
notmuch_database_end_atomic (notmuch_database_t *notmuch);

/**
 * Retrieve a directory object from the database for 'path'.
 *
 * Here, 'path' should be a path relative to the path of 'database'
 * (see notmuch_database_get_path), or else should be an absolute path
 * with initial components that match the path of 'database'.
 *
 * If this directory object does not exist in the database, this
 * returns NOTMUCH_STATUS_SUCCESS and sets *directory to NULL.
 *
 * Otherwise the returned directory object is owned by the database
 * and as such, will only be valid until notmuch_database_destroy is
 * called.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Successfully retrieved directory.
 *
 * NOTMUCH_STATUS_NULL_POINTER: The given 'directory' argument is NULL.
 *
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION: A Xapian exception occurred;
 *	directory not retrieved.
 *
 * NOTMUCH_STATUS_UPGRADE_REQUIRED: The caller must upgrade the
 * 	database to use this function.
 */
notmuch_status_t
notmuch_database_get_directory (notmuch_database_t *database,
				const char *path,
				notmuch_directory_t **directory);

/**
 * Add a new message to the given notmuch database or associate an
 * additional filename with an existing message.
 *
 * Here, 'filename' should be a path relative to the path of
 * 'database' (see notmuch_database_get_path), or else should be an
 * absolute filename with initial components that match the path of
 * 'database'.
 *
 * The file should be a single mail message (not a multi-message mbox)
 * that is expected to remain at its current location, (since the
 * notmuch database will reference the filename, and will not copy the
 * entire contents of the file.
 *
 * If another message with the same message ID already exists in the
 * database, rather than creating a new message, this adds 'filename'
 * to the list of the filenames for the existing message.
 *
 * If 'message' is not NULL, then, on successful return
 * (NOTMUCH_STATUS_SUCCESS or NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID) '*message'
 * will be initialized to a message object that can be used for things
 * such as adding tags to the just-added message. The user should call
 * notmuch_message_destroy when done with the message. On any failure
 * '*message' will be set to NULL.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Message successfully added to database.
 *
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION: A Xapian exception occurred,
 *	message not added.
 *
 * NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID: Message has the same message
 *	ID as another message already in the database. The new
 *	filename was successfully added to the message in the database
 *	(if not already present) and the existing message is returned.
 *
 * NOTMUCH_STATUS_FILE_ERROR: an error occurred trying to open the
 *	file, (such as permission denied, or file not found,
 *	etc.). Nothing added to the database.
 *
 * NOTMUCH_STATUS_FILE_NOT_EMAIL: the contents of filename don't look
 *	like an email message. Nothing added to the database.
 *
 * NOTMUCH_STATUS_READ_ONLY_DATABASE: Database was opened in read-only
 *	mode so no message can be added.
 *
 * NOTMUCH_STATUS_UPGRADE_REQUIRED: The caller must upgrade the
 * 	database to use this function.
 */
notmuch_status_t
notmuch_database_add_message (notmuch_database_t *database,
			      const char *filename,
			      notmuch_message_t **message);

/**
 * Remove a message filename from the given notmuch database. If the
 * message has no more filenames, remove the message.
 *
 * If the same message (as determined by the message ID) is still
 * available via other filenames, then the message will persist in the
 * database for those filenames. When the last filename is removed for
 * a particular message, the database content for that message will be
 * entirely removed.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: The last filename was removed and the
 *	message was removed from the database.
 *
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION: A Xapian exception occurred,
 *	message not removed.
 *
 * NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID: This filename was removed but
 *	the message persists in the database with at least one other
 *	filename.
 *
 * NOTMUCH_STATUS_READ_ONLY_DATABASE: Database was opened in read-only
 *	mode so no message can be removed.
 *
 * NOTMUCH_STATUS_UPGRADE_REQUIRED: The caller must upgrade the
 * 	database to use this function.
 */
notmuch_status_t
notmuch_database_remove_message (notmuch_database_t *database,
				 const char *filename);

/**
 * Find a message with the given message_id.
 *
 * If a message with the given message_id is found then, on successful return
 * (NOTMUCH_STATUS_SUCCESS) '*message' will be initialized to a message
 * object.  The caller should call notmuch_message_destroy when done with the
 * message.
 *
 * On any failure or when the message is not found, this function initializes
 * '*message' to NULL. This means, when NOTMUCH_STATUS_SUCCESS is returned, the
 * caller is supposed to check '*message' for NULL to find out whether the
 * message with the given message_id was found.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Successful return, check '*message'.
 *
 * NOTMUCH_STATUS_NULL_POINTER: The given 'message' argument is NULL
 *
 * NOTMUCH_STATUS_OUT_OF_MEMORY: Out of memory, creating message object
 *
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION: A Xapian exception occurred
 */
notmuch_status_t
notmuch_database_find_message (notmuch_database_t *database,
			       const char *message_id,
			       notmuch_message_t **message);

/**
 * Find a message with the given filename.
 *
 * If the database contains a message with the given filename then, on
 * successful return (NOTMUCH_STATUS_SUCCESS) '*message' will be initialized to
 * a message object. The caller should call notmuch_message_destroy when done
 * with the message.
 *
 * On any failure or when the message is not found, this function initializes
 * '*message' to NULL. This means, when NOTMUCH_STATUS_SUCCESS is returned, the
 * caller is supposed to check '*message' for NULL to find out whether the
 * message with the given filename is found.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Successful return, check '*message'
 *
 * NOTMUCH_STATUS_NULL_POINTER: The given 'message' argument is NULL
 *
 * NOTMUCH_STATUS_OUT_OF_MEMORY: Out of memory, creating the message object
 *
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION: A Xapian exception occurred
 *
 * NOTMUCH_STATUS_UPGRADE_REQUIRED: The caller must upgrade the
 * 	database to use this function.
 */
notmuch_status_t
notmuch_database_find_message_by_filename (notmuch_database_t *notmuch,
					   const char *filename,
					   notmuch_message_t **message);

/**
 * Return a list of all tags found in the database.
 *
 * This function creates a list of all tags found in the database. The
 * resulting list contains all tags from all messages found in the database.
 *
 * On error this function returns NULL.
 */
notmuch_tags_t *
notmuch_database_get_all_tags (notmuch_database_t *db);

/**
 * Create a new query for 'database'.
 *
 * Here, 'database' should be an open database, (see
 * notmuch_database_open and notmuch_database_create).
 *
 * For the query string, we'll document the syntax here more
 * completely in the future, but it's likely to be a specialized
 * version of the general Xapian query syntax:
 *
 * http://xapian.org/docs/queryparser.html
 *
 * As a special case, passing either a length-zero string, (that is ""),
 * or a string consisting of a single asterisk (that is "*"), will
 * result in a query that returns all messages in the database.
 *
 * See notmuch_query_set_sort for controlling the order of results.
 * See notmuch_query_search_messages and notmuch_query_search_threads
 * to actually execute the query.
 *
 * User should call notmuch_query_destroy when finished with this
 * query.
 *
 * Will return NULL if insufficient memory is available.
 */
notmuch_query_t *
notmuch_query_create (notmuch_database_t *database,
		      const char *query_string);

/**
 * Sort values for notmuch_query_set_sort.
 */
typedef enum {
    /**
     * Oldest first.
     */
    NOTMUCH_SORT_OLDEST_FIRST,
    /**
     * Newest first.
     */
    NOTMUCH_SORT_NEWEST_FIRST,
    /**
     * Sort by message-id.
     */
    NOTMUCH_SORT_MESSAGE_ID,
    /**
     * Do not sort.
     */
    NOTMUCH_SORT_UNSORTED
} notmuch_sort_t;

/**
 * Return the query_string of this query. See notmuch_query_create.
 */
const char *
notmuch_query_get_query_string (notmuch_query_t *query);

/**
 * Exclude values for notmuch_query_set_omit_excluded. The strange
 * order is to maintain backward compatibility: the old FALSE/TRUE
 * options correspond to the new
 * NOTMUCH_EXCLUDE_FLAG/NOTMUCH_EXCLUDE_TRUE options.
 */
typedef enum {
    NOTMUCH_EXCLUDE_FLAG,
    NOTMUCH_EXCLUDE_TRUE,
    NOTMUCH_EXCLUDE_FALSE,
    NOTMUCH_EXCLUDE_ALL
} notmuch_exclude_t;

/**
 * Specify whether to omit excluded results or simply flag them.  By
 * default, this is set to TRUE.
 *
 * If set to TRUE or ALL, notmuch_query_search_messages will omit excluded
 * messages from the results, and notmuch_query_search_threads will omit
 * threads that match only in excluded messages.  If set to TRUE,
 * notmuch_query_search_threads will include all messages in threads that
 * match in at least one non-excluded message.  Otherwise, if set to ALL,
 * notmuch_query_search_threads will omit excluded messages from all threads.
 *
 * If set to FALSE or FLAG then both notmuch_query_search_messages and
 * notmuch_query_search_threads will return all matching
 * messages/threads regardless of exclude status. If set to FLAG then
 * the exclude flag will be set for any excluded message that is
 * returned by notmuch_query_search_messages, and the thread counts
 * for threads returned by notmuch_query_search_threads will be the
 * number of non-excluded messages/matches. Otherwise, if set to
 * FALSE, then the exclude status is completely ignored.
 *
 * The performance difference when calling
 * notmuch_query_search_messages should be relatively small (and both
 * should be very fast).  However, in some cases,
 * notmuch_query_search_threads is very much faster when omitting
 * excluded messages as it does not need to construct the threads that
 * only match in excluded messages.
 */
void
notmuch_query_set_omit_excluded (notmuch_query_t *query,
				 notmuch_exclude_t omit_excluded);

/**
 * Specify the sorting desired for this query.
 */
void
notmuch_query_set_sort (notmuch_query_t *query, notmuch_sort_t sort);

/**
 * Return the sort specified for this query. See
 * notmuch_query_set_sort.
 */
notmuch_sort_t
notmuch_query_get_sort (notmuch_query_t *query);

/**
 * Add a tag that will be excluded from the query results by default.
 * This exclusion will be overridden if this tag appears explicitly in
 * the query.
 */
void
notmuch_query_add_tag_exclude (notmuch_query_t *query, const char *tag);

/**
 * Execute a query for threads, returning a notmuch_threads_t object
 * which can be used to iterate over the results. The returned threads
 * object is owned by the query and as such, will only be valid until
 * notmuch_query_destroy.
 *
 * Typical usage might be:
 *
 *     notmuch_query_t *query;
 *     notmuch_threads_t *threads;
 *     notmuch_thread_t *thread;
 *
 *     query = notmuch_query_create (database, query_string);
 *
 *     for (threads = notmuch_query_search_threads (query);
 *          notmuch_threads_valid (threads);
 *          notmuch_threads_move_to_next (threads))
 *     {
 *         thread = notmuch_threads_get (threads);
 *         ....
 *         notmuch_thread_destroy (thread);
 *     }
 *
 *     notmuch_query_destroy (query);
 *
 * Note: If you are finished with a thread before its containing
 * query, you can call notmuch_thread_destroy to clean up some memory
 * sooner (as in the above example). Otherwise, if your thread objects
 * are long-lived, then you don't need to call notmuch_thread_destroy
 * and all the memory will still be reclaimed when the query is
 * destroyed.
 *
 * Note that there's no explicit destructor needed for the
 * notmuch_threads_t object. (For consistency, we do provide a
 * notmuch_threads_destroy function, but there's no good reason
 * to call it if the query is about to be destroyed).
 *
 * If a Xapian exception occurs this function will return NULL.
 */
notmuch_threads_t *
notmuch_query_search_threads (notmuch_query_t *query);

/**
 * Execute a query for messages, returning a notmuch_messages_t object
 * which can be used to iterate over the results. The returned
 * messages object is owned by the query and as such, will only be
 * valid until notmuch_query_destroy.
 *
 * Typical usage might be:
 *
 *     notmuch_query_t *query;
 *     notmuch_messages_t *messages;
 *     notmuch_message_t *message;
 *
 *     query = notmuch_query_create (database, query_string);
 *
 *     for (messages = notmuch_query_search_messages (query);
 *          notmuch_messages_valid (messages);
 *          notmuch_messages_move_to_next (messages))
 *     {
 *         message = notmuch_messages_get (messages);
 *         ....
 *         notmuch_message_destroy (message);
 *     }
 *
 *     notmuch_query_destroy (query);
 *
 * Note: If you are finished with a message before its containing
 * query, you can call notmuch_message_destroy to clean up some memory
 * sooner (as in the above example). Otherwise, if your message
 * objects are long-lived, then you don't need to call
 * notmuch_message_destroy and all the memory will still be reclaimed
 * when the query is destroyed.
 *
 * Note that there's no explicit destructor needed for the
 * notmuch_messages_t object. (For consistency, we do provide a
 * notmuch_messages_destroy function, but there's no good
 * reason to call it if the query is about to be destroyed).
 *
 * If a Xapian exception occurs this function will return NULL.
 */
notmuch_messages_t *
notmuch_query_search_messages (notmuch_query_t *query);

/**
 * Destroy a notmuch_query_t along with any associated resources.
 *
 * This will in turn destroy any notmuch_threads_t and
 * notmuch_messages_t objects generated by this query, (and in
 * turn any notmuch_thread_t and notmuch_message_t objects generated
 * from those results, etc.), if such objects haven't already been
 * destroyed.
 */
void
notmuch_query_destroy (notmuch_query_t *query);

/**
 * Is the given 'threads' iterator pointing at a valid thread.
 *
 * When this function returns TRUE, notmuch_threads_get will return a
 * valid object. Whereas when this function returns FALSE,
 * notmuch_threads_get will return NULL.
 *
 * If passed a NULL pointer, this function returns FALSE
 *
 * See the documentation of notmuch_query_search_threads for example
 * code showing how to iterate over a notmuch_threads_t object.
 */
notmuch_bool_t
notmuch_threads_valid (notmuch_threads_t *threads);

/**
 * Get the current thread from 'threads' as a notmuch_thread_t.
 *
 * Note: The returned thread belongs to 'threads' and has a lifetime
 * identical to it (and the query to which it belongs).
 *
 * See the documentation of notmuch_query_search_threads for example
 * code showing how to iterate over a notmuch_threads_t object.
 *
 * If an out-of-memory situation occurs, this function will return
 * NULL.
 */
notmuch_thread_t *
notmuch_threads_get (notmuch_threads_t *threads);

/**
 * Move the 'threads' iterator to the next thread.
 *
 * If 'threads' is already pointing at the last thread then the
 * iterator will be moved to a point just beyond that last thread,
 * (where notmuch_threads_valid will return FALSE and
 * notmuch_threads_get will return NULL).
 *
 * See the documentation of notmuch_query_search_threads for example
 * code showing how to iterate over a notmuch_threads_t object.
 */
void
notmuch_threads_move_to_next (notmuch_threads_t *threads);

/**
 * Destroy a notmuch_threads_t object.
 *
 * It's not strictly necessary to call this function. All memory from
 * the notmuch_threads_t object will be reclaimed when the
 * containing query object is destroyed.
 */
void
notmuch_threads_destroy (notmuch_threads_t *threads);

/**
 * Return an estimate of the number of messages matching a search.
 *
 * This function performs a search and returns Xapian's best
 * guess as to number of matching messages.
 *
 * If a Xapian exception occurs, this function may return 0 (after
 * printing a message).
 */
unsigned
notmuch_query_count_messages (notmuch_query_t *query);

/**
 * Return the number of threads matching a search.
 *
 * This function performs a search and returns the number of unique thread IDs
 * in the matching messages. This is the same as number of threads matching a
 * search.
 *
 * Note that this is a significantly heavier operation than
 * notmuch_query_count_messages().
 *
 * If an error occurs, this function may return 0.
 */
unsigned
notmuch_query_count_threads (notmuch_query_t *query);

/**
 * Get the thread ID of 'thread'.
 *
 * The returned string belongs to 'thread' and as such, should not be
 * modified by the caller and will only be valid for as long as the
 * thread is valid, (which is until notmuch_thread_destroy or until
 * the query from which it derived is destroyed).
 */
const char *
notmuch_thread_get_thread_id (notmuch_thread_t *thread);

/**
 * Get the total number of messages in 'thread'.
 *
 * This count consists of all messages in the database belonging to
 * this thread. Contrast with notmuch_thread_get_matched_messages() .
 */
int
notmuch_thread_get_total_messages (notmuch_thread_t *thread);

/**
 * Get a notmuch_messages_t iterator for the top-level messages in
 * 'thread' in oldest-first order.
 *
 * This iterator will not necessarily iterate over all of the messages
 * in the thread. It will only iterate over the messages in the thread
 * which are not replies to other messages in the thread.
 *
 * The returned list will be destroyed when the thread is destroyed.
 */
notmuch_messages_t *
notmuch_thread_get_toplevel_messages (notmuch_thread_t *thread);

/**
 * Get a notmuch_thread_t iterator for all messages in 'thread' in
 * oldest-first order.
 *
 * The returned list will be destroyed when the thread is destroyed.
 */
notmuch_messages_t *
notmuch_thread_get_messages (notmuch_thread_t *thread);

/**
 * Get the number of messages in 'thread' that matched the search.
 *
 * This count includes only the messages in this thread that were
 * matched by the search from which the thread was created and were
 * not excluded by any exclude tags passed in with the query (see
 * notmuch_query_add_tag_exclude). Contrast with
 * notmuch_thread_get_total_messages() .
 */
int
notmuch_thread_get_matched_messages (notmuch_thread_t *thread);

/**
 * Get the authors of 'thread' as a UTF-8 string.
 *
 * The returned string is a comma-separated list of the names of the
 * authors of mail messages in the query results that belong to this
 * thread.
 *
 * The returned string belongs to 'thread' and as such, should not be
 * modified by the caller and will only be valid for as long as the
 * thread is valid, (which is until notmuch_thread_destroy or until
 * the query from which it derived is destroyed).
 */
const char *
notmuch_thread_get_authors (notmuch_thread_t *thread);

/**
 * Get the subject of 'thread' as a UTF-8 string.
 *
 * The subject is taken from the first message (according to the query
 * order---see notmuch_query_set_sort) in the query results that
 * belongs to this thread.
 *
 * The returned string belongs to 'thread' and as such, should not be
 * modified by the caller and will only be valid for as long as the
 * thread is valid, (which is until notmuch_thread_destroy or until
 * the query from which it derived is destroyed).
 */
const char *
notmuch_thread_get_subject (notmuch_thread_t *thread);

/**
 * Get the date of the oldest message in 'thread' as a time_t value.
 */
time_t
notmuch_thread_get_oldest_date (notmuch_thread_t *thread);

/**
 * Get the date of the newest message in 'thread' as a time_t value.
 */
time_t
notmuch_thread_get_newest_date (notmuch_thread_t *thread);

/**
 * Get the tags for 'thread', returning a notmuch_tags_t object which
 * can be used to iterate over all tags.
 *
 * Note: In the Notmuch database, tags are stored on individual
 * messages, not on threads. So the tags returned here will be all
 * tags of the messages which matched the search and which belong to
 * this thread.
 *
 * The tags object is owned by the thread and as such, will only be
 * valid for as long as the thread is valid, (for example, until
 * notmuch_thread_destroy or until the query from which it derived is
 * destroyed).
 *
 * Typical usage might be:
 *
 *     notmuch_thread_t *thread;
 *     notmuch_tags_t *tags;
 *     const char *tag;
 *
 *     thread = notmuch_threads_get (threads);
 *
 *     for (tags = notmuch_thread_get_tags (thread);
 *          notmuch_tags_valid (tags);
 *          notmuch_tags_move_to_next (tags))
 *     {
 *         tag = notmuch_tags_get (tags);
 *         ....
 *     }
 *
 *     notmuch_thread_destroy (thread);
 *
 * Note that there's no explicit destructor needed for the
 * notmuch_tags_t object. (For consistency, we do provide a
 * notmuch_tags_destroy function, but there's no good reason to call
 * it if the message is about to be destroyed).
 */
notmuch_tags_t *
notmuch_thread_get_tags (notmuch_thread_t *thread);

/**
 * Destroy a notmuch_thread_t object.
 */
void
notmuch_thread_destroy (notmuch_thread_t *thread);

/**
 * Is the given 'messages' iterator pointing at a valid message.
 *
 * When this function returns TRUE, notmuch_messages_get will return a
 * valid object. Whereas when this function returns FALSE,
 * notmuch_messages_get will return NULL.
 *
 * See the documentation of notmuch_query_search_messages for example
 * code showing how to iterate over a notmuch_messages_t object.
 */
notmuch_bool_t
notmuch_messages_valid (notmuch_messages_t *messages);

/**
 * Get the current message from 'messages' as a notmuch_message_t.
 *
 * Note: The returned message belongs to 'messages' and has a lifetime
 * identical to it (and the query to which it belongs).
 *
 * See the documentation of notmuch_query_search_messages for example
 * code showing how to iterate over a notmuch_messages_t object.
 *
 * If an out-of-memory situation occurs, this function will return
 * NULL.
 */
notmuch_message_t *
notmuch_messages_get (notmuch_messages_t *messages);

/**
 * Move the 'messages' iterator to the next message.
 *
 * If 'messages' is already pointing at the last message then the
 * iterator will be moved to a point just beyond that last message,
 * (where notmuch_messages_valid will return FALSE and
 * notmuch_messages_get will return NULL).
 *
 * See the documentation of notmuch_query_search_messages for example
 * code showing how to iterate over a notmuch_messages_t object.
 */
void
notmuch_messages_move_to_next (notmuch_messages_t *messages);

/**
 * Destroy a notmuch_messages_t object.
 *
 * It's not strictly necessary to call this function. All memory from
 * the notmuch_messages_t object will be reclaimed when the containing
 * query object is destroyed.
 */
void
notmuch_messages_destroy (notmuch_messages_t *messages);

/**
 * Return a list of tags from all messages.
 *
 * The resulting list is guaranteed not to contain duplicated tags.
 *
 * WARNING: You can no longer iterate over messages after calling this
 * function, because the iterator will point at the end of the list.
 * We do not have a function to reset the iterator yet and the only
 * way how you can iterate over the list again is to recreate the
 * message list.
 *
 * The function returns NULL on error.
 */
notmuch_tags_t *
notmuch_messages_collect_tags (notmuch_messages_t *messages);

/**
 * Get the message ID of 'message'.
 *
 * The returned string belongs to 'message' and as such, should not be
 * modified by the caller and will only be valid for as long as the
 * message is valid, (which is until the query from which it derived
 * is destroyed).
 *
 * This function will not return NULL since Notmuch ensures that every
 * message has a unique message ID, (Notmuch will generate an ID for a
 * message if the original file does not contain one).
 */
const char *
notmuch_message_get_message_id (notmuch_message_t *message);

/**
 * Get the thread ID of 'message'.
 *
 * The returned string belongs to 'message' and as such, should not be
 * modified by the caller and will only be valid for as long as the
 * message is valid, (for example, until the user calls
 * notmuch_message_destroy on 'message' or until a query from which it
 * derived is destroyed).
 *
 * This function will not return NULL since Notmuch ensures that every
 * message belongs to a single thread.
 */
const char *
notmuch_message_get_thread_id (notmuch_message_t *message);

/**
 * Get a notmuch_messages_t iterator for all of the replies to
 * 'message'.
 *
 * Note: This call only makes sense if 'message' was ultimately
 * obtained from a notmuch_thread_t object, (such as by coming
 * directly from the result of calling notmuch_thread_get_
 * toplevel_messages or by any number of subsequent
 * calls to notmuch_message_get_replies).
 *
 * If 'message' was obtained through some non-thread means, (such as
 * by a call to notmuch_query_search_messages), then this function
 * will return NULL.
 *
 * If there are no replies to 'message', this function will return
 * NULL. (Note that notmuch_messages_valid will accept that NULL
 * value as legitimate, and simply return FALSE for it.)
 */
notmuch_messages_t *
notmuch_message_get_replies (notmuch_message_t *message);

/**
 * Get a filename for the email corresponding to 'message'.
 *
 * The returned filename is an absolute filename, (the initial
 * component will match notmuch_database_get_path() ).
 *
 * The returned string belongs to the message so should not be
 * modified or freed by the caller (nor should it be referenced after
 * the message is destroyed).
 *
 * Note: If this message corresponds to multiple files in the mail
 * store, (that is, multiple files contain identical message IDs),
 * this function will arbitrarily return a single one of those
 * filenames. See notmuch_message_get_filenames for returning the
 * complete list of filenames.
 */
const char *
notmuch_message_get_filename (notmuch_message_t *message);

/**
 * Get all filenames for the email corresponding to 'message'.
 *
 * Returns a notmuch_filenames_t iterator listing all the filenames
 * associated with 'message'. These files may not have identical
 * content, but each will have the identical Message-ID.
 *
 * Each filename in the iterator is an absolute filename, (the initial
 * component will match notmuch_database_get_path() ).
 */
notmuch_filenames_t *
notmuch_message_get_filenames (notmuch_message_t *message);

/**
 * Message flags.
 */
typedef enum _notmuch_message_flag {
    NOTMUCH_MESSAGE_FLAG_MATCH,
    NOTMUCH_MESSAGE_FLAG_EXCLUDED
} notmuch_message_flag_t;

/**
 * Get a value of a flag for the email corresponding to 'message'.
 */
notmuch_bool_t
notmuch_message_get_flag (notmuch_message_t *message,
			  notmuch_message_flag_t flag);

/**
 * Set a value of a flag for the email corresponding to 'message'.
 */
void
notmuch_message_set_flag (notmuch_message_t *message,
			  notmuch_message_flag_t flag, notmuch_bool_t value);

/**
 * Get the date of 'message' as a time_t value.
 *
 * For the original textual representation of the Date header from the
 * message call notmuch_message_get_header() with a header value of
 * "date".
 */
time_t
notmuch_message_get_date  (notmuch_message_t *message);

/**
 * Get the value of the specified header from 'message' as a UTF-8 string.
 *
 * Common headers are stored in the database when the message is
 * indexed and will be returned from the database.  Other headers will
 * be read from the actual message file.
 *
 * The header name is case insensitive.
 *
 * The returned string belongs to the message so should not be
 * modified or freed by the caller (nor should it be referenced after
 * the message is destroyed).
 *
 * Returns an empty string ("") if the message does not contain a
 * header line matching 'header'. Returns NULL if any error occurs.
 */
const char *
notmuch_message_get_header (notmuch_message_t *message, const char *header);

/**
 * Get the tags for 'message', returning a notmuch_tags_t object which
 * can be used to iterate over all tags.
 *
 * The tags object is owned by the message and as such, will only be
 * valid for as long as the message is valid, (which is until the
 * query from which it derived is destroyed).
 *
 * Typical usage might be:
 *
 *     notmuch_message_t *message;
 *     notmuch_tags_t *tags;
 *     const char *tag;
 *
 *     message = notmuch_database_find_message (database, message_id);
 *
 *     for (tags = notmuch_message_get_tags (message);
 *          notmuch_tags_valid (tags);
 *          notmuch_tags_move_to_next (tags))
 *     {
 *         tag = notmuch_tags_get (tags);
 *         ....
 *     }
 *
 *     notmuch_message_destroy (message);
 *
 * Note that there's no explicit destructor needed for the
 * notmuch_tags_t object. (For consistency, we do provide a
 * notmuch_tags_destroy function, but there's no good reason to call
 * it if the message is about to be destroyed).
 */
notmuch_tags_t *
notmuch_message_get_tags (notmuch_message_t *message);

/**
 * The longest possible tag value.
 */
#define NOTMUCH_TAG_MAX 200

/**
 * Add a tag to the given message.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Tag successfully added to message
 *
 * NOTMUCH_STATUS_NULL_POINTER: The 'tag' argument is NULL
 *
 * NOTMUCH_STATUS_TAG_TOO_LONG: The length of 'tag' is too long
 *	(exceeds NOTMUCH_TAG_MAX)
 *
 * NOTMUCH_STATUS_READ_ONLY_DATABASE: Database was opened in read-only
 *	mode so message cannot be modified.
 */
notmuch_status_t
notmuch_message_add_tag (notmuch_message_t *message, const char *tag);

/**
 * Remove a tag from the given message.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Tag successfully removed from message
 *
 * NOTMUCH_STATUS_NULL_POINTER: The 'tag' argument is NULL
 *
 * NOTMUCH_STATUS_TAG_TOO_LONG: The length of 'tag' is too long
 *	(exceeds NOTMUCH_TAG_MAX)
 *
 * NOTMUCH_STATUS_READ_ONLY_DATABASE: Database was opened in read-only
 *	mode so message cannot be modified.
 */
notmuch_status_t
notmuch_message_remove_tag (notmuch_message_t *message, const char *tag);

/**
 * Remove all tags from the given message.
 *
 * See notmuch_message_freeze for an example showing how to safely
 * replace tag values.
 *
 * NOTMUCH_STATUS_READ_ONLY_DATABASE: Database was opened in read-only
 *	mode so message cannot be modified.
 */
notmuch_status_t
notmuch_message_remove_all_tags (notmuch_message_t *message);

/**
 * Add/remove tags according to maildir flags in the message filename(s).
 *
 * This function examines the filenames of 'message' for maildir
 * flags, and adds or removes tags on 'message' as follows when these
 * flags are present:
 *
 *	Flag	Action if present
 *	----	-----------------
 *	'D'	Adds the "draft" tag to the message
 *	'F'	Adds the "flagged" tag to the message
 *	'P'	Adds the "passed" tag to the message
 *	'R'	Adds the "replied" tag to the message
 *	'S'	Removes the "unread" tag from the message
 *
 * For each flag that is not present, the opposite action (add/remove)
 * is performed for the corresponding tags.
 *
 * Flags are identified as trailing components of the filename after a
 * sequence of ":2,".
 *
 * If there are multiple filenames associated with this message, the
 * flag is considered present if it appears in one or more
 * filenames. (That is, the flags from the multiple filenames are
 * combined with the logical OR operator.)
 *
 * A client can ensure that notmuch database tags remain synchronized
 * with maildir flags by calling this function after each call to
 * notmuch_database_add_message. See also
 * notmuch_message_tags_to_maildir_flags for synchronizing tag changes
 * back to maildir flags.
 */
notmuch_status_t
notmuch_message_maildir_flags_to_tags (notmuch_message_t *message);

/**
 * Rename message filename(s) to encode tags as maildir flags.
 *
 * Specifically, for each filename corresponding to this message:
 *
 * If the filename is not in a maildir directory, do nothing.  (A
 * maildir directory is determined as a directory named "new" or
 * "cur".) Similarly, if the filename has invalid maildir info,
 * (repeated or outof-ASCII-order flag characters after ":2,"), then
 * do nothing.
 *
 * If the filename is in a maildir directory, rename the file so that
 * its filename ends with the sequence ":2," followed by zero or more
 * of the following single-character flags (in ASCII order):
 *
 *   'D' iff the message has the "draft" tag
 *   'F' iff the message has the "flagged" tag
 *   'P' iff the message has the "passed" tag
 *   'R' iff the message has the "replied" tag
 *   'S' iff the message does not have the "unread" tag
 *
 * Any existing flags unmentioned in the list above will be preserved
 * in the renaming.
 *
 * Also, if this filename is in a directory named "new", rename it to
 * be within the neighboring directory named "cur".
 *
 * A client can ensure that maildir filename flags remain synchronized
 * with notmuch database tags by calling this function after changing
 * tags, (after calls to notmuch_message_add_tag,
 * notmuch_message_remove_tag, or notmuch_message_freeze/
 * notmuch_message_thaw). See also notmuch_message_maildir_flags_to_tags
 * for synchronizing maildir flag changes back to tags.
 */
notmuch_status_t
notmuch_message_tags_to_maildir_flags (notmuch_message_t *message);

/**
 * Freeze the current state of 'message' within the database.
 *
 * This means that changes to the message state, (via
 * notmuch_message_add_tag, notmuch_message_remove_tag, and
 * notmuch_message_remove_all_tags), will not be committed to the
 * database until the message is thawed with notmuch_message_thaw.
 *
 * Multiple calls to freeze/thaw are valid and these calls will
 * "stack". That is there must be as many calls to thaw as to freeze
 * before a message is actually thawed.
 *
 * The ability to do freeze/thaw allows for safe transactions to
 * change tag values. For example, explicitly setting a message to
 * have a given set of tags might look like this:
 *
 *    notmuch_message_freeze (message);
 *
 *    notmuch_message_remove_all_tags (message);
 *
 *    for (i = 0; i < NUM_TAGS; i++)
 *        notmuch_message_add_tag (message, tags[i]);
 *
 *    notmuch_message_thaw (message);
 *
 * With freeze/thaw used like this, the message in the database is
 * guaranteed to have either the full set of original tag values, or
 * the full set of new tag values, but nothing in between.
 *
 * Imagine the example above without freeze/thaw and the operation
 * somehow getting interrupted. This could result in the message being
 * left with no tags if the interruption happened after
 * notmuch_message_remove_all_tags but before notmuch_message_add_tag.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Message successfully frozen.
 *
 * NOTMUCH_STATUS_READ_ONLY_DATABASE: Database was opened in read-only
 *	mode so message cannot be modified.
 */
notmuch_status_t
notmuch_message_freeze (notmuch_message_t *message);

/**
 * Thaw the current 'message', synchronizing any changes that may have
 * occurred while 'message' was frozen into the notmuch database.
 *
 * See notmuch_message_freeze for an example of how to use this
 * function to safely provide tag changes.
 *
 * Multiple calls to freeze/thaw are valid and these calls with
 * "stack". That is there must be as many calls to thaw as to freeze
 * before a message is actually thawed.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: Message successfully thawed, (or at least
 *	its frozen count has successfully been reduced by 1).
 *
 * NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW: An attempt was made to thaw
 *	an unfrozen message. That is, there have been an unbalanced
 *	number of calls to notmuch_message_freeze and
 *	notmuch_message_thaw.
 */
notmuch_status_t
notmuch_message_thaw (notmuch_message_t *message);

/**
 * Destroy a notmuch_message_t object.
 *
 * It can be useful to call this function in the case of a single
 * query object with many messages in the result, (such as iterating
 * over the entire database). Otherwise, it's fine to never call this
 * function and there will still be no memory leaks. (The memory from
 * the messages get reclaimed when the containing query is destroyed.)
 */
void
notmuch_message_destroy (notmuch_message_t *message);

/**
 * Is the given 'tags' iterator pointing at a valid tag.
 *
 * When this function returns TRUE, notmuch_tags_get will return a
 * valid string. Whereas when this function returns FALSE,
 * notmuch_tags_get will return NULL.
 *
 * See the documentation of notmuch_message_get_tags for example code
 * showing how to iterate over a notmuch_tags_t object.
 */
notmuch_bool_t
notmuch_tags_valid (notmuch_tags_t *tags);

/**
 * Get the current tag from 'tags' as a string.
 *
 * Note: The returned string belongs to 'tags' and has a lifetime
 * identical to it (and the query to which it ultimately belongs).
 *
 * See the documentation of notmuch_message_get_tags for example code
 * showing how to iterate over a notmuch_tags_t object.
 */
const char *
notmuch_tags_get (notmuch_tags_t *tags);

/**
 * Move the 'tags' iterator to the next tag.
 *
 * If 'tags' is already pointing at the last tag then the iterator
 * will be moved to a point just beyond that last tag, (where
 * notmuch_tags_valid will return FALSE and notmuch_tags_get will
 * return NULL).
 *
 * See the documentation of notmuch_message_get_tags for example code
 * showing how to iterate over a notmuch_tags_t object.
 */
void
notmuch_tags_move_to_next (notmuch_tags_t *tags);

/**
 * Destroy a notmuch_tags_t object.
 *
 * It's not strictly necessary to call this function. All memory from
 * the notmuch_tags_t object will be reclaimed when the containing
 * message or query objects are destroyed.
 */
void
notmuch_tags_destroy (notmuch_tags_t *tags);

/**
 * Store an mtime within the database for 'directory'.
 *
 * The 'directory' should be an object retrieved from the database
 * with notmuch_database_get_directory for a particular path.
 *
 * The intention is for the caller to use the mtime to allow efficient
 * identification of new messages to be added to the database. The
 * recommended usage is as follows:
 *
 *   o Read the mtime of a directory from the filesystem
 *
 *   o Call add_message for all mail files in the directory
 *
 *   o Call notmuch_directory_set_mtime with the mtime read from the
 *     filesystem.
 *
 * Then, when wanting to check for updates to the directory in the
 * future, the client can call notmuch_directory_get_mtime and know
 * that it only needs to add files if the mtime of the directory and
 * files are newer than the stored timestamp.
 *
 * Note: The notmuch_directory_get_mtime function does not allow the
 * caller to distinguish a timestamp of 0 from a non-existent
 * timestamp. So don't store a timestamp of 0 unless you are
 * comfortable with that.
 *
 * Return value:
 *
 * NOTMUCH_STATUS_SUCCESS: mtime successfully stored in database.
 *
 * NOTMUCH_STATUS_XAPIAN_EXCEPTION: A Xapian exception
 *	occurred, mtime not stored.
 *
 * NOTMUCH_STATUS_READ_ONLY_DATABASE: Database was opened in read-only
 *	mode so directory mtime cannot be modified.
 */
notmuch_status_t
notmuch_directory_set_mtime (notmuch_directory_t *directory,
			     time_t mtime);

/**
 * Get the mtime of a directory, (as previously stored with
 * notmuch_directory_set_mtime).
 *
 * Returns 0 if no mtime has previously been stored for this
 * directory.
 */
time_t
notmuch_directory_get_mtime (notmuch_directory_t *directory);

/**
 * Get a notmuch_filenames_t iterator listing all the filenames of
 * messages in the database within the given directory.
 *
 * The returned filenames will be the basename-entries only (not
 * complete paths).
 */
notmuch_filenames_t *
notmuch_directory_get_child_files (notmuch_directory_t *directory);

/**
 * Get a notmuch_filenams_t iterator listing all the filenames of
 * sub-directories in the database within the given directory.
 *
 * The returned filenames will be the basename-entries only (not
 * complete paths).
 */
notmuch_filenames_t *
notmuch_directory_get_child_directories (notmuch_directory_t *directory);

/**
 * Destroy a notmuch_directory_t object.
 */
void
notmuch_directory_destroy (notmuch_directory_t *directory);

/**
 * Is the given 'filenames' iterator pointing at a valid filename.
 *
 * When this function returns TRUE, notmuch_filenames_get will return
 * a valid string. Whereas when this function returns FALSE,
 * notmuch_filenames_get will return NULL.
 *
 * It is acceptable to pass NULL for 'filenames', in which case this
 * function will always return FALSE.
 */
notmuch_bool_t
notmuch_filenames_valid (notmuch_filenames_t *filenames);

/**
 * Get the current filename from 'filenames' as a string.
 *
 * Note: The returned string belongs to 'filenames' and has a lifetime
 * identical to it (and the directory to which it ultimately belongs).
 *
 * It is acceptable to pass NULL for 'filenames', in which case this
 * function will always return NULL.
 */
const char *
notmuch_filenames_get (notmuch_filenames_t *filenames);

/**
 * Move the 'filenames' iterator to the next filename.
 *
 * If 'filenames' is already pointing at the last filename then the
 * iterator will be moved to a point just beyond that last filename,
 * (where notmuch_filenames_valid will return FALSE and
 * notmuch_filenames_get will return NULL).
 *
 * It is acceptable to pass NULL for 'filenames', in which case this
 * function will do nothing.
 */
void
notmuch_filenames_move_to_next (notmuch_filenames_t *filenames);

/**
 * Destroy a notmuch_filenames_t object.
 *
 * It's not strictly necessary to call this function. All memory from
 * the notmuch_filenames_t object will be reclaimed when the
 * containing directory object is destroyed.
 *
 * It is acceptable to pass NULL for 'filenames', in which case this
 * function will do nothing.
 */
void
notmuch_filenames_destroy (notmuch_filenames_t *filenames);

/* @} */

NOTMUCH_END_DECLS

#endif

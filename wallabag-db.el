;;; wallabag-db.el --- Emacs wallabag client db realted operations -*- lexical-binding: t; -*-

(require 'emacsql)
(require 'emacsql-sqlite)

(defcustom wallabag-db-connector (if (and (progn
                                            (require 'emacsql-sqlite-builtin nil t)
                                            (functionp 'emacsql-sqlite-builtin))
                                          (functionp 'sqlite-open))
                                     'sqlite-builtin
                                   'sqlite)
  "The database connector used by wallabag.
This must be set before `wallabag' is loaded.  To use an alternative
connector you must install the respective package explicitly.
The default is `sqlite', which uses the `emacsql-sqlite' library
that is being maintained in the same repository as `emacsql'
itself.
If you are using Emacs 29, then the recommended connector is
`sqlite-builtin', which uses the new builtin support for SQLite.
You need to install the `emacsql-sqlite-builtin' package to use
this connector.
If you are using an older Emacs release, then the recommended
connector is `sqlite-module', which uses the module provided by
the `sqlite3' package.  This is very similar to the previous
connector and the built-in support in Emacs 29 derives from this
module.  You need to install the `emacsql-sqlite-module' package
to use this connector.
For the time being `libsqlite3' is still supported.  Do not use
this, it is an older version of the `sqlite-module' connector
from before the connector and the package were renamed.
For the time being `sqlite3' is also supported.  Do not use this.
This uses the third-party `emacsql-sqlite3' package, which uses
the official `sqlite3' cli tool, which is not intended
to be used like this.  See https://nullprogram.com/blog/2014/02/06/."
  :group 'wallabag
  :type '(choice (const sqlite)
          (const sqlite-builtin)
          (const sqlite-module)
          (const :tag "libsqlite3 (OBSOLETE)" libsqlite3)
          (const :tag "sqlite3 (BROKEN)" sqlite3)))

(defcustom wallabag-db-file
  (expand-file-name (concat user-emacs-directory ".cache/wallabag.sqlite"))
  "Wallabag sqlite file for all entries."
  :group 'wallabag
  :type 'file)

(defvar wallabag-db-connection nil
  "The EmacSQL database connection.")

(defconst wallabag-db-version 1)

(defvar wallabag-db-newp nil)

(defun wallabag-db--conn-fn ()
  "Return the function for creating the database connection."
  (cl-case wallabag-db-connector
    (sqlite
     (progn
       (require 'emacsql-sqlite)
       #'emacsql-sqlite))
    (sqlite-builtin
     (progn
       (require 'emacsql-sqlite-builtin)
       #'emacsql-sqlite-builtin))
    (sqlite-module
     (progn
       (require 'emacsql-sqlite-module)
       #'emacsql-sqlite-module))
    (libsqlite3
     (progn
       (require 'emacsql-libsqlite3)
       #'emacsql-libsqlite3))
    (sqlite3
     (progn
       (require 'emacsql-sqlite3)
       #'emacsql-sqlite3))))

(defun wallabag-db ()
  "Connect or create database."
  (unless (and wallabag-db-connection (emacsql-live-p wallabag-db-connection))
    (unless (file-exists-p (concat user-emacs-directory ".cache/"))
      (make-directory (concat user-emacs-directory ".cache/")))
    (setq wallabag-db-connection (funcall (wallabag-db--conn-fn) wallabag-db-file))

    ;; create items table
    (emacsql wallabag-db-connection [:create-table :if-not-exists items ([tag
                                                                          is_archived
                                                                          is_starred
                                                                          user_name
                                                                          user_email
                                                                          user_id
                                                                          tags
                                                                          is_public
                                                                          (id integer :primary-key)
                                                                          uid
                                                                          title
                                                                          url
                                                                          hashed_url
                                                                          origin_url
                                                                          given_url
                                                                          hashed_given_url
                                                                          archived_at
                                                                          content
                                                                          created_at
                                                                          updated_at
                                                                          published_at
                                                                          published_by
                                                                          starred_at
                                                                          annotations
                                                                          mimetype
                                                                          language
                                                                          reading_time
                                                                          domain_name
                                                                          preview_picture
                                                                          http_status
                                                                          headers
                                                                          _links])])
    ;; create version table
    (emacsql wallabag-db-connection [:create-table :if-not-exists version ([user-version])])

    (let* ((db wallabag-db-connection)
           (version (wallabag-db-maybe-update db wallabag-db-version)))
      (cond
       ((> version wallabag-db-version)
        (emacsql-close db)
        (user-error
         "The wallabag database was created with a newer wallabag version.  %s"
         "You need to update the Anki package."))
       ((< version wallabag-db-version)
        (emacsql-close db)
        (error "BUG: The wallabag database scheme changed %s"
               "and there is no upgrade path")))))
  wallabag-db-connection)

(defun wallabag-db-maybe-update (db version)
  (if (emacsql-live-p db)
      (cond ((eq version 1)
             (wallabag-db-set-version db (setq version 1))
             (message "Wallabag database is version 1...done"))
            ((eq version 2)
             (message "Upgrading Wallabag database from version 2 to 3...")
             (wallabag-db-set-version db (setq version 3))
             (message "Upgrading Wallabag database from version 2 to 3...done"))
            (t (setq version wallabag-db-version))))
  version)

(defun wallabag-db-get-version (db)
  (caar (emacsql db [:select user-version :from version])))

(defun wallabag-db-set-version (db dbv)
  "Insert user-version if not exists."
  (cl-assert (integerp dbv))
  (if (wallabag-db-get-version db)
      (emacsql db `[:update version :set  (= user-version ,dbv)])
    (emacsql db `[:insert :into version :values ([(,@wallabag-db-version)])])))


(defun wallabag-db-sql (sql &rest args)
  (if (stringp sql)
      (emacsql (wallabag-db) (apply #'format sql args))
    (apply #'emacsql (wallabag-db) sql args)))

;;; database operation

;; select
(defun wallabag-db-select (&rest properties)
  (let ((candidates)
        (sql (plist-get properties :sql))
        (id (plist-get properties :id))
        (title (plist-get properties :title)))
    (setq candidates (mapcar (lambda(x)
                               (cl-pairlis
                                '(tag
                                  is_archived
                                  is_starred
                                  user_name
                                  user_email
                                  user_id
                                  tags
                                  is_public
                                  id
                                  uid
                                  title
                                  url
                                  hashed_url
                                  origin_url
                                  given_url
                                  hashed_given_url
                                  archived_at
                                  content
                                  created_at
                                  updated_at
                                  published_at
                                  published_by
                                  starred_at
                                  annotations
                                  mimetype
                                  language
                                  reading_time
                                  domain_name
                                  preview_picture
                                  http_status
                                  headers
                                  _links)
                                x))
                             (if id
                                 (wallabag-db-sql `[:select * :from items :where (= id ,id)])
                               (if title
                                   (wallabag-db-sql `[:select * :from items :where (= title ,title)])
                                 (wallabag-db-sql (or sql [:select * :from items] ))))) )

    (if candidates
        candidates
      ;; (message "No items in wallabag database, try to update with 'u'.")
      (setq wallabag-db-newp t)
      nil)))

;; insert
(defun wallabag-db-insert (entries)
  (let ((entries
         (cl-loop for entry in entries collect
                  (cl-map 'array #'identity (mapcar 'cdr entry)))))
    (wallabag-db-sql `[:insert :or :ignore :into items
                       :values ,entries])))
;; delete
(defun wallabag-db-delete (ids)
  (cond ((vectorp ids)
         (wallabag-db-sql `[:delete :from items
                            :where (in id ,ids)]) )
        ((numberp ids) (wallabag-db-sql `[:delete :from items
                                          :where (= id ,ids)]))
        (t nil)))

;; update
(defmacro wallabag-db-update (field)
  `(defun ,(intern (format "wallabag-db-update-%s" field)) (id new)
     (wallabag-db-sql (vector ':update 'items
                              ':set (list '= ',(intern field) (vector new ))
                              ':where (list '= 'id id) ))))

(wallabag-db-update "title")
(wallabag-db-update "tag")
(wallabag-db-update "tags")
(wallabag-db-update "content")
(wallabag-db-update "is_archived")
(wallabag-db-update "is_starred")
(wallabag-db-update "origin_url")

(provide 'wallabag-db)

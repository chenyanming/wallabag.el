;;; wallabag-db.el --- Emacs wallabag client db realted operations -*- lexical-binding: t; -*-

(require 'emacsql)
(require 'emacsql-sqlite)

(defcustom wallabag-db-file
  (expand-file-name (concat user-emacs-directory ".cache/wallabag.sqlite"))
  "Wallabag sqlite file for all entries."
  :group 'wallabag
  :type 'file)

(defvar wallabag-db-connection nil
  "The EmacSQL database connection.")

(defconst wallabag-db-version 1)

(defun wallabag-db ()
  "Connect or create database."
  (unless (and wallabag-db-connection (emacsql-live-p wallabag-db-connection))
    (unless (file-exists-p (concat user-emacs-directory ".cache/"))
      (make-directory (concat user-emacs-directory ".cache/")))
    (setq wallabag-db-connection (emacsql-sqlite wallabag-db-file))

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
         "The anki database was created with a newer Anki version.  %s"
         "You need to update the Anki package."))
       ((< version wallabag-db-version)
        (emacsql-close db)
        (error "BUG: The Anki database scheme changed %s"
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
(defun wallabag-db-select (&optional id)
  (let (candidates)
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
                               (wallabag-db-sql [:select * :from items]))) )

    (if candidates
        candidates
      (message "No items in wallabag database, try to update with 'u'.")
      nil)))

;; insert
(defun wallabag-db-insert (entries)
  (let ((entries
         (cl-loop for entry in entries collect
                  (map 'array #'identity (mapcar 'cdr entry)))))
    (wallabag-db-sql `[:insert :or :ignore :into items
                    :values ,entries])))
;; delete
(defun wallabag-db-delete (id)
  (wallabag-db-sql `[:delete :from items
                  :where (= id ,id)]))

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
(provide 'wallabag-db)

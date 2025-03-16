;;; wallabag-db.el --- Emacs wallabag client db realted operations -*- lexical-binding: t; -*-

;; Author: Damon Chan <elecming@gmail.com>
;; Maintainer: Damon Chan <elecming@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'emacsql)
;; REVIEW: is this require needed?
;; emacsql-sqlite provides a common interface to an emacsql SQLite backend (e.g. emacs-sqlite-builtin)
;; not to be confused with a backend itself named emacsql-sqlite that existed in emacsql < 4.0.
(require 'emacsql-sqlite)

(defcustom wallabag-db-file
  (expand-file-name (concat user-emacs-directory ".cache/wallabag.sqlite"))
  "Wallabag sqlite file for all entries."
  :group 'wallabag
  :type 'file)

(defvar wallabag-db-connection nil
  "The EmacSQL database connection.")

(defconst wallabag-db-version 1)

(defvar wallabag-db-newp nil)

(defun wallabag-db ()
  "Connect or create database."
  (unless (and wallabag-db-connection (emacsql-live-p wallabag-db-connection))
    (unless (file-exists-p (concat user-emacs-directory ".cache/"))
      (make-directory (concat user-emacs-directory ".cache/")))
    (setq wallabag-db-connection (emacsql-sqlite-open wallabag-db-file))

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
        (title (plist-get properties :title))
        (url (plist-get properties :url))
        (next-entry (plist-get properties :next-entry))
        (previous-entry (plist-get properties :previous-entry)))
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
                             (cond
                              (id (wallabag-db-sql `[:select * :from items :where (= id ,id)]))
                              (title (wallabag-db-sql `[:select * :from items :where (= title ,title)]))
                              (url (wallabag-db-sql `[:select * :from items :where (= url ,url)]))
                              (previous-entry (wallabag-db-sql `[:select * :from items :where (> id ,previous-entry) :limit 1]) )
                              (next-entry (wallabag-db-sql `[:select * :from items :where (< id ,next-entry) :order-by (desc id) :limit 1]) )
                              (t (wallabag-db-sql (or sql [:select * :from items]))))))
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

;;; wallabag.el --- Emacs wallabag client -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/wallabag.el
;; Keywords: tools
;; Created: 13 April 2021
;; Version: 1.1.0
;; Package-Requires: ((emacs "25.1") (request "0.3.3") (emacsql "3.0.0"))

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

;; Emacs wallabag client.

;;; Code:

(require 'wallabag-faces)
(require 'wallabag-db)
(require 'wallabag-util)
(require 'wallabag-org)
(require 'cl-lib)
(require 'request)
(require 'shr)
(require 'browse-url)
(require 's)
(ignore-errors
  (require 'evil)
  (require 'ivy))
(require 'consult nil t)

(declare-function evil-define-key "evil")
(declare-function ivy-read "ivy")

(defcustom wallabag-host ""
  "Wallabag host."
  :group 'wallabag
  :type 'string)

(defvar wallabag-token nil)

(defcustom wallabag-username ""
  "Wallabag username."
  :group 'wallabag
  :type 'string)

(defcustom wallabag-password ""
  "Wallabag password."
  :group 'wallabag
  :type 'string)

(defcustom wallabag-clientid ""
  "Wallabag clientid."
  :group 'wallabag
  :type 'string)

(defcustom wallabag-secret ""
  "Wallabag secret."
  :group 'wallabag
  :type 'string)

(defcustom wallabag-json-file (expand-file-name (concat user-emacs-directory ".cache/wallabag.json"))
  "TODO: Wallabag json file for all entries. Not Used now."
  :group 'wallabag
  :type 'string)

(defcustom wallabag-download-dir "~/Downloads"
  "Wallabag download directory."
  :group 'wallabag
  :type 'directory)

(defcustom wallabag-search-filter ""
  "Query string filtering shown entries."
  :group 'wallabag
  :type 'string)

(defcustom wallabag-show-entry-switch #'switch-to-buffer-other-window
  "Function used to display the calibre entry buffer."
  :group 'wallabag
  :type '(choice (function-item switch-to-buffer-other-window)
                 (function-item switch-to-buffer)
                 (function-item pop-to-buffer)
                 function))

(defcustom wallabag-search-title-min-width 16
  "Minimum column width for titles in the wallabag-search buffer."
  :group 'wallabag
  :type 'integer)

(defcustom wallabag-search-title-max-width (window-width (selected-window))
  "Maximum column width for titles in the wallabag-search buffer."
  :group 'wallabag
  :type 'integer)

(defcustom wallabag-search-content-max-width 200
  "Maximum width for content in the wallabag-search buffer."
  :group 'wallabag
  :type 'integer)

(defcustom wallabag-search-trailing-width 50
  "Space reserved for displaying the feed and tag information."
  :group 'wallabag
  :type 'integer)

(defcustom wallabag-show-sidebar nil
  "Set t to show sidebar when enter *wallabag-search*."
  :group 'wallabag
  :type 'boolean)


(define-obsolete-variable-alias 'wallabag-number-of-entries-to-be-retrieved
  'wallabag-number-of-entries-to-be-synchronized "wallabag 1.1.0")

(defcustom wallabag-number-of-entries-to-be-synchronized -1
  "When runs `wallabag-request-and-synchronize-entries', first it will retrieve all new entries and insert to local database.
Then it will call `wallabag-request-and-delete-entries' and check
`wallabag-number-of-entries-to-be-synchronized' entries. The
entries do not exist in server will be deleted.

If -1, all entries will be checked.
If set to N (N > 0), N entries will be checked."
  :group 'wallabag
  :type 'integer)

(defcustom wallabag-starred-icon "★"
  "The starred icon."
  :group 'wallabag
  :type 'string)


(defcustom wallabag-render-html-function 'wallabag-render-html
  "Function used to render HTML.
It's called without arguments with a buffer containing HTML and
should change it to contain the rendered version of it."
  :type 'function
  :group 'wallabag)

(defcustom wallabag-pre-html-render-hook nil
  "Hook run before `wallabag-render-html'."
  :type 'hook
  :group 'wallabag)

(defcustom wallabag-post-html-render-hook nil
  "Hook run after `wallabag-render-html'."
  :type 'hook
  :group 'wallabag)

(defcustom wallabag-after-render-hook nil
  "A hook called after wallabag has finished rendering the buffer."
  :group 'wallabag
  :type 'hook)

(defcustom wallabag-browser-function 'browse-url
  "Browser function used when opening wallabag entry."
  :type browse-url--browser-defcustom-type)

(defvar wallabag-search-filter-active nil
  "When non-nil, wallabag is currently reading a filter from the minibuffer.
When live editing the filter, it is bound to :live.")

(defvar wallabag-search-header-function #'wallabag-search-header
  "Function that returns the string to be used for the wallabag search header.")

(defvar wallabag-search-print-entry-function #'wallabag-search-print-entry--default
  "Function to print entries into the *wallabag-search* buffer.")

(defvar wallabag-all-tags nil)

(defvar wallabag-appname nil)
(defvar wallabag-version nil)
(defvar wallabag-allowed-registration nil)

(defvar wallabag-user-id nil)
(defvar wallabag-user-email nil)
(defvar wallabag-user-created-at nil)
(defvar wallabag-user-updated-at nil)

(defvar wallabag-retrieving-p nil)

(defvar wallabag-live-filteringp nil)
(defvar wallabag-group-filteringp nil)

(defcustom wallabag-css-file
  (concat (file-name-directory load-file-name) "default.css")
  "Wallabag css file for styling the entry when calls
`wallabag-browse-with-external-browser.'"
  :group 'wallabag
  :type 'file)

(defconst wallabag-field-mapping '(("title" . "title")
                                   ("tags" . "tags")
                                   ("archive" . "is_archived")
                                   ("starred" . "is_starred")
                                   ("content" . "content")
                                   ("language" . "language")
                                   ("preview_picture" . "preview_picture")
                                   ("published_at" . "published_at")
                                   ("authors" . "published_by")
                                   ("public" . "is_public")
                                   ("origin_url" . "origin_url")))
;;; requests

(defun wallabag-request-token ()
  "Request wallbag token."
  (interactive)
  (let ((host wallabag-host)
        (username wallabag-username)
        (password wallabag-password)
        (clientid wallabag-clientid)
        (secret wallabag-secret)
        token)
    (request (format "%s/oauth/v2/token" host)
      :parser 'json-read
      :params `(("username" . ,username ) ( "password" . ,password ) ( "client_id" . ,clientid ) ( "client_secret" . ,secret ) ( "grant_type" . "password" ) )
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                 ("Content-Type" . "application/json"))
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq token (assoc-default 'access_token data) ))))
    (setq wallabag-token token)))

(defun wallabag-request-server-info ()
  "Request the wallabag server info."
  (interactive)
  (let* ((host wallabag-host)
         (token (or wallabag-token (wallabag-request-token))))
    (request (format "%s/api/info.json" host)
      :parser 'json-read
      :params `(("access_token" . ,token))
      :headers '(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Wallaget request error: %S" error-thrown)))
      :status-code '((401 . (lambda (&rest _)
                              (message "Authenticating...")
                              (wallabag-request-token)
                              (funcall 'wallabag-request-server-info))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq wallabag-appname (assoc-default 'appname data))
                  (setq wallabag-version (alist-get 'version data))
                  (setq wallabag-allowed-registration (alist-get 'allowed_registration data))
                  (message "Request Server Info Done."))))))

(defun wallabag-request-user-info ()
  "Request the wallabag user info."
  (interactive)
  (let* ((host wallabag-host)
         (token (or wallabag-token (wallabag-request-token))))
    (request (format "%s/api/user.json" host)
      :parser 'json-read
      :params `(("access_token" . ,token))
      :headers '(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Wallaget request error: %S" error-thrown)))
      :status-code '((401 . (lambda (&rest _)
                              (message "Authenticating...")
                              (wallabag-request-token)
                              (funcall 'wallabag-request-user-info))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq wallabag-user-id (assoc-default 'id data))
                  (setq wallabag-user-email (assoc-default 'email data))
                  (setq wallabag-user-created-at (assoc-default 'created_at data))
                  (setq wallabag-user-updated-at (assoc-default 'updated_at data))
                  (message "Request User Info Done."))))))

(define-obsolete-function-alias 'wallabag-request-entries
  'wallabag-request-and-synchronize-entries "wallabag 1.1.0")

(defun wallabag-request-new-entries ()
  "Request one dummy entry from server and compare with latest one entry in database if it has new entries or not.
If new entries are found, retrive the new entries and update the
database, and delete the latest entires if they have been deleted
in server."
  (interactive)
  (setq wallabag-retrieving-p "Updating...")
  (let ((host wallabag-host)
        (token (or wallabag-token (wallabag-request-token)))
        (sort "created")
        (order "desc")
        (page 1))
    (request (format "%s/api/entries.json" host)
      :parser 'buffer-string
      :params `(("sort" . ,sort)
                ("order" . ,order)
                ("page" . ,page)
                ("perPage" . 1)
                ("access_token" . ,token))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                 ("Content-Type" . "application/json"))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Wallaget request error: %S" error-thrown)))
      :status-code '((401 . (lambda (&rest _)
                              (message "Authenticating...")
                              (wallabag-request-token)
                              (funcall 'wallabag-request-new-entries))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq entries (append (wallabag-parse-json (json-read-from-string data)) nil))
                  (let* ((latest-id (alist-get 'id (car entries)))
                         (max-id (or (caar (wallabag-db-sql `[:select id :from items :order :by id :desc :limit 1])) 0))
                         (number-of-retrieved (- latest-id max-id)))
                    (cond
                     ((= number-of-retrieved 0)
                      (message "No New Entries")
                      (setq wallabag-retrieving-p nil))
                     ((< number-of-retrieved 0) ;; the actual number may be less than (abs number-of-retrieved)
                      (wallabag-request-and-delete-entries (abs number-of-retrieved)))
                     (t
                      ;; (message "Found there may have %s new articles." number-of-retrieved)
                      (wallabag-request-and-insert-entries number-of-retrieved)))))))))

(defun wallabag-request-and-insert-entries (num-entries &optional callback args page)
  "Request NUM-ENTRIES entries and insert them to database if request succeeds.

Call CALLBACK with ARGS.

By default retrieval starts with the first page of results. With
non-nil integer PAGE retrieval starts at this page."
  (if (<= num-entries 0)
      (message "No more entries to retrive.")
    (let* ((host wallabag-host)
           (token (or wallabag-token (wallabag-request-token)))
           (sort "created")
           (order "desc")
           (perpage (min num-entries 30))
           (page (or page 1))
           current
           position)
      (request (format "%s/api/entries.json" host)
        :parser 'buffer-string
        :params `(("sort" . ,sort)
                  ("order" . ,order)
                  ("page" . ,page)
                  ("perPage" . ,perpage)
                  ("access_token" . ,token))
        :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                   ("Content-Type" . "application/json"))
        :error
        (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                       (message "Wallaget request error: %S" error-thrown)
                       (setq wallabag-retrieving-p nil)))
        :status-code '((401 . (lambda (&rest _)
                                (message "Authenticating...")
                                (wallabag-request-token)
                                (funcall 'wallabag-request-and-insert-entries num-entries callback args page))))
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    ;; save the original string
                    ;; (with-temp-file wallabag-json-file
                    ;;   (insert data))
                    (setq entries (append (wallabag-parse-json (json-read-from-string data)) nil))
                    (setq entries (cl-loop for entry in entries collect
                                           (progn
                                             ;; push a new tag column into it
                                             (push
                                              (cons
                                               'tag
                                               (wallabag-convert-tags-to-tag entry)) entry)
                                             ;; return entry
                                             entry)))

                    ;; insert new entries retried from wallabag server
                    (wallabag-db-insert entries)
                    (setq wallabag-db-newp nil)
                    (message "Retrived the latest %s articles." (length entries))

                    ;; refresh dashboard
                    (with-silent-modifications
                      (wallabag-request-tags)
                      (if (buffer-live-p (get-buffer "*wallabag-search*"))
                          (with-current-buffer (get-buffer "*wallabag-search*")
                            (setq wallabag-search-filter "")
                            (setq current (point))
                            (setq position (window-start))
                            (erase-buffer)
                            (wallabag-search-update-buffer)
                            (wallabag-search-mode)
                            (set-window-start (selected-window) position)
                            (goto-char current)) ))

                    ;; indicate the retrieving is finished, and update the header
                    (setq wallabag-retrieving-p nil)

                    ;; call the callback
                    (if callback
                        (funcall callback args))

                    (run-with-idle-timer
                     4 nil
                     #'wallabag-request-and-insert-entries
                     (max 0 (- num-entries perpage)) callback args (1+ page))))))))

(defun wallabag-request-and-synchronize-entries ()
  "Request and synchronize wallabag server.
1. Request the new entries and insert to local database.
2. Verify `wallabag-number-of-entries-to-be-synchronized' entries, entries do not exist in server will be deleted."
  (interactive)
  (setq wallabag-retrieving-p "Updating...")
  (let ((host wallabag-host)
        (token (or wallabag-token (wallabag-request-token)))
        (sort "created")
        (order "desc")
        (page 1))
    (request (format "%s/api/entries.json" host)
      :parser 'buffer-string
      :params `(("sort" . ,sort)
                ("order" . ,order)
                ("page" . ,page)
                ("perPage" . 1)
                ("access_token" . ,token))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                 ("Content-Type" . "application/json"))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Wallaget request error: %S" error-thrown)))
      :status-code '((401 . (lambda (&rest _)
                              (message "Authenticating...")
                              (wallabag-request-token)
                              (funcall 'wallabag-request-and-synchronize-entries))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq entries (append (wallabag-parse-json (json-read-from-string data)) nil))
                  (setq total (wallabag-get-total (json-read-from-string data)))
                  (let* ((latest-id (alist-get 'id (car entries)))
                         (max-id (or (caar (wallabag-db-sql `[:select id :from items :order :by id :desc :limit 1])) 0))
                         (number-of-retrieved (- latest-id max-id)))
                    (cond
                     ((= number-of-retrieved 0)
                      ;; need to add one
                      (message "No New Entries in server, verifing %s entries in local database..." total)
                      (wallabag-request-and-delete-entries total))
                     ((< number-of-retrieved 0) ;; the actual number may be less than (abs number-of-retrieved)
                      (wallabag-request-and-delete-entries (abs number-of-retrieved)))
                     (t
                      ;; (message "Found there may have %s new articles." number-of-retrieved)
                      (wallabag-request-and-insert-entries number-of-retrieved 'wallabag-request-and-delete-entries total)))))))))

(defun wallabag-request-and-delete-entries (perpage)
  "Request and check `wallabag-number-of-entries-to-be-synchronized' entries, entries that do not exist in the server will be deleted.
Please notice: this function should be called only when no new entires in the server!"
  (setq wallabag-retrieving-p "Verifing...") ; indicate it is retrieving.
  (let ((host wallabag-host)
        (token (or wallabag-token (wallabag-request-token)))
        (sort "created")
        (order "desc")
        (page 1)
        current
        position)
    (request (format "%s/api/entries.json" host)
      :parser 'buffer-string
      :params `(("sort" . ,sort)
                ("order" . ,order)
                ("page" . ,page)
                ("perPage" . ,(if (> wallabag-number-of-entries-to-be-synchronized 0)
                                  wallabag-number-of-entries-to-be-synchronized
                                perpage))
                ("access_token" . ,token))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                 ("Content-Type" . "application/json"))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Wallaget request error: %S" error-thrown)))
      :status-code '((401 . (lambda (&rest _)
                              (message "Authenticating...")
                              (wallabag-request-token)
                              (funcall 'wallabag-request-and-delete-entries perpage))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  ;; save the original string
                  ;; (with-temp-file wallabag-json-file
                  ;;   (insert data))
                  (setq entries (append (wallabag-parse-json (json-read-from-string data)) nil))
                  (setq entries (cl-loop for entry in entries collect
                                         (progn
                                           ;; push a new tag column into it
                                           (push
                                            (cons
                                             'tag
                                             (wallabag-convert-tags-to-tag entry)) entry)
                                           ;; return entry
                                           entry)))
                  (let* ((to-be-deleted (vconcat (cl-set-difference
                                                  (cl-loop for item in (wallabag-db-sql `[:select id :from items :order :by id :desc :limit ,perpage]) collect
                                                           (car item))
                                                  (cl-loop for entry in entries collect
                                                           (alist-get 'id entry)))))
                         (number-to-be-deleted (length to-be-deleted)))
                    ;; delete the non exist entries replied from wallabag server
                    (wallabag-db-delete to-be-deleted)

                    (setq wallabag-db-newp nil)
                    (cond
                     ((> number-to-be-deleted 0)
                      (message "Finished synchronization. Deleted %s articles." number-to-be-deleted) )
                     ((= number-to-be-deleted 0)
                      (message "Finished synchronization."))
                     (t (error "Synchronization error: number-to-be-deleted is %s", number-to-be-deleted))))
                  (with-silent-modifications
                    (wallabag-request-tags)
                    (if (buffer-live-p (get-buffer "*wallabag-search*"))
                        (with-current-buffer (get-buffer "*wallabag-search*")
                          (setq wallabag-search-filter "")
                          (setq current (point))
                          (setq position (window-start))
                          (erase-buffer)
                          (wallabag-search-update-buffer)
                          (wallabag-search-mode)
                          (set-window-start (selected-window) position)
                          (goto-char current)) ))

                  ;; indicate the retrieving is finished, and update the header
                  (setq wallabag-retrieving-p nil))))))

(defun wallabag-request-format (&optional format)
  "TODO: Request the format to be exported."
  (interactive)
  (let* ((entry (wallabag-find-candidate-at-point))
         (id (alist-get 'id entry))
         (host wallabag-host)
         (token (or wallabag-token (wallabag-request-token))))
    (request (format "%s/api/entries/%s/export.%s" host id (or format "pdf"))
      :encoding 'binary
      :params `(("access_token" . ,token))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Wallaget request error: %S" error-thrown)))
      :status-code '((401 . (lambda (&rest _)
                              (message "Authenticating...")
                              (wallabag-request-token)
                              (funcall 'wallabag-request-format format))))
      :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (message "Done: %s" (request-response-header response "content-type"))
                  (let*  ((name (let ((content-disposition (request-response-header response "content-disposition")))
                                  (when (string-match "\"\\(.*\\)\"" content-disposition)
                                    (match-string 1 content-disposition))))
                          (file (expand-file-name name wallabag-download-dir)))
                    (with-temp-file file
                      (insert (request-response-data response)))
                    (find-file file)))))))


(defun wallabag-request-tags (&optional callback)
  "Request all tags."
  (interactive)
  (let* ((host wallabag-host)
         (token (or wallabag-token (wallabag-request-token))))
    (request (format "%s/api/tags.json" host)
      :parser 'json-read
      :params `(("access_token" . ,token))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Wallaget request error: %S" error-thrown)))
      :status-code '((401 . (lambda (&rest _)
                              (message "Authenticating...")
                              (wallabag-request-token)
                              (funcall 'wallabag-request-tags callback))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((tag-list (mapcar
                                  (lambda(x)
                                    (cons
                                     (alist-get 'id x)
                                     (alist-get 'label x)))
                                  (append data nil))))
                    (setq wallabag-all-tags tag-list)
                    (if (and wallabag-show-sidebar wallabag-all-tags)
                        (wallabag-sidebar-create-window))
                    (if callback (funcall callback))
                    ;; (message "Retrieved all tags Done")
                    ))))))

(defun wallabag-add-tags (tags)
  "Add TAGS to the entry at point.
TAGS are seperated by comma."
  (interactive (list
                (wallabag-get-tag-name)))
  (let* ((entry (wallabag-find-candidate-at-point) )
         (id (alist-get 'id entry))
         (host wallabag-host)
         (token (or wallabag-token (wallabag-request-token)))
         (beg (line-beginning-position))
         (end (1+ (line-end-position)))
         ori)
    (request (format "%s/api/entries/%s/tags.json" host id)
      :parser 'json-read
      :type "POST"
      :data `(("access_token" . ,token)
                ("tags" . ,tags))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Wallaget request error: %S" error-thrown)))
      :status-code '((401 . (lambda (&rest _)
                              (message "Authenticating...")
                              (wallabag-request-token)
                              (funcall 'wallabag-add-tags tags))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((inhibit-read-only t)
                         (tags (alist-get 'tags data))
                         (tag (wallabag-convert-tags-to-tag data)))
                    (wallabag-db-update-tags id tags)
                    (wallabag-db-update-tag id tag)
                    (if (buffer-live-p (get-buffer "*wallabag-search*"))
                        (with-current-buffer (get-buffer "*wallabag-search*")
                          (setq ori (point))
                          (wallabag-search-update-buffer)
                          (goto-char ori)) )
                    (wallabag-request-tags)
                    (if (eq major-mode 'wallabag-entry-mode)
                        (wallabag-show-entry (car (wallabag-db-select :id id))))
                    (message "Add Tags Done")))))))

(defun wallabag-remove-tag ()
  "Remove one tag of the entry."
  (interactive)
  (let* ((entry (wallabag-find-candidate-at-point) )
         (id (alist-get 'id entry))
         (host wallabag-host)
         (token (or wallabag-token (wallabag-request-token)))
         (tag-list (mapcar
                    (lambda(x)
                      (cons
                       (alist-get 'id x)
                       (alist-get 'label x)))
                    (append (alist-get 'tags entry) nil)))
         (tag (car
               (cl-find
                (completing-read
                 "Selete the tag you want to delete: "
                 (mapcar 'cdr tag-list)) tag-list :test 'string= :key 'cdr)))
         (beg (line-beginning-position))
         (end (1+ (line-end-position)))
         ori)
    (request (format "%s/api/entries/%s/tags/%s.json" host id tag)
      :type "DELETE"
      :parser 'json-read
      :data `(("access_token" . ,token))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Wallaget request error: %S" error-thrown)))
      :status-code '((401 . (lambda (&rest _)
                              (message "Authenticating...")
                              (wallabag-request-token)
                              (funcall 'wallabag-remove-tag))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((inhibit-read-only t)
                        (tags (alist-get 'tags data))
                        (tag (wallabag-convert-tags-to-tag data)))
                    (wallabag-db-update-tags id tags)
                    (wallabag-db-update-tag id tag)
                    (if (buffer-live-p (get-buffer "*wallabag-search*"))
                        (with-current-buffer (get-buffer "*wallabag-search*")
                          (setq ori (point))
                          (wallabag-search-update-buffer)
                          (goto-char ori)) )
                    (wallabag-request-tags)
                    (message "Remove Tag Done")))))))

(defun wallabag-add-entry (&optional url)
  "Add a new entry by URL and TAGS."
  (interactive)
  (let* ((url (pcase major-mode
                ('elfeed-show-mode
                 (if elfeed-show-entry (elfeed-entry-link elfeed-show-entry) "" ))
                ('eaf-mode
                 (abbreviate-file-name eaf--buffer-url))
                ('eww-mode
                 (plist-get eww-data :url))
                (_ (if url url (read-from-minibuffer "What URL do you want to add? ")))))
         ;; FIXME if no tags pull before, it will return empty string
         (tags (wallabag-get-tag-name))
         (host wallabag-host)
         (token (or wallabag-token (wallabag-request-token))))
    (request (format "%s/api/entries.json" host)
      :parser 'json-read
      :type "POST"
      :data `(("url" . ,url)
              ("archive" . 0)
              ("starred" . 0)
              ("tags" . ,tags)
              ("access_token" . ,token))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Wallaget request error: %S" error-thrown)))
      :status-code '((401 . (lambda (&rest _)
                              (message "Authenticating...")
                              (wallabag-request-token)
                              (funcall 'wallabag-add-entry url))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  ;; convert tags array to tag comma seperated string
                  (setq data
                        (progn
                          (setf
                           (alist-get 'tag data)
                           (if (stringp (alist-get 'tag data))
                               (alist-get 'tag data)
                             (wallabag-convert-tags-to-tag data)))
                          data))
                  (let ((inhibit-read-only t)
                        (id (alist-get 'id data)))
                    ;; check id exists or not
                    (if (eq 1 (caar (wallabag-db-sql
                                     `[:select :exists
                                       [:select id :from items :where (= id ,id)]])))
                        (progn
                          (message "Entry Already Exists")
                          (goto-char (wallabag-find-candidate-location id))
                          (wallabag-flash-show (line-beginning-position) (line-end-position) 'highlight 0.5))
                      (wallabag-db-insert (list data))
                      (if (buffer-live-p (get-buffer "*wallabag-search*"))
                          (with-current-buffer (get-buffer "*wallabag-search*")
                            (save-excursion
                              (goto-char (point-min))
                              (funcall wallabag-search-print-entry-function data))) )
                      (message "Add Entry: %s" id)
                      (if wallabag-show-entry-after-creation
                          (wallabag-show-entry (car (wallabag-db-select :id id))) ))))))))

(defun wallabag-insert-entry (&optional url title content)
  "Insert a entry by URL, TITLE, and CONTENT."
  (interactive)
  (let* ((url (or url (read-from-minibuffer "What URL do you want to add? ") ))
         ;; FIXME if no tags pull before, it will return empty string
         ;; (tags (wallabag-get-tag-name))
         (tags "") ;; use empty string for now, it is faster to insert, and tags can be added later
         (host wallabag-host)
         (token (or wallabag-token (wallabag-request-token))))
    (require 'org-id)
    (request (format "%s/api/entries.json" host)
      :parser 'json-read
      :type "POST"
      :data `(("url" . ,url)
              ("title" . ,title)
              ("content" . ,content)
              ("archive" . 0)
              ("starred" . 0)
              ("tags" . ,tags)
              ("access_token" . ,token))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Wallaget request error: %S" error-thrown)))
      :status-code '((401 . (lambda (&rest _)
                              (message "Authenticating...")
                              (wallabag-request-token)
                              (funcall 'wallabag-insert-entry url title content))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  ;; convert tags array to tag comma seperated string
                  (setq data
                        (progn
                          (setf
                           (alist-get 'tag data)
                           (if (stringp (alist-get 'tag data))
                               (alist-get 'tag data)
                             (wallabag-convert-tags-to-tag data)))
                          data))
                  (let ((inhibit-read-only t)
                        (id (alist-get 'id data)))
                    (wallabag-db-insert (list data))
                    (if (buffer-live-p (get-buffer "*wallabag-search*"))
                        (with-current-buffer (get-buffer "*wallabag-search*")
                          (save-excursion
                            (goto-char (point-min))
                            (funcall wallabag-search-print-entry-function data))) )
                    (message "Insert Entry: %s" id)
                    (if wallabag-show-entry-after-creation
                        (wallabag-show-entry (car (wallabag-db-select :id id))) )))))))

(defun wallabag-delete-entry ()
  "Delete a entry at point."
  (interactive)
  (let* ((entry (wallabag-find-candidate-at-point) )
         (id (alist-get 'id entry))
         (title (alist-get 'title entry))
         (host wallabag-host)
         (token (or wallabag-token (wallabag-request-token)))
         (ori))
    (if (yes-or-no-p (format "Do you really want to Delete \"%s\"?" title))
        (request (format "%s/api/entries/%s.json" host id)
          :parser 'json-read
          :type "DELETE"
          :data `(("access_token" . ,token))
          :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
          :error
          (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                         (message "Wallaget request error: %S" error-thrown)))
          :status-code '((401 . (lambda (&rest _)
                                  (message "Authenticating...")
                                  (wallabag-request-token)
                                  (funcall 'wallabag-delete-entry))))
          :success (cl-function
                    (lambda (&key _data &allow-other-keys)
                      (let ((inhibit-read-only t))
                        (wallabag-db-delete id)
                        (if (buffer-live-p (get-buffer "*wallabag-search*"))
                            (with-current-buffer (get-buffer "*wallabag-search*")
                              (setq ori (point))
                              (wallabag-search-update-buffer wallabag-search-current-page)
                              (goto-char ori)) )
                        (if (eq major-mode 'wallabag-entry-mode)
                            (kill-buffer))
                        (message "Deletion Done"))))))))



(defmacro wallabag-update-entry (field int-or-str)
  `(defun ,(intern (format "wallabag-update-entry-%s" field)) (new)
     ,(format "Set %s." field)
     (interactive (list (if ,int-or-str
                            (if (eq (alist-get ',(intern (alist-get field wallabag-field-mapping nil nil #'equal))
                                               (wallabag-find-candidate-at-point)) 1 ) 0 1)
                          (read-from-minibuffer ,(format "Insert a new %s? " field)
                                                (alist-get ',(intern (alist-get field wallabag-field-mapping nil nil #'equal))
                                                           (wallabag-find-candidate-at-point))))))
     (let* ((entry (wallabag-find-candidate-at-point) )
         (id (alist-get 'id entry))
         (host wallabag-host)
         (token (or wallabag-token (wallabag-request-token)))
         (beg (line-beginning-position))
         (end (1+ (line-end-position))))
    (request (format "%s/api/entries/%s.json" host id)
          :parser 'json-read
          :type "PATCH"
          :data `(("access_token" . ,token)
                  (,,field . ,new))
          :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
          :error
          (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                         (message "Wallaget request error: %S" error-thrown)))
          :status-code '((401 . (lambda (&rest _)
                                  (message "Authenticating...")
                                  (wallabag-request-token)
                                  (funcall ',(intern (format "wallabag-update-entry-%s" field)) new))))
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (let* ((inhibit-read-only t)
                             (content (alist-get ',(intern (alist-get field wallabag-field-mapping nil nil #'equal)) data)))
                        (,(intern (format "wallabag-db-update-%s" (alist-get field wallabag-field-mapping nil nil #'equal))) id content)
                        (if (buffer-live-p (get-buffer "*wallabag-search*"))
                            (with-current-buffer (get-buffer "*wallabag-search*")
                              (setq ori (point))
                              (wallabag-search-update-buffer)
                              (goto-char ori)) )
                        (message "Update %s Done" ,field))))))))

(wallabag-update-entry "title" nil)
(wallabag-update-entry "archive" t)
(wallabag-update-entry "starred" t)
(wallabag-update-entry "origin_url" nil)

(defun wallabag-original-entry ()
  "Show entry rendered with original html."
  (interactive)
  (message "Retriving original page...")
  (let ((url (alist-get 'url (get-text-property (point-min) 'wallabag-entry nil))))
    (request url
      :parser 'buffer-string
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Wallaget request error: %S" error-thrown)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (wallabag-show-entry
                   (or
                    (get-text-property (point) 'wallabag-entry nil)
                    (get-text-property (point-min) 'wallabag-entry nil)) nil data))))))

(defun wallabag-browse-url ()
  "Browser entry with original url."
  (interactive)
  (funcall wallabag-browser-function
   (alist-get
    'url
    (or
     (get-text-property (point) 'wallabag-entry nil)
     (get-text-property (point-min) 'wallabag-entry nil)) )))

(defun wallabag-get-tag-name ()
  "Get the tag name in a list."
  (interactive)
  (if wallabag-all-tags
      (completing-read
       "Selete the tag you want to add: "
       (append '("All" "Unread" "Starred" "Archive" "All") (mapcar 'cdr wallabag-all-tags)))
      (wallabag-request-tags 'wallabag-get-tag-name)
      "")) ;; return empty tag name to avoid error


(defun wallabag-parse-json (json)
  (alist-get 'items (assoc '_embedded json)))

(defun wallabag-get-total (json)
  (cdr (assoc 'total json)))

(defvar wallabag-find-history nil)

;;;###autoload
(defun wallabag-find ()
  "Find the entry from list using ivy or consult."
  (interactive)
  (cond
   ((featurep 'ivy)
    (ivy-read "Wallabag: " #'wallabag-parse-entries-as-list
              :dynamic-collection t
              :sort nil
              :action (lambda (cand)
                        (with-ivy-window
                         (wallabag-show-entry (get-text-property 0 'wallabag-entry cand))))
              :caller 'wallagab-find))
   ((fboundp 'consult--read)
    (consult--read (nreverse (mapcar (lambda(x)
                                       (list (format "[%s] ⇰ %s (%s) %s %s"
                                                     (propertize (nth 0 x) 'face 'wallabag-title-face)
                                                     (propertize (nth 1 x) 'face 'wallabag-domain-name-face)
                                                     (propertize (mapconcat
                                                      #'identity
                                                      (mapcar
                                                       ;; get label of each element
                                                       (lambda(x)
                                                         (alist-get 'label x))
                                                       ;; get tags vector
                                                       (nth 2 x))
                                                      ;; concat with ,
                                                      ",") 'face 'wallabag-tag-face)
                                                     (propertize (concat (number-to-string (nth 3 x)) " min") 'face 'wallabag-reading-time-face)
                                                     (propertize (s-left 10 (nth 4 x)) 'face 'wallabag-date-face))
                                             (nth 5 x)))
                                     (wallabag-db-sql '[:select [title domain_name tags reading_time created_at id] :from items])))
                   :sort nil
                   :history 'wallabag-find-history
                   :prompt "Wallabag: "
                   :lookup (lambda(cand candidates input-string _)
                             (when (string-match "⇰\s\\(.+\\)$" cand)
                               (wallabag-show-entry (car (wallabag-db-select :id (nth 1 (assoc cand candidates)))))))))
   (t (message "`wallabag-find' only supportes ivy and consult."))))

;;; header

(defun wallabag-search-header ()
  "TODO: Return the string to be used as the wallabag header."
  (format "%s%s"
          (if (string-equal system-type "android")
              ""
            (format "%s%s   "
                    (propertize "Wallabag: " 'face font-lock-preprocessor-face)
                    (propertize (format "%s" wallabag-host) 'face font-lock-type-face)))
          (concat
           (if wallabag-retrieving-p
               (propertize wallabag-retrieving-p 'face font-lock-warning-face)
             (format "Total: %s"
                     (propertize (if (equal wallabag-search-entries-length 0)
                                     "0   "
                                   (concat (number-to-string wallabag-search-entries-length) " ")) 'face font-lock-warning-face)))
           (format "  Page: %s/%s  "
                   (propertize (number-to-string wallabag-search-current-page) 'face 'font-lock-type-face)
                   (propertize (number-to-string wallabag-search-pages) 'face 'font-lock-type-face))
           (propertize (format "%s%s"
                               (if wallabag-group-filteringp
                                   "Group: "
                                 "")
                               (if (equal wallabag-search-filter "")
                                        ""
                                      (concat wallabag-search-filter "   "))) 'face font-lock-keyword-face)
           (propertize (let ((len (length (wallabag-find-marked-candidates))))
                         (if (> len 0)
                             (concat "Marked: " (number-to-string len)) "")) 'face font-lock-negation-char-face))))

;;; wallabag-search-mode

(defvar wallabag-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<RET>") #'wallabag-view)
    (define-key map "v" #'wallabag-view)
    (define-key map "V" #'wallabag-browse-url)
    (define-key map "&" #'wallabag-browse-with-external-browser)
    (define-key map "o" #'wallabag-original-entry)
    (define-key map "s" #'wallabag-search-live-filter)
    (define-key map "q" #'wallabag-search-quit)
    (define-key map "g" #'wallabag-search-refresh-and-clear-filter)
    (define-key map "G" #'wallabag-search-clear-filter)
    (define-key map "u" #'wallabag-search-update-and-clear-filter)
    (define-key map "U" #'wallabag-search-synchronize-and-clear-filter)
    (define-key map "m" #'wallabag-mark-and-forward)
    (define-key map (kbd "<DEL>") #'wallabag-unmark-and-backward)
    (define-key map "a" #'wallabag-add-entry)
    (define-key map "d" #'wallabag-delete-entry)
    (define-key map "n" #'wallabag-search-next-page)
    (define-key map "p" #'wallabag-search-previous-page)
    (define-key map "N" #'wallabag-next-entry)
    (define-key map "P" #'wallabag-previous-entry)
    (define-key map "w" #'wallabag-org-link-copy)
    (define-key map "W" #'wallabag-org-protocol-link-copy)
    (define-key map "t" #'wallabag-add-tags)
    (define-key map "T" #'wallabag-remove-tag)
    (define-key map "'" #'wallabag-goto-sidebar)
    (define-key map "b" #'wallabag-toggle-sidebar)
    (define-key map "x" #'wallabag-update-entry-archive)
    (define-key map "f" #'wallabag-update-entry-starred)
    (define-key map "i" #'wallabag-update-entry-title)
    (define-key map "I" #'wallabag-update-entry-origin_url)
    map)
  "Keymap for `wallabag-search-mode'.")

(if (featurep 'evil)
    (evil-define-key '(normal emacs) wallabag-search-mode-map
      (kbd "<RET>") 'wallabag-view
      (kbd "v") 'wallabag-view
      (kbd "V") 'wallabag-browse-url
      (kbd "&") 'wallabag-browse-with-external-browser
      (kbd "o") 'wallabag-original-entry
      (kbd "/") 'wallabag-search-live-filter
      (kbd "q") 'wallabag-search-quit
      (kbd "g r") 'wallabag-search-clear-filter
      (kbd "g R") 'wallabag-search-refresh-and-clear-filter
      (kbd "u") 'wallabag-search-update-and-clear-filter
      (kbd "U") 'wallabag-search-synchronize-and-clear-filter
      (kbd "m") 'wallabag-mark-and-forward
      (kbd "<DEL>") 'wallabag-unmark-and-backward
      (kbd "a") 'wallabag-add-entry
      (kbd "d") 'wallabag-delete-entry
      (kbd "D") 'wallabag-delete-entry
      (kbd "j") 'wallabag-next-entry
      (kbd "k") 'wallabag-previous-entry
      (kbd "n") 'wallabag-search-next-page
      (kbd "p") 'wallabag-search-previous-page
      (kbd "y o") 'wallabag-org-link-copy
      (kbd "y M") 'wallabag-org-markdown-copy
      (kbd "y t") 'wallabag-org-title-copy
      (kbd "y u") 'wallabag-org-url-copy
      (kbd "y m") 'wallabag-org-protocol-link-markdown-copy
      (kbd "y y") 'wallabag-org-protocol-link-copy
      (kbd "t") 'wallabag-add-tags
      (kbd "T") 'wallabag-remove-tag
      (kbd "'") 'wallabag-list-tags
      (kbd "g t") 'wallabag-toggle-sidebar
      (kbd "g s") 'wallabag-goto-sidebar
      (kbd "x") 'wallabag-update-entry-archive
      (kbd "f") 'wallabag-update-entry-starred
      (kbd "i") 'wallabag-update-entry-title
      (kbd "I") 'wallabag-update-entry-origin_url))

(define-derived-mode wallabag-search-mode fundamental-mode "wallabag-search"
  "Major mode for listing wallabag entries.
\\{wallabag-search-mode-map}"
  (setq truncate-lines t
        buffer-read-only t
        header-line-format '(:eval (funcall wallabag-search-header-function)))
  (buffer-disable-undo)
  (require 'hl-line)
  (set (make-local-variable 'hl-line-face) 'wallabag-current-match)
  (hl-line-mode)
  (add-hook 'minibuffer-setup-hook 'wallabag-search-minibuffer-setup))

(defun wallabag-search-buffer ()
  "Create buffer *wallabag-search*."
  (get-buffer-create "*wallabag-search*"))

;;;###autoload
(defun wallabag ()
  "Enter the *wallbag-search* buffer."
  (interactive)
  (wallabag-db)
  (wallabag-emoji-init)
  (when (get-buffer (wallabag-search-buffer))
    (kill-buffer (wallabag-search-buffer)))
  (switch-to-buffer (wallabag-search-buffer))
  (goto-char (point-min))
  (let ((wallabag-live-filteringp t))
    (wallabag-search-update-buffer wallabag-search-current-page) )
  (unless (eq major-mode 'wallabag-search-mode)
    (wallabag-search-mode))
  (if (and wallabag-show-sidebar wallabag-all-tags)
      (wallabag-sidebar-create-window))
  (run-hooks 'wallabag-after-render-hook))

(defun wallabag-mouse-1 (event)
  "Browser the url click.
Argument EVENT mouse event."
  (interactive "e")
  ;; (message "click mouse-3")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No URL chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (funcall wallabag-browser-function (get-text-property (point) 'help-echo)))))

(defun wallabag-ret ()
  "Browser the url with keyboad RET."
  (interactive)
  ;; (message "click mouse-3")
  (funcall wallabag-browser-function (get-text-property (point) 'help-echo)))

(defun wallabag-view ()
  "View the wallabag entry."
  (interactive)
  (wallabag-show-entry (wallabag-find-candidate-at-point)))

(defun wallabag-browse-with-external-browser ()
  "View the wallabag entry with `wallabag-browser-function'."
  (interactive)
  (let* ((entry (wallabag-find-candidate-at-point))
         (title (or (alist-get 'title entry) "NO TITLE"))
         (reading-time (alist-get 'reading_time entry))
         (created-at (alist-get 'created_at entry))
         (tag (alist-get 'tag entry))
         (domain-name (or (alist-get 'domain_name entry) ""))
         (content (or (alist-get 'content entry)  ""))
         (url (alist-get 'url entry))
         (origin-url (or (alist-get 'origin_url entry) "")))
    (when (get-buffer "*wallabag-entry-html*")
      (kill-buffer "*wallabag-entry-html*"))
    (with-current-buffer (get-buffer-create "*wallabag-entry-html*")
      (wallabag-emoji-init)
      (insert "<title>" title "</title>")
      (insert "<h1>" title "</h1>")
      (insert
       (format "
<div class=\"subtitle\">
%s %s min read
%s<a href=\"%s\">%s</a>
%s<a href=\"%s\">%s</a>
%s%s
</div>"
               (replace-regexp-in-string "T" " " (substring created-at 0 19))
               reading-time
               (cdr (cl-find ":arrow-upper-right:" wallabag-emoji-alist :test 'string= :key 'car) )
               url
               domain-name
               (if (string= origin-url "")
                   ""
                 (cdr (cl-find ":arrow-upper-right:" wallabag-emoji-alist :test 'string= :key 'car) ))
               origin-url
               (let ((len (length origin-url))
                     (max 30))
                 (if (> len max)
                     (concat (substring origin-url 0 max) "...")
                   (substring origin-url 0 len)))
               (if (string= tag "")
                   ""
                 (cdr (cl-find ":pushpin:" wallabag-emoji-alist :test 'string= :key 'car)))
               tag))
      (insert (format "<div class=\"article\">%s</div>" content) )
      (insert "<style>")
      (insert-file-contents wallabag-css-file)
      (goto-char (point-max))
      (insert "</style>")
      ;; hcak `browse-url-of-buffer'
      (let ((file-name
             ;; Ignore real name if restricted
             (and (not (buffer-narrowed-p))
                  (or buffer-file-name
                      (and (boundp 'dired-directory) dired-directory)))))
        (when (or (not file-name)
                  ;; This can happen when we're looking at a file from a
                  ;; zip file buffer, for instance.
                  (not (file-exists-p file-name)))
          (unless browse-url-temp-file-name
            (setq browse-url-temp-file-name
                  (convert-standard-filename
                   (make-temp-file
                    (expand-file-name "burl" browse-url-temp-dir)
                    nil ".html"))))
          (setq file-name browse-url-temp-file-name)
          (write-region (point-min) (point-max) file-name nil 'no-message))
        (funcall wallabag-browser-function (browse-url-file-url file-name))))))


(defcustom wallabag-search-view-mode 'list
  "The view mode of the search buffer.
Under table mode, the tile width is controlled by
 `wallabag-search-title-min-width',
 `wallabag-search-title-max-width',
 `wallabag-search-trailing-width'.
If list mode, the title is full width."
  :type '(choice (const :tag "Table" table)
                 (const :tag "List" list)))

(defcustom wallabag-search-print-items '("title" "domain" "tag" "reading-time" "date")
  "The items to be printed in the search buffer.
The items are printed in the order of the list.
title, domain, tag, reading-time, date, content, seperator are supported.
for other characters, they are printed as they are."
  :type '(repeat string)
  :group 'wallabag)

(defun wallabag-parse-entry-as-string (entry)
  "Parse the wallabag ENTRY and return as string."
  (let* ((title (or (alist-get 'title entry) "NO TITLE"))
         (created-at (alist-get 'created_at entry))
         (created-at-days (string-to-number (format-seconds "%d" (+ (float-time (time-subtract (current-time) (encode-time (parse-time-string created-at))))
                                                                    86400)))) ;; shift 1 day
         (reading-time (alist-get 'reading_time entry))
         (is-archived (alist-get 'is_archived entry))
         (is-starred (alist-get 'is_starred entry))
         (tag (alist-get 'tag entry))
         (domain-name (or (alist-get 'domain_name entry) ""))
         (title-width (- (window-width (selected-window) ) wallabag-search-trailing-width))
         (star (if (= is-starred 0)
                   ""
                 (format "%s " (propertize wallabag-starred-icon
                                           'face 'wallabag-starred-face
                                           'mouse-face 'wallabag-mouse-face
                                           'help-echo "Filter the favorite items"))))
         (content (alist-get 'content entry)))
    (mapconcat #'identity
               (cl-loop for item in wallabag-search-print-items
                        collect (pcase item
                                  ("date" (propertize
                                           (cond ((< created-at-days 7)
                                                  (format "%sd" created-at-days))
                                                 ((< created-at-days 30)
                                                  (format "%sw" (/ created-at-days 7)))
                                                 ((< created-at-days 365)
                                                  (format "%sm" (/ created-at-days 30)))
                                                 (t
                                                  (format "%sy" (/ created-at-days 365))))
                                           'face 'wallabag-date-face))
                                  ("title" (format "%s%s" star
                                                    (if (= is-archived 0)
                                                        (if (equal wallabag-search-view-mode 'table)
                                                            (propertize (wallabag-format-column
                                                                         title (wallabag-clamp
                                                                                (- wallabag-search-title-min-width (length star))
                                                                                (- title-width (length star))
                                                                                (- wallabag-search-title-max-width (length star)))
                                                                         :left) 'face 'wallabag-title-face)
                                                          (propertize title 'face 'wallabag-title-face))
                                                      (if (equal wallabag-search-view-mode 'table)
                                                          (propertize (wallabag-format-column
                                                                       title (wallabag-clamp
                                                                              (- wallabag-search-title-min-width (length star))
                                                                              (- title-width (length star))
                                                                              (- wallabag-search-title-max-width (length star)))
                                                                       :left) 'face 'wallabag-archive-face)
                                                        (propertize title 'face 'wallabag-title-face)))))
                                  ("content" (propertize (if (or (string-empty-p content)
                                                                 (eq content nil))
                                                             ""
                                                           (s-word-wrap (window-width (selected-window))
                                                                        (s-truncate
                                                                         wallabag-search-content-max-width
                                                                         (replace-regexp-in-string "[[:space:]\n\r]+" " "
                                                                                                   (replace-regexp-in-string "<[^>]+>" "" content)
                                                                                                   ;; (wallabag-render-content content) ;; too slow, disable
                                                                                                   ))))
                                                         'face 'wallabag-content-face))
                                  ("domain" (propertize domain-name 'face 'wallabag-domain-name-face))
                                  ("tag" (format (if (string-empty-p tag) "" "(%s)" ) (propertize tag 'face 'wallabag-tag-face) ))
                                  ("reading-time" (propertize (concat (number-to-string reading-time) " min") 'face 'wallabag-reading-time-face))
                                  ("seperator" (format "\n%s" (make-string (window-width) ?-)))
                                  (_ item)))
               " ")))

(defun wallabag-parse-entries-as-list (filter)
  "Parse all entries with FILTER, return as propertized string list."
  (cl-loop for entry in (wallabag-search-get-filtered-entries filter) collect
           (propertize (wallabag-parse-entry-as-string entry) 'wallabag-entry entry)))

(defun wallabag-search-print-entry--default (entry)
  "Print ENTRY to the buffer."
  (unless (equal entry "")
    (let (beg end)
      (setq beg (point))
      (insert (wallabag-parse-entry-as-string entry))
      (setq end (point))
      ;; format the tag and push into attr alist
      (put-text-property beg end 'wallabag-entry entry)
      (put-text-property beg end 'wallabag-id (alist-get 'id entry))
      (insert "\n"))))


(defun wallabag-quit ()
  "Quit *wallabag-entry* or *wallabag-search*."
  (interactive)
  (when (eq major-mode 'wallabag-search-mode)
    (cond ((get-buffer "*wallabag-entry*")
           (pop-to-buffer "*wallabag-entry*")
           (if (< (length (window-prev-buffers)) 2)
               (progn
                 (quit-window)
                 (kill-buffer "*wallabag-entry*"))
             (kill-buffer "*wallabag-entry*")))
          ((get-buffer "*wallabag-search*")
           (quit-window)
           (kill-buffer "*wallabag-search*")
           (wallabag-sidebar-quit)))))

(defun wallabag-search-quit ()
  "Quit *wallabag-search*."
  (interactive)
  (when (eq major-mode 'wallabag-search-mode)
    (emacsql-close (wallabag-db))
    (quit-window)
    (kill-buffer "*wallabag-search*")
    (wallabag-sidebar-quit)))

;; mark/unmark

(defun wallabag-mark-at-point ()
  "Mark the current line."
  (interactive)
  (remove-overlays (line-beginning-position) (line-end-position))
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (inhibit-read-only t)
         (overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'wallabag-mark-face)
    (put-text-property beg end 'wallabag-mark ?>)))

(defun wallabag-mark-and-forward ()
  "Mark the current line and forward."
  (interactive)
  (wallabag-mark-at-point)
  (wallabag-next-entry))

(defun wallabag-unmark-and-forward ()
  "Unmark the current line and forward."
  (interactive)
  (wallabag-unmark-at-point)
  (wallabag-next-entry))

(defun wallabag-unmark-and-backward ()
  "Unmark the current line and backward."
  (interactive)
  (wallabag-previous-entry)
  (wallabag-unmark-at-point))

;; moving

(defun wallabag-unmark-at-point ()
  "Unmark the current line."
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (inhibit-read-only t))
    (remove-overlays (line-beginning-position) (line-end-position))
    (remove-text-properties beg end '(wallabag-mark nil))))

(defun wallabag-next-entry ()
  "Move to next entry."
  (interactive)
  (if (eq major-mode 'wallabag-search-mode)
      (if (text-property-not-all (point) (point-max) 'wallabag-entry nil)
          (let ((ori "") (new ""))
            (while (and (equal new ori) new ori)
              (setq ori (alist-get 'id (get-text-property (point) 'wallabag-entry nil)))
              (text-property-search-forward 'wallabag-entry nil)
              (goto-char (1+ (point)))
              (setq new (alist-get 'id (get-text-property (point) 'wallabag-entry nil)))
              ;; (recenter (round (/ (window-body-height) 2)))
              ;; last one in the page
              (unless (text-property-not-all (point) (point-max) 'wallabag-entry nil)
                (wallabag-search-next-page))))
        (wallabag-search-next-page))
    (let* ((current-id (alist-get 'id (get-text-property (point-min) 'wallabag-entry nil)))
           (next-entry (car (wallabag-db-select :next-entry current-id) )))
      (if next-entry (wallabag-show-entry next-entry)
        (message "This is the last entry")))))

(defun wallabag-previous-entry ()
  "Move to previous entry."
  (interactive)
  (if (eq major-mode 'wallabag-search-mode)
      (if (eq (point) (point-min))
          (if (eq wallabag-search-current-page 1)
              (message "This is the first entry")
            (wallabag-search-previous-page)
            (goto-char (point-max))
            (text-property-search-backward 'wallabag-entry nil)
            ;; (recenter (round (/ (window-body-height) 2)))
            )
        (let ((ori "") (new ""))
          (while (and (equal new ori) new ori (> (line-number-at-pos) 1))
            (forward-line -1)
            (save-excursion
              (setq ori (alist-get 'id (get-text-property (point) 'wallabag-entry nil)))
              (forward-line -1)
              (setq new (alist-get 'id (get-text-property (point) 'wallabag-entry nil)))))) )
    (let* ((current-id (alist-get 'id (get-text-property (point-min) 'wallabag-entry nil)))
           (previous-entry (car (wallabag-db-select :previous-entry current-id) )))
      (if previous-entry (wallabag-show-entry previous-entry)
        (message "This is the first entry")))))

;; refresh

(defun wallabag-refresh-and-resume (&optional begin position)
  "Refresh wallabag or resume the BEGIN point and windows POSITION."
  (interactive)
  (let (beg pos)
    (setq beg (or begin (point)))
    (setq pos (or position (window-start)))
    (wallabag-search-refresh-and-clear-filter)
    (set-window-start (selected-window) pos)
    (goto-char beg)))

(defun wallabag-search-refresh-and-clear-filter ()
  "Refresh wallabag and clear the filter keyword."
  (interactive)
  (setq wallabag-group-filteringp nil)
  (setq wallabag-search-current-page 1)
  (wallabag-search-update-buffer-with-keyword "")
  (wallabag))

(defun wallabag-search-update-and-clear-filter ()
  "Request new entries, clear the filter keyword, and update *wallabag-search*."
  (interactive)
  (call-interactively 'wallabag-request-new-entries)
  (message "Retriving new articles from wallabag host %s ..." wallabag-host))

(defun wallabag-search-synchronize-and-clear-filter ()
  "Synchronize entries, clear the filter keyword, and update *wallabag-search*."
  (interactive)
  (call-interactively 'wallabag-request-and-synchronize-entries)
  (message "Synchronizing articles from wallabag host %s ..." wallabag-host))

;;; wallabag-entry-mode

(defvar wallabag-entry-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "[" #'wallabag-previous-entry)
    (define-key map "]" #'wallabag-next-entry)
    (define-key map "&" #'wallabag-browse-with-external-browser)
    (define-key map "q" #'wallabag-entry-quit)
    (define-key map "d" #'wallabag-delete-entry)
    (define-key map "t" #'wallabag-add-tags)
    (define-key map "T" #'wallabag-remove-tag)
    (define-key map "v" #'wallabag-view)
    (define-key map "V" #'wallabag-browse-url)
    (define-key map "o" #'wallabag-original-entry)
    map)
  "Keymap for `wallabag-entry-mode'.")

(if (featurep 'evil)
    (evil-define-key '(normal emacs) wallabag-entry-mode-map
      (kbd "[") 'wallabag-previous-entry
      (kbd "]") 'wallabag-next-entry
      (kbd "&") 'wallabag-browse-url
      (kbd "g r") 'wallabag-view
      (kbd "o") 'wallabag-original-entry
      (kbd "t") 'wallabag-add-tags
      (kbd "T") 'wallabag-remove-tag
      (kbd "D") 'wallabag-delete-entry
      (kbd "q") 'wallabag-entry-quit))

(define-derived-mode wallabag-entry-mode fundamental-mode "wallabag-entry"
  "Mode for displaying wallabag entry details.
\\{wallabag-entry-mode-map}"
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defun wallabag-show--buffer-name ()
  "Return the appropriate buffer name for wallabag entry."
  "*wallabag-entry*")

(defun wallabag-show-entry (entry &optional switch html)
  "Display ENTRY in the current buffer.
Optional argument SWITCH to switch to *wallabag-entry* buffer to other window."
  (unless (eq major-mode 'wallabag-entry-mode)
      (when (get-buffer (wallabag-show--buffer-name))
        (kill-buffer (wallabag-show--buffer-name))))
  (let* ((buff (get-buffer-create (wallabag-show--buffer-name)))
         (original (point))
         (title (or (alist-get 'title entry) "NO TITLE"))
         (reading-time (alist-get 'reading_time entry))
         (created-at (alist-get 'created_at entry))
         (tag (alist-get 'tag entry))
         (domain-name (or (alist-get 'domain_name entry) ""))
         (content (or html (alist-get 'content entry) ""))
         (url (alist-get 'url entry))
         (origin-url (or (alist-get 'origin_url entry) ""))
         beg end)
    (unless (eq major-mode 'wallabag-entry-mode)
      (funcall wallabag-show-entry-switch buff)
      (when switch
        (switch-to-buffer-other-window (set-buffer (wallabag-search-buffer)))
        (goto-char original)))
    (let ((inhibit-read-only t))
      (with-current-buffer buff
        (wallabag-emoji-init)
        (erase-buffer)
        (insert (propertize title 'face 'wallabag-entry-title-face 'wallabag-entry entry))
        (insert "\n")
        (insert (format "%s  %s  %s%s  %s%s %s%s"
                        (propertize (format "%s min" reading-time) 'face 'wallabag-reading-time-face)
                        (propertize (format "%s" (replace-regexp-in-string "T" " " (substring created-at 0 19))) 'face 'wallabag-date-face)
                        (cdr (cl-find ":arrow-right-hook:" wallabag-emoji-alist :test 'string= :key 'car) )
                        (let ((map (make-sparse-keymap)))
                          (define-key map [mouse-1] 'wallabag-mouse-1)
                          (define-key map (kbd "<RET>") 'wallabag-ret)
                          (propertize domain-name
                                      'face 'wallabag-domain-name-face
                                      'help-echo url
                                      'follow-link t
                                      'mouse-face 'highlight
                                      'keymap map))
                        (if (string= origin-url "")
                            ""
                            (cdr (cl-find ":arrow-upper-right:" wallabag-emoji-alist :test 'string= :key 'car) ))
                        (let ((map (make-sparse-keymap)))
                          (define-key map [mouse-1] 'wallabag-mouse-1)
                          (define-key map (kbd "<RET>") 'wallabag-ret)
                          (propertize (let ((len (length origin-url))
                                            (max 30))
                                        (if (> len max)
                                            (concat (substring origin-url 0 max) "...")
                                          (substring origin-url 0 len)))
                                      'face 'wallabag-domain-name-face
                                      'help-echo origin-url
                                      'follow-link t
                                      'mouse-face 'highlight
                                      'keymap map))
                        (if (string= tag "")
                            ""
                          (cdr (cl-find ":pushpin:" wallabag-emoji-alist :test 'string= :key 'car)))
                        (propertize tag 'face 'wallabag-tag-face)))
        (insert "\n")
        (insert "\n")
        (setq beg (point))
        (insert content)
        (setq end (point))
        (wallabag-entry-mode)
        (funcall wallabag-render-html-function beg end)
        (goto-char (point-min))))))

(defun wallabag-render-html (begin end)
  "Render HTML in current buffer with shr."
  (run-hooks 'wallabag-pre-html-render-hook)
  (shr-render-region begin end)
  (run-hooks 'wallabag-post-html-render-hook))

(defun wallabag-render-content (content)
  "Render content with shr."
  (with-temp-buffer
    (insert content)
    (let ((shr-use-fonts nil))
      (cl-letf (((symbol-function 'shr-tag-img) nil)) ;; do not load online images
        (shr-render-region (point-min) (point-max))))
    (buffer-string)))

(defun wallabag-entry-quit ()
  "Quit the *wallabag-entry*."
  (interactive)
  (when (eq major-mode 'wallabag-entry-mode)
    (when (get-buffer "*wallabag-entry*")
      (pop-to-buffer "*wallabag-entry*")
      (if (< (length (window-prev-buffers)) 2)
          (progn
            (quit-window)
            (kill-buffer "*wallabag-entry*"))
        (kill-buffer "*wallabag-entry*")
        (when (buffer-live-p (get-buffer "*wallabag-search*"))
          (switch-to-buffer (get-buffer "*wallabag-search*")))))))


;;; sidebar

(defgroup wallabag-sidebar ()
  "Options for `wallabag-sidebar-mode'."
  :group 'wallabag)

(defcustom wallabag-sidebar-display-alist
  '((side . left)
    (window-width . 20)
    (slot . -1))
  "Association list used to display wallabag sidebar buffer.

See `display-buffer-in-side-window' for example options."
  :type 'alist
  :safe (lambda (value)
          (and (listp value)
               (seq-every-p 'consp value)))
  :group 'wallabag-sidebar)


(defcustom wallabag-sidebar-persistent-window
  t
  "When non-nil, sidebar will persist when calling `delete-other-windows'.

This marks `no-delete-other-windows' window parameter as non-nil.

Use `wallabag-toggle-sidebar' or `quit-window' to close the sidebar."
  :type 'boolean
  :safe 'booleanp
  :group 'wallabag-sidebar)

(defcustom wallabag-sidebar-buffer "*Wallabag Sidebar*"
  "Default buffer name for wallabag sidebar."
  :type 'string
  :safe 'stringp
  :group 'wallabag-sidebar)

(defcustom wallabag-sidebar-select-window
  nil
  "If non-nil, switch to wallabag sidebar upon displaying it."
  :type 'boolean
  :safe 'booleanp
  :group 'wallabag-sidebar)

(defvar wallabag-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "'" #'wallabag-toggle-sidebar)
    (define-key map "<RET>" #'wallabag-sidebar-find-tag)
    (define-key map "g" #'wallabag-search-clear-filter)
    (define-key map "G" #'wallabag-search-clear-filter)
    (define-key map "n" #'wallabag-sidebar-find-next-tag)
    (define-key map "p" #'wallabag-sidebar-find-previous-tag)
    (define-key map "q" #'wallabag-sidebar-quit)
    map)
  "Keymap for `wallabag-sidebar-mode'.")

(if (featurep 'evil)
    (evil-define-key '(normal emacs) wallabag-sidebar-mode-map
      (kbd "'") 'wallabag-toggle-sidebar
      (kbd "<RET>") 'wallabag-sidebar-find-tag
      (kbd "g r") 'wallabag-search-clear-filter
      (kbd "g R") 'wallabag-search-clear-filter
      (kbd "n") 'wallabag-sidebar-find-next-tag
      (kbd "p") 'wallabag-sidebar-find-previous-tag
      (kbd "q") 'wallabag-sidebar-quit))

(define-derived-mode wallabag-sidebar-mode
  special-mode "Wallabag Sidebar"
  "Major mode for working with `wallabag' projects."
  (face-remap-add-relative 'default 'wallabag-sidebar-face)
  ;; (add-hook 'post-command-hook #'wallabag-sidebar-sync-notes t t)

  )

(defun wallabag-sidebar-create-buffer ()
  "Return wallabag sidebar buffer for DIRECTORY."
  ;; (wallabag-ensure-in-project)
  (with-current-buffer (get-buffer-create wallabag-sidebar-buffer)
    (wallabag-sidebar-mode)
    (wallabag-sidebar-refresh)
    (current-buffer)))

(defun wallabag-sidebar-create-window ()
  "Return wallabag sidebar window for DIRECTORY.
Defaults to current directory."
  (let ((display-buffer-mark-dedicated t))
    (display-buffer-in-side-window
     (wallabag-sidebar-create-buffer)
     (append wallabag-sidebar-display-alist
             (when wallabag-sidebar-persistent-window
               (list '(window-parameters (no-delete-other-windows . t))))))))

;;;###autoload
(defun wallabag-toggle-sidebar ()
  "Toggle visibility of project sidebar window."
  (interactive)
  (if (window-live-p (get-buffer-window wallabag-sidebar-buffer))
      (delete-window (get-buffer-window wallabag-sidebar-buffer))
    (wallabag-sidebar-create-window)
    (when wallabag-sidebar-select-window
      (select-window (get-buffer-window wallabag-sidebar-buffer)))))


;;;###autoload
(defun wallabag-goto-sidebar ()
  "Create or goto sidebar window."
  (interactive)
  (if (window-live-p (get-buffer-window wallabag-sidebar-buffer))
      (select-window (get-buffer-window wallabag-sidebar-buffer))
    (wallabag-sidebar-create-window)
    (select-window (get-buffer-window wallabag-sidebar-buffer))))

(defun wallabag-sidebar-refresh ()
   (with-current-buffer wallabag-sidebar-buffer
    (with-silent-modifications
      (setq header-line-format (list :propertize "Groups" 'face 'bold))
      (erase-buffer)
      (insert (propertize "Unread\n" 'face 'bold))
      (insert (propertize "Starred\n" 'face 'bold))
      (insert (propertize "Archive\n" 'face 'bold))
      (insert (propertize "All\n" 'face 'bold))
      (insert (propertize "Tags\n" 'face 'bold))
      ;; insert tags
      (dolist (tag wallabag-all-tags)
        (insert (cdr tag))
        (insert "\n")))
    (goto-char (point-min))))

(defun wallabag-sidebar-quit ()
  (interactive)
  (if (window-live-p (get-buffer-window wallabag-sidebar-buffer))
      (delete-window (get-buffer-window wallabag-sidebar-buffer))))

(defun wallabag-sidebar-find-tag ()
  "Filter by tag at point."
  (interactive)
  (setq wallabag-group-filteringp t)
  (wallabag-search-update-buffer-with-keyword (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun wallabag-sidebar-find-next-tag ()
  "Filter by next tag at point."
  (interactive)
  (forward-line 1)
  (setq wallabag-group-filteringp t)
  (wallabag-search-update-buffer-with-keyword (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun wallabag-sidebar-find-previous-tag ()
  "Filter by previous tag at point."
  (interactive)
  (forward-line -1)
  (setq wallabag-group-filteringp t)
  (wallabag-search-update-buffer-with-keyword (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun wallabag-list-tags ()
  "List all tags in the sidebar."
  (interactive)
  (setq wallabag-group-filteringp t)
  (wallabag-search-update-buffer-with-keyword (wallabag-get-tag-name)))

;;; live filtering

(defun wallabag-search-live-filter ()
  "Filter the *wallabag-search* buffer as the filter is written.
Currently, the filtering is column-oriented, not buffer oriented.
The following columns will be searched:

- title
- created_at
- domain_name
- tag

If the keyword occurs in any of the columns above, the matched
record will be shown.

1. Live filter on the database directly.
2. The keyword supports REGEX.
3. Inserting Spaces between
   keywords can narrow down the search results."

  (interactive)
  (setq wallabag-live-filteringp t)
  (setq wallabag-group-filteringp nil)
  (unwind-protect
      (let ((wallabag-search-filter-active :live))
        (setq wallabag-search-filter
              (read-from-minibuffer "Filter: " wallabag-search-filter))
        (message wallabag-search-filter))
    (progn (wallabag-search-update-buffer)
           (setq wallabag-live-filteringp nil))))

(defun wallabag-search-minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when wallabag-search-filter-active
    (when (eq :live wallabag-search-filter-active)
      (add-hook 'post-command-hook 'wallabag-search-update-buffer-with-minibuffer-contents nil :local))))

(defun wallabag-search-update-buffer-with-minibuffer-contents ()
  "Update the *wallabag-search* buffer based on the contents of the minibuffer."
  (when (eq :live wallabag-search-filter-active)
    ;; (message "HELLO")
    (let ((buffer (wallabag-search-buffer))
          (current-filter (minibuffer-contents-no-properties)))
      (when buffer
        (with-current-buffer buffer
          (let ((wallabag-search-filter current-filter))
            (setq wallabag-search-current-page 1)
            (wallabag-search-update-buffer)))))))

(defvar wallabag-search-entries-length 0)

(defun wallabag-search-update-buffer (&optional page)
  "Update the *wallabag-search* buffer listing to match the database.
When FORCE is non-nil, redraw even when the database hasn't changed."
  (interactive)
  (with-current-buffer (wallabag-search-buffer)
    (let* ((inhibit-read-only t)
           (standard-output (current-buffer))
           (id 0)
           (entries (wallabag-search-get-filtered-entries page))
           (len (length entries)))
      (setq wallabag-search-entries-length (cdaar (wallabag-db-select
                                                   :sql `[:select (funcall count id)
                                                          :from
                                                          ,(wallabag-search-parse-filter wallabag-search-filter)])))
      (if wallabag-search-entries-length
          (progn
            (setq wallabag-search-pages (ceiling wallabag-search-entries-length wallabag-search-page-max-rows))
            (erase-buffer)
            (dolist (entry entries)
              (setq id (1+ id))
              (if (<= id wallabag-search-page-max-rows)
                  (funcall wallabag-search-print-entry-function entry)))
            (if (< len wallabag-search-entries-length)
                (dotimes (i wallabag-search-pages)
                  (let ((button-string (format "%d" (1+ i))))
                    (if (equal (string-to-number button-string) wallabag-search-current-page)
                        (add-face-text-property 0 (length button-string) 'wallabag-current-page-button-face t button-string))
                    (insert " " (buttonize button-string #'wallabag-search-more-data (1+ i)) " ") ))
              (insert "End of entries.\n"))
            (goto-char (point-min)))
        (emacsql-close (wallabag-db))
        (error "Database Error, please reopen with `M-x wallabag'")))))

(defun wallabag-search-get-filtered-entries (&optional page)
  "Get wallabag entries by PAGE."
  (wallabag-db-select :sql (wallabag-search-parse-filter wallabag-search-filter :limit wallabag-search-page-max-rows :page page)))

(defcustom wallabag-search-page-max-rows 31
  "The maximum number of entries to display in a single page."
  :group 'wallabag
  :type 'integer)

(defcustom wallabag-show-entry-after-creation nil
  "If non-nil, show the entry after adding it."
  :group 'wallabag
  :type 'boolean)

(defvar wallabag-search-current-page 1
  "The number of current page in the current search result.")

(defvar wallabag-search-pages 0
  "The number of pages in the current search result.")


(defun wallabag-search-more-data (page)
  (let ((inhibit-read-only t))
    (setq wallabag-search-current-page page)
    (beginning-of-line)
    (delete-region (point) (progn (forward-line 1) (point)))
    (wallabag-search-update-buffer page)))


(defun wallabag-search-next-page ()
  (interactive)
  (if (< wallabag-search-current-page wallabag-search-pages)
      (progn
        (setq wallabag-search-current-page (1+ wallabag-search-current-page))
        (wallabag-search-update-buffer wallabag-search-current-page) )
    (message "Last page.")))

(defun wallabag-search-previous-page ()
  (interactive)
  (if (> wallabag-search-current-page 1)
      (progn
        (setq wallabag-search-current-page (1- wallabag-search-current-page))
        (wallabag-search-update-buffer wallabag-search-current-page) )
    (message "First page.")))


(defun wallabag-search-parse-filter (filter &rest properties)
  "Parse the elements of a search FILTER into an emacsql."
  (let ((words (split-string filter " "))
        (id (plist-get properties :id))
        (limit (plist-get properties :limit))
        (count (plist-get properties :count))
        (page (plist-get properties :page)))
    (cond
     (wallabag-group-filteringp (apply #'vector
                 (append '(:select * :from items)
                         `(,@(list :where
                                   `(or
                                     ,@(cl-loop for word in words collect
                                                (cond
                                                 ((string= "Unread" word) `(= is_archived 0))
                                                 ((string= "Starred" word) `(= is_starred 1))
                                                 ((string= "Archive" word) `(= is_archived 1))
                                                 ((string= "All" word) `(!= id 0))
                                                 ((string= "Tags" word) `(!= id 0))
                                                 ((string= "" word) `(!= id 0))
                                                 (t `(= tag ,word))) ) ))


                           :order-by (desc created_at)
                           ,@(when limit
                               (list :limit limit) )
                           ,@(when page
                               (list :offset (* (1- page) wallabag-search-page-max-rows)))))))
     (id `[:select * :from items
           :where (= id ,id)])
     ((or wallabag-live-filteringp (not (string-empty-p wallabag-search-filter) ))
      (apply #'vector
             (append '(:select * :from items)
                     `(,@(list :where
                               `(and
                                 ,@(cl-loop for word in words collect
                                            `(or (like title ,(concat "%" word "%"))
                                                 (like created_at ,(concat "%" word "%"))
                                                 (like domain_name ,(concat "%" word "%"))
                                                 (like tag ,(concat "%" word "%"))))))
                       :order-by (desc created_at)
                       ,@(when limit
                           (list :limit limit) )
                       ,@(when page
                           (list :offset (* (1- page) wallabag-search-page-max-rows)))))))
     (t (apply #'vector
                 (append '(:select * :from items)
                         `(,@(list :where
                                   `(or
                                     ,@(cl-loop for word in words collect
                                                (cond
                                                 ((string= "Unread" word) `(= is_archived 0))
                                                 ((string= "Starred" word) `(= is_starred 1))
                                                 ((string= "Archive" word) `(= is_archived 1))
                                                 ((string= "All" word) `(!= id 0))
                                                 ((string= "Tags" word) `(!= id 0))
                                                 ((string= "" word) `(!= id 0))
                                                 (t `(= tag ,word))) ) ))


                           :order-by (desc created_at)
                           ,@(when limit
                               (list :limit limit) )
                           ,@(when page
                               (list :offset (* (1- page) wallabag-search-page-max-rows))))))))))

(defun wallabag-search-clear-filter ()
  "Clear the fitler keyword."
  (interactive)
  (setq wallabag-group-filteringp nil)
  (setq wallabag-search-current-page 1)
  (wallabag-search-update-buffer-with-keyword ""))

(defun wallabag-search-update-buffer-with-keyword (keyword)
  "Filter the *wallabag-search* buffer with KEYWORD."
  (setq wallabag-search-filter keyword)
  (wallabag-search-update-buffer))

;;; full update
(defun wallabag-full-update ()
  "Perform a full database update."
  (interactive)
  (if (file-exists-p wallabag-db-file)
      (when (yes-or-no-p (format "Are you want to perform full update?"))
        (emacsql-close (wallabag-db))
        (delete-file wallabag-db-file)
        (setq wallabag-db-newp t)
        (wallabag-search-update-and-clear-filter))))

(provide 'wallabag)

;;; wallabag.el --- Emacs wallabag client -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/wallabag.el
;; Keywords: tools
;; Created: 13 April 2021
;; Version: 1.0.0
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
(ignore-errors
  (require 'evil)
  (require 'ivy))

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
  "Wallabag secret"
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

(defcustom wallabag-search-title-max-width 70
  "Maximum column width for titles in the wallabag-search buffer."
  :group 'wallabag
  :type 'integer)

(defcustom wallabag-search-trailing-width 30
  "Space reserved for displaying the feed and tag information."
  :group 'wallabag
  :type 'integer)

(defcustom wallabag-show-sidebar nil
  "Set t to show sidebar when enter *wallabag-search*."
  :group 'wallabag
  :type 'boolean)


(defcustom wallabag-number-of-entries-to-be-retrieved 50
  "Number of entries to be retrieved when run `wallabag-request-entries'."
  :group 'wallabag
  :type 'integer)

(defvar wallabag-full-entries nil
  "List of the all entries currently on database.")

(defvar wallabag-search-entries nil
  "List of the entries currently on display.")

(defvar wallabag-search-filter-active nil
  "When non-nil, wallabag is currently reading a filter from the minibuffer.
When live editing the filter, it is bound to :live.")

(defvar wallabag-search-header-function #'wallabag-search-header
  "Function that returns the string to be used for the wallabag search header.")

(defvar wallabag-search-print-entry-function #'wallabag-search-print-entry--default
  "Function to print entries into the *wallabag-search* buffer.")

(defvar wallabag-all-tags nil)

(defvar wallabag-new-databasep nil)

(defvar wallabag-appname nil)
(defvar wallabag-version nil)
(defvar wallabag-allowed-registration nil)

(defvar wallabag-user-id nil)
(defvar wallabag-user-email nil)
(defvar wallabag-user-created-at nil)
(defvar wallabag-user-updated-at nil)

(defvar wallabag-retrievingp nil)

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
      (cl-function (lambda (&rest args &key _error-thrown &allow-other-keys)
                     ;; one of error is token expires
                     (setq wallabag-token nil)))
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
      (cl-function (lambda (&rest args &key _error-thrown &allow-other-keys)
                     ;; one of error is token expires
                     (setq wallabag-token nil)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq wallabag-user-id (assoc-default 'id data))
                  (setq wallabag-user-email (assoc-default 'email data))
                  (setq wallabag-user-created-at (assoc-default 'created_at data))
                  (setq wallabag-user-updated-at (assoc-default 'updated_at data))
                  (message "Request User Info Done."))))))

(defun wallabag-request-entries(perpage)
  "Request PERPAGE entries, entries that do not exist in the server will be deleted."
  (interactive (list (if wallabag-new-databasep
                         (let ((num (read-from-minibuffer "How many articles you want to retrieve? ")))
                           (if (= (string-to-number (or num "0")) 0)
                               wallabag-number-of-entries-to-be-retrieved
                             (string-to-number num)) )
                       wallabag-number-of-entries-to-be-retrieved)))
  ;; indicate it is retrieving.
  (setq wallabag-retrievingp t)
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
                ("perPage" . ,perpage)
                ("access_token" . ,token))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                 ("Content-Type" . "application/json"))
      :error
      (cl-function (lambda (&rest args &key _error-thrown &allow-other-keys)
                     ;; one of error is token expires
                     (setq wallabag-token nil)))
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
                  ;; delete the non exist entries replied from wallabag server
                  (wallabag-db-delete (vconcat (cl-set-difference
                                                (cl-loop for item in (wallabag-db-sql `[:select id :from items :order :by id :desc :limit ,perpage]) collect
                                                         (car item))
                                                (cl-loop for entry in entries collect
                                                         (alist-get 'id entry)))))
                  ;; insert new entries retried from wallabag server
                  (wallabag-db-insert entries)
                  (setq wallabag-new-databasep nil)
                  (message "Retrived the latest %s articles." (length entries))

                  (with-silent-modifications
                    (wallabag-request-tags)
                    (with-current-buffer (get-buffer-create (wallabag-search-buffer))
                      (setq wallabag-search-filter "")
                      (setq current (point))
                      (setq position (window-start))
                      (erase-buffer)
                      (setq wallabag-search-entries (nreverse (wallabag-db-select)))
                      (setq wallabag-full-entries wallabag-search-entries)
                      (unless (equal wallabag-full-entries '(""))   ; not empty list
                        (cl-loop for entry in wallabag-full-entries do
                                 (funcall wallabag-search-print-entry-function entry)))
                      (wallabag-search-mode)
                      (set-window-start (selected-window) position)
                      (goto-char current)))

                  ;; indicate the retrieving is finished, and update the header
                  (setq wallabag-retrievingp nil))))))

(defun wallabag-request-format (&optional format)
  "TODO: Request the format to be exported."
  (interactive)
  (let* ((entry (get-text-property (point) 'wallabag-entry nil))
         (id (alist-get 'id entry))
         (host wallabag-host)
         (token (or wallabag-token (wallabag-request-token))))
    (request (format "%s/api/entries/%s/export.%s" host id (or format "pdf"))
      :encoding 'binary
      :params `(("access_token" . ,token))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
      :error
      (cl-function (lambda (&rest args &key _error-thrown &allow-other-keys)
                     ;; one of error is token expires
                     (setq wallabag-token nil)))
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


(defun wallabag-request-tags ()
  "Request all tags."
  (interactive)
  (let* ((host wallabag-host)
         (token (or wallabag-token (wallabag-request-token))))
    (request (format "%s/api/tags.json" host)
      :parser 'json-read
      :params `(("access_token" . ,token))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
      :error
      (cl-function (lambda (&rest args &key _error-thrown &allow-other-keys)
                     (setq wallabag-token nil)))
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
                    ;; (message "Retrieved all tags Done")
                    ))))))

(defun wallabag-add-tags(tags)
  "Add TAGS to the entry at point.
TAGS are seperated by comma."
  (interactive (list
                (wallabag-get-tag-name)))
  (let* ((entry (get-text-property (point) 'wallabag-entry) )
         (id (alist-get 'id entry))
         (host wallabag-host)
         (token (or wallabag-token (wallabag-request-token))))
    (request (format "%s/api/entries/%s/tags.json" host id)
      :parser 'json-read
      :type "POST"
      :data `(("access_token" . ,token)
                ("tags" . ,tags))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
      :error
      (cl-function (lambda (&rest args &key _error-thrown &allow-other-keys)
                     ;; one of error is token expires
                     (setq wallabag-token nil)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (wallabag-db-update-tags id (alist-get 'tags data))
                  (wallabag-db-update-tag id (wallabag-convert-tags-to-tag data))
                  (wallabag-refresh-and-resume)
                  (wallabag-request-tags)
                  (message "Add Tags Done"))))))

(defun wallabag-remove-tag()
  "Remove one tag of the entry."
  (interactive)
  (let* ((entry (get-text-property (point) 'wallabag-entry) )
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
                 (mapcar 'cdr tag-list)) tag-list :test 'string= :key 'cdr))))
    (request (format "%s/api/entries/%s/tags/%s.json" host id tag)
      :type "DELETE"
      :parser 'json-read
      :data `(("access_token" . ,token))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
      :error
      (cl-function (lambda (&rest args &key _error-thrown &allow-other-keys)
                     ;; one of error is token expires
                     (setq wallabag-token nil)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (wallabag-db-update-tags id (alist-get 'tags data))
                  (wallabag-db-update-tag id (wallabag-convert-tags-to-tag data))
                  (wallabag-refresh-and-resume)
                  (wallabag-request-tags)
                  (message "Remove Tag Done"))))))

(defun wallabag-add-entry(url tags)
  "Add a new entry by URL and TAGS."
  (interactive (list
                (read-from-minibuffer "What URL do you want to add? ")
                (read-from-minibuffer "How about TAGS? ")))
  (let* ((host wallabag-host)
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
      (cl-function (lambda (&rest args &key _error-thrown &allow-other-keys)
                     ;; one of error is token expires
                     (setq wallabag-token nil)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq data
                        (progn
                          (setf
                           (alist-get 'tag data)
                           (if (stringp (alist-get 'tag data))
                               (alist-get 'tag data)
                             (wallabag-convert-tags-to-tag data)))
                          data))
                  (wallabag-db-insert (list data))
                  (if (eq major-mode 'wallabag-search-mode)
                    (wallabag-search-refresh-and-clear-filter))
                  (message "Add Entry Done"))))))

(defun wallabag-insert-entry(title tags)
  "TODO: Insert a entry by TITLE and TAGS, using current buffer."
  (interactive (list
                (read-from-minibuffer "What TITLE do you want to add? " (buffer-name))
                (read-from-minibuffer "How about TAGS? ")))
  (let* ((host wallabag-host)
         (token (or wallabag-token (wallabag-request-token))))
    (require 'org-id)
    (request (format "%s/api/entries.json" host)
      :parser 'json-read
      :type "POST"
      :data `(("url" . ,(org-id-uuid))
              ("title" . ,title)
              ("content" . ,(format "<pre>%s</pre>" (buffer-string)))
              ("archive" . 0)
              ("starred" . 0)
              ("tags" . ,(format "%s,%s" tags major-mode))
              ("access_token" . ,token))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
      :error
      (cl-function (lambda (&rest args &key _error-thrown &allow-other-keys)
                     ;; one of error is token expires
                     (setq wallabag-token nil)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq data
                        (progn
                          (setf
                           (alist-get 'tag data)
                           (if (stringp (alist-get 'tag data))
                               (alist-get 'tag data)
                             (wallabag-convert-tags-to-tag data)))
                          data))
                  (wallabag-db-insert (list data))
                  (wallabag-search-refresh-and-clear-filter)
                  (message "Add Entry Done"))))))

(defun wallabag-delete-entry()
  "Delete a entry at point."
  (interactive)
  (let* ((entry (get-text-property (point) 'wallabag-entry) )
         (id (alist-get 'id entry))
         (title (alist-get 'title entry))
         (host wallabag-host)
         (token (or wallabag-token (wallabag-request-token))))
    (if (yes-or-no-p (format "Do you really want to Delete \"%s\"?" title))
        (request (format "%s/api/entries/%s.json" host id)
          :parser 'json-read
          :type "DELETE"
          :data `(("access_token" . ,token))
          :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
          :error
          (cl-function (lambda (&rest args &key _error-thrown &allow-other-keys)
                         ;; one of error is token expires
                         (setq wallabag-token nil)))
          :success (cl-function
                    (lambda (&key _data &allow-other-keys)
                      (wallabag-db-delete id)
                      (wallabag-search-refresh-and-clear-filter)
                      (message "Deletion Done")))))))

;; (defun wallabag-search-entries (query)
;;   (or
;;    (ivy-more-chars)
;;    (progn
;;      (let ((host wallabag-host)
;;            (token (or wallabag-token (wallabag-request-token)))
;;            (page 1)
;;            (perpage 30)
;;            entries)
;;        (request (format "%s/api/search.json" host)
;;          :parser 'json-read
;;          :params `(("term" . ,query)
;;                    ("page" . ,page)
;;                    ("perPage" . ,perpage)
;;                    ("access_token" . ,token))
;;          :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
;;                     ("Content-Type" . "application/json"))
;;          :sync t
;;          :success (cl-function
;;                    (lambda (&key data &allow-other-keys)
;;                      (setq entries (wallabag-parse-json-as-list data)))))
;;        entries) ) ))

;; (defun wallabag-parse-json-as-list (json)
;;   (mapcar (lambda(x)
;;             (alist-get 'title x))
;;           (wallabag-parse-json json)))

;;; utils

;; (defun wallabag-get-tag-id ()
;;   (interactive)
;;   (car
;;    (cl-find
;;     (completing-read
;;      "Selete the tag you want to add: "
;;      (mapcar 'cdr wallabag-all-tags)) wallabag-all-tags :test 'string= :key 'cdr)))

(defun wallabag-get-tag-name ()
  "Get the tag name in a list."
  (interactive)
  (completing-read
   "Selete the tag you want to add: "
   (mapcar 'cdr wallabag-all-tags)))


(defun wallabag-parse-json (json)
  (alist-get 'items (assoc '_embedded json)))

;;;###autoload
(defun wallabag-find ()
  "Find the entry from list using ivy."
  (interactive)
  (if (featurep 'ivy)
      (ivy-read "Wallabag: " #'wallabag-parse-entries-as-list
                :dynamic-collection t
                :sort nil
                :action (lambda (cand)
                          (with-ivy-window
                            (wallabag-show-entry (get-text-property 0 'wallabag-entry cand))))
                :caller 'wallagab-find)
    (message "`wallabag-find' only supportes ivy.")))

;;; header

(defun wallabag-search-header ()
  "TODO: Return the string to be used as the wallabag header."
  (format "%s: %s   %s"
          (propertize "Wallabag" 'face font-lock-preprocessor-face)
          (propertize (format "%s" wallabag-host) 'face font-lock-type-face)
          (concat
           (if wallabag-retrievingp
               (propertize "Updating..." 'face font-lock-warning-face)
               (propertize (format "Total: %s"
                                   (if (equal wallabag-search-entries '(""))
                                       "0   "
                                     (concat (number-to-string (length wallabag-search-entries)) "   "))) 'face font-lock-warning-face) )
           (propertize (format "%s" (if (equal wallabag-search-filter "")
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
    (define-key map "s" #'wallabag-search-live-filter)
    (define-key map "q" #'wallabag-search-quit)
    (define-key map "g" #'wallabag-search-refresh-and-clear-filter)
    (define-key map "G" #'wallabag-search-clear-filter)
    (define-key map "u" #'wallabag-search-update-and-clear-filter)
    (define-key map "m" #'wallabag-mark-and-forward)
    (define-key map (kbd "<DEL>") #'wallabag-unmark-and-backward)
    (define-key map "a" #'wallabag-add-entry)
    (define-key map "d" #'wallabag-delete-entry)
    (define-key map "n" #'wallabag-next-entry)
    (define-key map "p" #'wallabag-previous-entry)
    (define-key map "w" #'wallabag-org-link-copy)
    (define-key map "t" #'wallabag-add-tags)
    (define-key map "T" #'wallabag-remove-tag)
    (define-key map "'" #'wallabag-toggle-sidebar)
    map)
  "Keymap for `wallabag-search-mode'.")

(if (featurep 'evil)
    (evil-define-key '(normal emacs) wallabag-search-mode-map
      (kbd "<RET>") 'wallabag-view
      (kbd "v") 'wallabag-view
      (kbd "/") 'wallabag-search-live-filter
      (kbd "q") 'wallabag-search-quit
      (kbd "r") 'wallabag-search-refresh-and-clear-filter
      (kbd "R") 'wallabag-search-clear-filter
      (kbd "u") 'wallabag-search-update-and-clear-filter
      (kbd "m") 'wallabag-mark-and-forward
      (kbd "<DEL>") 'wallabag-unmark-and-backward
      (kbd "a") 'wallabag-add-entry
      (kbd "d") 'wallabag-delete-entry
      (kbd "j") 'wallabag-next-entry
      (kbd "k") 'wallabag-previous-entry
      (kbd "y") 'wallabag-org-link-copy
      (kbd "t") 'wallabag-add-tags
      (kbd "T") 'wallabag-remove-tag
      (kbd "'") 'wallabag-toggle-sidebar))

(define-derived-mode wallabag-search-mode fundamental-mode "wallabag-search"
  "Major mode for listing wallabag entries.
\\{wallabag-search-mode-map}"
  (setq truncate-lines t
        buffer-read-only t
        header-line-format '(:eval (funcall wallabag-search-header-function)))
  (buffer-disable-undo)
  (set (make-local-variable 'hl-line-face) 'wallabag-current-match)
  (hl-line-mode)
  (add-hook 'minibuffer-setup-hook 'wallabag-search--minibuffer-setup))

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
  (let ((cands (if wallabag-search-entries
                  wallabag-search-entries
                (progn
                  (wallabag-request-tags)
                  (setq wallabag-search-entries (nreverse (wallabag-db-select) ))
                  ;; (if (file-exists-p wallabag-json-file)
                  ;;     (setq wallabag-search-entries (append (wallabag-parse-json (json-read-file wallabag-json-file)) nil))
                  ;;   (setq wallabag-search-entries (append (wallabag-request-entries) nil)))
                  ;; convert all entries's tags to string

                  (setq wallabag-full-entries wallabag-search-entries)
                  ;; reqeust all tags
                  ))))
    (unless (equal cands '(""))   ; not empty list
      (cl-loop for entry in cands do
               (funcall wallabag-search-print-entry-function entry))
      (goto-char (point-min)))
    (unless (eq major-mode 'wallabag-search-mode)
      (wallabag-search-mode))
    (if (and wallabag-show-sidebar wallabag-all-tags)
        (wallabag-sidebar-create-window))))

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
      (browse-url (get-text-property (point) 'help-echo)))))

(defun wallabag-ret ()
  "Browser the url with keyboad RET."
  (interactive)
  ;; (message "click mouse-3")
  (browse-url (get-text-property (point) 'help-echo)))

(defun wallabag-view ()
  "View the wallabag entry."
  (interactive)
  (wallabag-show-entry (get-text-property (point) 'wallabag-entry nil)))

(defun wallabag-parse-entry-as-string (entry)
  "Parse the wallabag ENTRY and return as string."
  (let ((title (or (alist-get 'title entry) "NO TITLE"))
        (created-at (alist-get 'created_at entry))
        (reading-time (alist-get 'reading_time entry))
        (tag (alist-get 'tag entry))
        (domain-name (or (alist-get 'domain_name entry) ""))
        (title-width (- (window-width (get-buffer-window "*wallabag-search*") ) 10 wallabag-search-trailing-width)))
    (format "%s %s %s (%s) %s"
            (propertize (substring created-at 0 10) 'face 'wallabag-date-face)
            (propertize (wallabag-format-column
                         title (wallabag-clamp
                                wallabag-search-title-min-width
                                title-width
                                wallabag-search-title-max-width)
                         :left) 'face 'wallabag-title-face)
            (propertize domain-name 'face 'wallabag-domain-name-face)
            (propertize tag 'face 'wallabag-tag-face)
            (propertize (concat (number-to-string reading-time) " min") 'face 'wallabag-reading-time-face))))

(defun wallabag-parse-entries-as-list(filter)
  "Parse all entries with FILTER, return as propertized string list."
  (cl-loop for entry in (wallabag-search--update-list filter) collect
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
      (insert "\n"))))


(defun wallabag-search-quit ()
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
  (wallabag-unmark-at-point)
  (wallabag-previous-entry))

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
  (let ((ori "") (new ""))
    (while (and (equal new ori) new ori)
      (setq ori (alist-get 'id (get-text-property (point) 'wallabag-entry nil)))
      (forward-line 1)
      (setq new (alist-get 'id (get-text-property (point) 'wallabag-entry nil))))))

(defun wallabag-previous-entry ()
  "Move to previous entry."
  (interactive)
  (let ((ori "") (new ""))
    (while (and (equal new ori) new ori (> (line-number-at-pos) 1))
      (forward-line -1)
      (save-excursion
        (setq ori (alist-get 'id (get-text-property (point) 'wallabag-entry nil)))
        (forward-line -1)
        (setq new (alist-get 'id (get-text-property (point) 'wallabag-entry nil)))))))

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
  (setq wallabag-search-entries nil)
  (setq wallabag-full-entries nil)
  (wallabag-search-keyword-filter "")
  (wallabag))

(defun wallabag-search-update-and-clear-filter ()
  "Refresh wallabag and clear the filter keyword."
  (interactive)
  (call-interactively 'wallabag-request-entries)
  (message "Retriving articles from wallabag host %s ..." wallabag-host))

;;; wallabag-entry-mode

(defvar wallabag-entry-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'wallabag-entry-quit)
    map)
  "Keymap for `wallabag-entry-mode'.")

(if (featurep 'evil)
    (evil-define-key '(normal emacs) wallabag-entry-mode-map
      (kbd "q") 'wallabag-entry-quit))

(define-derived-mode wallabag-entry-mode fundamental-mode "wallabag-entry"
  "Mode for displaying wallabag entry details.
\\{wallabag-show-mode-map}"
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defun wallabag-show--buffer-name ()
  "Return the appropriate buffer name for wallabag entry."
  "*wallabag-entry*")

(defun wallabag-show-entry (entry &optional switch)
  "Display ENTRY in the current buffer.
Optional argument SWITCH to switch to *wallabag-entry* buffer to other window."
  (unless (eq major-mode 'wallabag-show-mode)
      (when (get-buffer (wallabag-show--buffer-name))
        (kill-buffer (wallabag-show--buffer-name))))
  (let* ((buff (get-buffer-create (wallabag-show--buffer-name)))
         (original (point))
         (title (or (alist-get 'title entry) "NO TITLE"))
         (reading-time (alist-get 'reading_time entry))
         (created-at (alist-get 'created_at entry))
         (tag (alist-get 'tag entry))
         (domain-name (or (alist-get 'domain_name entry) ""))
         (content (or (alist-get 'content entry) ""))
         (url (alist-get 'url entry))
         beg end)
    (let ((inhibit-read-only t))
      (with-current-buffer buff
        (erase-buffer)
        (insert (propertize title 'face 'wallabag-entry-title-face 'wallabag-entry entry))
        (insert "\n")
        (insert (format "%s  %s  %s%s  %s%s"
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
                        (if (string= tag "")
                            ""
                          (cdr (cl-find ":pushpin:" wallabag-emoji-alist :test 'string= :key 'car)))
                        (propertize tag 'face 'wallabag-tag-face)))
        (insert "\n")
        (insert "\n")
        (setq beg (point))
        (insert content)
        (setq end (point))
        (require 'shr)
        (shr-render-region beg end)
        ;; (setq end (point))
        (wallabag-entry-mode)
        (goto-char (point-min))))
    (unless (eq major-mode 'wallabag-entry-mode)
      (funcall wallabag-show-entry-switch buff)
      (when switch
        (switch-to-buffer-other-window (set-buffer (wallabag-search-buffer)))
        (goto-char original)))))

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
        (kill-buffer "*wallabag-entry*")))))


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
      (kbd "r") 'wallabag-search-clear-filter
      (kbd "R") 'wallabag-search-clear-filter
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

(defun wallabag-sidebar-refresh ()
   (with-current-buffer wallabag-sidebar-buffer
    (with-silent-modifications
      (setq header-line-format (list :propertize "Tags" 'face 'bold))
      (erase-buffer)
      (dolist (tag wallabag-all-tags)
        (insert (cdr tag))
        (insert "\n")))))

(defun wallabag-sidebar-quit ()
  (interactive)
  (if (window-live-p (get-buffer-window wallabag-sidebar-buffer))
      (delete-window (get-buffer-window wallabag-sidebar-buffer))))

(defun wallabag-sidebar-find-tag ()
  "Filter by tag at point."
  (interactive)
  (wallabag-search-keyword-filter (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun wallabag-sidebar-find-next-tag ()
  "Filter by next tag at point."
  (interactive)
  (forward-line 1)
  (wallabag-search-keyword-filter (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun wallabag-sidebar-find-previous-tag ()
  "Filter by previous tag at point."
  (interactive)
  (forward-line -1)
  (wallabag-search-keyword-filter (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

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
ebook record will be shown.

1. Live filter is search the results in =wallabag-full-entries=
rather than query the database.
2. The keyword supports REGEX.
3. Inserting Spaces between
   keywords can narrow down the search results."

  (interactive)
  (unwind-protect
      (let ((wallabag-search-filter-active :live))
        (setq wallabag-search-filter
              (read-from-minibuffer "Filter: " wallabag-search-filter))
        (message wallabag-search-filter))
    (wallabag-search-update :force)))

(defun wallabag-search--minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when wallabag-search-filter-active
    (when (eq :live wallabag-search-filter-active)
      (add-hook 'post-command-hook 'wallabag-search--live-update nil :local))))

(defun wallabag-search--live-update ()
  "Update the *wallabag-search* buffer based on the contents of the minibuffer."
  (when (eq :live wallabag-search-filter-active)
    ;; (message "HELLO")
    (let ((buffer (wallabag-search-buffer))
          (current-filter (minibuffer-contents-no-properties)))
      (when buffer
        (with-current-buffer buffer
          (let ((wallabag-search-filter current-filter))
            (wallabag-search-update :force)))))))

(defun wallabag-search-update (&optional force)
  "Update the *wallabag-search* buffer listing to match the database.
When FORCE is non-nil, redraw even when the database hasn't changed."
  (interactive)
  (with-current-buffer (wallabag-search-buffer)
    (when force
      (let ((inhibit-read-only t)
            (standard-output (current-buffer)))
        (erase-buffer)
        (wallabag-search--update-list)
        ;; (setq wallabag-search-entries (wallabag-db-select))
        (dolist (entry wallabag-search-entries)
          (funcall wallabag-search-print-entry-function entry))
        ;; (insert "End of entries.\n")
        (goto-char (point-min))         ; back to point-min after filtering
        (setf wallabag-search-last-update (float-time))))))
(defun wallabag-search--update-list (&optional filter)
  "Update `wallabag-search-entries' list."
  ;; replace space with _ (SQL) The underscore represents a single character
  (if filter
      (setq wallabag-search-filter filter)
    (setq filter wallabag-search-filter))
  (let* ((filter (wallabag-search-parse-filter filter)) ;; (replace-regexp-in-string " " "_" wallabag-search-filter)
         (head (wallabag-candidate-filter filter)))
    ;; Determine the final list order
    (let ((entries head))
      (setf wallabag-search-entries
            entries))))

(defun wallabag-search-parse-filter (filter)
  "Parse the elements of a search FILTER into an alist."
  (let ((matches ()))
    (cl-loop for element in (split-string filter) collect
             (when (wallabag-valid-regexp-p element)
               (push element matches)))
    `(,@(if matches
            (list :matches matches)))))

(defun wallabag-valid-regexp-p (regexp)
  "Return t if REGEXP is a valid REGEXP."
  (ignore-errors
    (prog1 t
      (string-match-p regexp ""))))

(defun wallabag-candidate-filter (filter)
  "Generate ebook candidate alist.
ARGUMENT FILTER is the filter string."
  (let ((matches (plist-get filter :matches))
        res-list)
    (cl-loop for entry in wallabag-full-entries do
             (if (eval `(and ,@(cl-loop for regex in matches collect
                                        (or
                                         (string-match-p regex (or (alist-get 'title entry) ""))
                                         (string-match-p regex (alist-get 'created_at entry))
                                         (string-match-p regex (or (alist-get 'domain_name entry) "") )
                                         (string-match-p regex (alist-get 'tag entry))))))
                 (push entry res-list)))
    (nreverse res-list)))

(defun wallabag-search-clear-filter ()
  "Clear the fitler keyword."
  (interactive)
  (wallabag-search-keyword-filter ""))

(defun wallabag-search-keyword-filter (keyword)
  "Filter the *wallabag-search* buffer with KEYWORD."
  (setq wallabag-search-filter keyword)
  (wallabag-search-update :force))

;;; full update
(defun wallabag-full-update ()
  "Perform a full database update."
  (interactive)
  (if (file-exists-p wallabag-db-file)
      (when (yes-or-no-p (format "Are you want to perform full update?"))
        (emacsql-close (wallabag-db))
        (delete-file wallabag-db-file)
        (setq wallabag-new-databasep t)
        (wallabag-search-update-and-clear-filter))))

(provide 'wallabag)

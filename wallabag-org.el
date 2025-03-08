;;; wallabag-org.el --- Emacs wallabag client org related features -*- lexical-binding: t; -*-

(require 'ol)
(require 'wallabag-util)
(require 'wallabag-db)

(declare-function wallabag-show-entry "calibredb.el")

(defun wallabag-org-link-copy ()
  "Copy the marked items as wallabag org links."
  (interactive)
  (let ((candidates (wallabag-find-marked-candidates)))
    (unless candidates
      (setq candidates (list (wallabag-find-candidate-at-point))))
    (kill-new
     (with-temp-buffer
       (dolist (cand candidates)
         (let ((id (alist-get 'id cand))
               (title (alist-get 'title cand)))
           (insert (format "[[wallabag:%s][%s]]" id title) (if (> (length candidates) 1) "\n" ""))
           (message "Copied: %s - \"%s\" as wallabag org link." id title)))
       (buffer-string)))
    ;; remove overlays and text properties
    (let* ((beg (point-min))
           (end (point-max))
           (inhibit-read-only t))
      (remove-overlays beg end)
      (remove-text-properties beg end '(wallabag-mark nil)))))

(defun wallabag-org-markdown-copy ()
  "Copy the marked items as markdown links."
  (interactive)
  (let ((candidates (wallabag-find-marked-candidates)))
    (unless candidates
      (setq candidates (list (wallabag-find-candidate-at-point))))
    (kill-new
     (with-temp-buffer
       (dolist (cand candidates)
         (let* ((url (alist-get 'url cand))
                (title (alist-get 'title cand))
                (org-protocol-link (format "[%s](%s)" title url)))
           ;; (insert (format "[[wallabag:%s][%s]]\n" id title))
           (insert org-protocol-link (if (> (length candidates) 1) "\n" ""))
           (message "Copied: %s" org-protocol-link)))
       (buffer-string)))
    ;; remove overlays and text properties
    (let* ((beg (point-min))
           (end (point-max))
           (inhibit-read-only t))
      (remove-overlays beg end)
      (remove-text-properties beg end '(wallabag-mark nil)))))


(defun wallabag-org-title-copy ()
  "Copy the marked items' titles."
  (interactive)
  (let ((candidates (wallabag-find-marked-candidates)))
    (unless candidates
      (setq candidates (list (wallabag-find-candidate-at-point))))
    (kill-new
     (with-temp-buffer
       (dolist (cand candidates)
         (let* ((title (alist-get 'title cand)))
           (insert title (if (> (length candidates) 1) "\n" ""))
           (message "Copied: %s" title)))
       (buffer-string)))
    ;; remove overlays and text properties
    (let* ((beg (point-min))
           (end (point-max))
           (inhibit-read-only t))
      (remove-overlays beg end)
      (remove-text-properties beg end '(wallabag-mark nil)))))

(defun wallabag-org-url-copy ()
  "Copy the marked items' urls."
  (interactive)
  (let ((candidates (wallabag-find-marked-candidates)))
    (unless candidates
      (setq candidates (list (wallabag-find-candidate-at-point))))
    (kill-new
     (with-temp-buffer
         (dolist (cand candidates)
         (let* ((url (alist-get 'url cand)))
           (insert url (if (> (length candidates) 1) "\n" ""))
           (message "Copied: %s" url)))
         (buffer-string)))
    ;; remove overlays and text properties
    (let* ((beg (point-min))
           (end (point-max))
           (inhibit-read-only t))
      (remove-overlays beg end)
      (remove-text-properties beg end '(wallabag-mark nil)))))


(defun wallabag-org-protocol-link-markdown-copy ()
  "Copy the marked items as org-protocol markdown links."
  (interactive)
  (let ((candidates (wallabag-find-marked-candidates)))
    (unless candidates
      (setq candidates (list (wallabag-find-candidate-at-point))))
    (kill-new
     (with-temp-buffer
       (dolist (cand candidates)
         (let* ((id (alist-get 'id cand))
                (title (alist-get 'title cand))
                (org-protocol-link (format "[%s](%s)" title (url-encode-url (format "org-protocol://wallabag?id=%s&title=%s" id title) ))  ))
           ;; (insert (format "[[wallabag:%s][%s]]\n" id title))
           (insert org-protocol-link (if (> (length candidates) 1) "\n" ""))
           (message "Copied: %s" org-protocol-link)))
       (buffer-string)))
    ;; remove overlays and text properties
    (let* ((beg (point-min))
           (end (point-max))
           (inhibit-read-only t))
      (remove-overlays beg end)
      (remove-text-properties beg end '(wallabag-mark nil)))))


(defun wallabag-org-protocol-link-copy ()
  "Copy the marked items as org-protocol links."
  (interactive)
  (let ((candidates (wallabag-find-marked-candidates)))
    (unless candidates
      (setq candidates (list (wallabag-find-candidate-at-point))))
    (kill-new
     (with-temp-buffer
       (dolist (cand candidates)
         (let* ((id (alist-get 'id cand))
                (title (alist-get 'title cand))
                (org-protocol-link (url-encode-url (format "org-protocol://wallabag?id=%s&title=%s" id title) ) ))
           ;; (insert (format "[[wallabag:%s][%s]]\n" id title))
           (insert org-protocol-link (if (> (length candidates) 1) "\n" ""))
           (message "Copied: %s" org-protocol-link)))
       (buffer-string)))
    ;; remove overlays and text properties
    (let* ((beg (point-min))
           (end (point-max))
           (inhibit-read-only t))
      (remove-overlays beg end)
      (remove-text-properties beg end '(wallabag-mark nil)))))

;;;###autoload
(defun wallabag-org-link-view (id _)
  "Follow wallabag org links."
  (let ((entry (wallabag-db-select :id (string-to-number id))))
    (if entry
        (wallabag-show-entry (car entry))
      (message "No this entry."))))

;; TODO: The description can not be set.
(org-link-set-parameters
 "wallabag"
 :follow #'wallabag-org-link-view
 :face 'wallabag-org-link)


(defun wallabag-org-protocol (data)
  (let* ((id (plist-get data :id))
         (url (org-protocol-sanitize-uri (or (plist-get data :url) "")))
         (title (or (wallabag-capture-html--nbsp-to-space (string-trim (or (plist-get data :title) ""))) ""))
         (content (or (wallabag-capture-html--nbsp-to-space (string-trim (or (plist-get data :body) ""))) "")))
    (if id
        (wallabag-show-entry (car (wallabag-db-select :id (string-to-number id))))
      ;; if we `paw-server-html-file' exists, we use it to insert the entry
      (if (boundp 'paw-server-html-file)
          (when (file-exists-p paw-server-html-file)
            (wallabag-insert-entry url title (with-temp-buffer
                                               (insert-file-contents paw-server-html-file)
                                               (buffer-string)))
            (delete-file paw-server-html-file))
        (wallabag-insert-entry url title content)))
    nil))

(defun wallabag-capture-html--nbsp-to-space (s)
  "Convert HTML non-breaking spaces to plain spaces in S."
  ;; Not sure why sometimes these are in the HTML and Pandoc converts
  ;; them to underlines instead of spaces, but this fixes it.
  (replace-regexp-in-string (rx "&nbsp;") " " s t t))

(defun wallabag-org-setup-org-protocol()
  (require 'org-protocol)
  (add-to-list 'org-protocol-protocol-alist '("wallabag"
                                              :protocol "wallabag"
                                              :function wallabag-org-protocol
                                              :kill-client t)))

;; Add the following setup to your init file to enable org-protocol
;; (wallabag-org-setup-org-protocol)

(provide 'wallabag-org)

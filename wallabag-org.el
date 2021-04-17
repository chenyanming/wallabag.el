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
           (insert (format "[[wallabag:%s][%s]]\n" id title))
           (message "Copied: %s - \"%s\" as wallabag org link." id title)))
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
  (let ((entry (wallabag-db-select (string-to-number id))))
    (if entry
        (wallabag-show-entry (car entry))
      (message "No this entry."))))

;; TODO: The description can not be set.
(org-link-set-parameters
 "wallabag"
 :follow #'wallabag-org-link-view
 :face 'wallabag-org-link)

(provide 'wallabag-org)

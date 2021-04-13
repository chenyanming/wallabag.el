;;; wallabag-utils.el --- Emacs wallabag client utils -*- lexical-binding: t; -*-

(defvar wallabag-emoji-alist nil)
(defvar wallabag-emoji-candidates nil)
(defvar wallabag-emoji-max-length 0)
(defvar wallabag-emoji-svg-images nil
  "Cache of SVG images for emojis of one char height.
Alist with elements in form (emoji . image)")
(defvar wallabag-emoji-file (concat (file-name-directory load-file-name) "emojis.alist"))

(defcustom wallabag-emoji-custom-alist nil
  "*Alist of custom emojis to add along with `etc/emojis.alist'."
  :type 'alist
  :group 'wallabag)

(defun wallabag-emoji-init ()
  "Initialize emojis."
  (unless wallabag-emoji-alist
    (setq wallabag-emoji-alist
          (nconc (with-temp-buffer
                   (insert-file-contents wallabag-emoji-file)
                   (goto-char (point-min))
                   (read (current-buffer)))
                 wallabag-emoji-custom-alist))
    (setq wallabag-emoji-candidates (mapcar 'car wallabag-emoji-alist))
    (setq wallabag-emoji-max-length
          (apply 'max (mapcar 'length wallabag-emoji-candidates)))))

(defun wallabag-emoji-name (emoji)
  "Find EMOJI name."
  (wallabag-emoji-init)
  (car (cl-find emoji wallabag-emoji-alist :test 'string= :key 'cdr)))

(defun wallabag-convert-tags-to-tag (entry)
  "Convert the tags array to tag strings, seperated by comma."
  (mapconcat
   #'identity
   (mapcar
    ;; get label of each element
    (lambda(x)
      (alist-get 'label x))
    ;; get tags vector
    (alist-get 'tags entry))
   ;; concat with ,
   ","))

;;; find candidates

(defun wallabag-find-candidate-at-point ()
  "Find candidate at point and return the list."
  (interactive)
  (get-text-property (point) 'wallabag-entry))

(defun wallabag-find-marked-candidates ()
  "Find marked candidates and return the alist."
  (interactive)
  (save-excursion
    (let (candidate beg end cand-list)
      (when (text-property-not-all (point-min) (point-max) 'wallabag-mark nil)
        (setq end (text-property-any (point-min) (point-max) 'wallabag-mark ?>))
        (while (setq beg (text-property-any end (point-max) 'wallabag-mark ?>) )
          (goto-char beg)
          (setq candidate (wallabag-find-candidate-at-point))
          (push candidate cand-list)
          ;; (message (number-to-string beg))
          (forward-line 1)
          (setq end (point)))
        cand-list))))

;;; format
(defun wallabag-format-column (string width &optional align)
  "Return STRING truncated or padded to WIDTH following ALIGNment.
Align should be a keyword :left or :right."
  (if (<= width 0)
      ""
    (format (format "%%%s%d.%ds" (if (eq align :left) "-" "") width width)
            string)))

(defun wallabag-clamp (min value max)
  "Clamp a value between two values."
  (min max (max min value)))

(provide 'wallabag-util)

;;; wallabag-utils.el --- Emacs wallabag client utils -*- lexical-binding: t; -*-

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

(require 'compile)

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
  "Convert the tags array to tag strings, seperated by comma.
ENTRY is the entry alist."
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
  (get-text-property (if (eq major-mode 'wallabag-entry-mode) (point-min) (point)) 'wallabag-entry))

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

(defun wallabag-find-candidate-location (id)
  "Find candidate location by ID.
Return value of point, as an integer.."
  (text-property-any (point-min) (point-max) 'wallabag-id id))

(defun wallabag-flash-show (pos end-pos face delay)
  "Flash a temporary highlight to help the user find something.
POS start position

END-POS end position, flash the characters between the two
points

FACE the flash face used

DELAY the flash delay"
  (when (and (numberp delay)
             (> delay 0))
    ;; else
    (when (timerp next-error-highlight-timer)
      (cancel-timer next-error-highlight-timer))
    (setq compilation-highlight-overlay (or compilation-highlight-overlay
                                            (make-overlay (point-min) (point-min))))
    (overlay-put compilation-highlight-overlay 'face face)
    (overlay-put compilation-highlight-overlay 'priority 10000)
    (move-overlay compilation-highlight-overlay pos end-pos)
    (add-hook 'pre-command-hook #'compilation-goto-locus-delete-o)
    (setq next-error-highlight-timer
          (run-at-time delay nil #'compilation-goto-locus-delete-o))))

;;; format
(defun wallabag-format-column (string width &optional align)
  "Return STRING truncated or padded to WIDTH following ALIGNment.
ALIGN should be a keyword :left or :right."
  (if (<= width 0)
      ""
    (format (format "%%%s%d.%ds" (if (eq align :left) "-" "") width width)
            string)))

(defun wallabag-clamp (min value max)
  "Clamp a VALUE between MIN and MAX."
  (min max (max min value)))

(provide 'wallabag-util)

;;; wallabag-util.el ends here

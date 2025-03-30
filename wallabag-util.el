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

(defvar wallabag-current-cache nil
  "Current cache of the wallabag entry.")

(defvar wallabag-response-overlay nil
  "Overlay for displaying GPTel streaming responses dynamically.")

(defcustom wallabag-cache-file
  (expand-file-name (concat user-emacs-directory "wallabag-cache.el"))
  "File to store wallabag cache."
  :type 'file
  :group 'wallabag)

(defvar wallabag-cache-table
  (if (file-exists-p wallabag-cache-file)
      (condition-case nil
          (let ((coding-system-for-read 'utf-8))
            (with-temp-buffer
              (insert-file-contents wallabag-cache-file)
              (goto-char (point-min))
              (read (current-buffer))))
        (error (message "Could not sync wallabag cache, starting over.")
               (make-hash-table :size 2048 'equal)))
    (make-hash-table :size 2048 :test 'equal))
  "Hash table to store wallabag cache.")

(defcustom wallabag-summary-prompts '(("一句话总结" . "用一句清晰且事实的句子总结这篇文章，抓住核心信息，不重复标题。使用简体中文撰写。")
                                      ("一般总结" . "以清晰简洁的方式总结这篇文章的关键点，突出主要论点、发现和结论。使用简体中文撰写。")
                                      ("要点列表总结" . "提供一份这篇文章主要内容的要点列表。仅用点列回答！使用简体中文撰写。")
                                      ("客观 vs. 观点分析" . "通过区分客观事实和作者的观点来总结这篇文章，明确区分两者。使用简体中文撰写。")
                                      ("简化总结" . "用简单易懂的语言总结这篇文章，就像向非专业人士解释一样。使用简体中文撰写。")
                                      ("用模板总结" . wallabag-summary-template-chinese)
                                      ("One sentence summary" . "Summarize this article in one clear and factual sentence, capturing the core message without repeating the title.")
                                      ("General summary" . "Summarize the key points of this article in a clear and concise manner, highlighting the main arguments, findings, and conclusions.")
                                      ("Bullet list summary" . "Provide a bullet-point summary of the main takeaways from this article. Answer only in bullets!")
                                      ("Objective vs. Opinion analysis" . "Summarize this article by separating objective facts from the author's opinions, clearly distinguishing the two.")
                                      ("Simplified summary" . "Summarize this article in simple, easy-to-understand language as if explaining to a non-expert.")
                                      ("Summarize with Template" . wallabag-summary-template-english))
  "Prompts for the summary."
  :type 'alist
  :group 'wallabag)

(defcustom wallabag-summary-template-english
  (concat "Summarize the highlights of the content and output a useful summary in a few sentences, your output should use the following template:\n"
          "*** Summary\n"
          "**** Highlights\n"
          "- [Emoji] Bulletpoint\n\n"
          "Your task is to summarize the text I have given you in up to seven concise bullet points, starting with a short highlight. "
          "Choose an appropriate emoji for each bullet point. "
          "Use the text above: {{Title}} {{Transcript}}.\n")
  "English Template for Wallabag summaries."
  :type 'string
  :group 'wallabag)

(defcustom wallabag-summary-template-chinese
  (concat
   "请阅读以下文章并生成摘要"
   "文章标题：[标题]"
   "摘要要求："
   "去掉时间，作者，网站。"
   "使用简体中文撰写。"
   "提取文章的主要信息，包括关键观点、重要数据或技术内容。"
   "语言简洁流畅，适合快速阅读。"
   "若文章涉及技术概念，保持准确性并避免过度简化。"
   "若适用，请提供简短的背景信息以帮助理解。")
  "Chinese Template for Wallabag summaries."
  :type 'string
  :group 'wallabag)

(defcustom wallabag-hr-length 60
  "Length of the horizontal rule."
  :type 'integer
  :group 'wallabag)

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
    (setq wallabag-emoji-candidates (mapcar #'car wallabag-emoji-alist))
    (setq wallabag-emoji-max-length
          (apply #'max (mapcar #'length wallabag-emoji-candidates)))))

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
  (get-text-property (if (derived-mode-p 'wallabag-entry-mode) (point-min) (point)) 'wallabag-entry))

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

(defun wallabag-current-cache-save (field value)
  "Save FIELD with VALUE to `wallabag-current-cache`."
  (let* ((entry (wallabag-find-candidate-at-point))
         (id (alist-get 'id entry))
         (cache (gethash id wallabag-cache-table)))
    (setq wallabag-current-cache cache)
    (unless (hash-table-p wallabag-current-cache)
      (setq wallabag-current-cache (make-hash-table :test 'equal)))
    (puthash field value wallabag-current-cache) ))

(defun wallabag-cache-save ()
  "Save a copy of the current cache to the cache table."
  (when-let ((id (alist-get 'id (wallabag-find-candidate-at-point))))
    ;; Store a fresh copy instead of a reference
    (puthash id (copy-hash-table wallabag-current-cache) wallabag-cache-table)))


(defun wallabag-cache-write ()
  "Write the cache table to the cache file."
  (when (and (boundp 'wallabag-cache-table)
             (hash-table-p wallabag-cache-table)
             (not (hash-table-empty-p wallabag-cache-table)))
    (let ((write-region-inhibit-fsync t)
          (coding-system-for-write 'utf-8)
          (print-level nil)
          (print-length nil))
      (with-temp-file wallabag-cache-file
        (insert ";;; -*- lisp-data -*-\n"
   (prin1-to-string wallabag-cache-table))))))

(defun wallabag-get-cache(field)
  "Get the FIELD of the current entry from cache."
  (let* ((entry (wallabag-find-candidate-at-point))
         (id (alist-get 'id entry))
         (cache (gethash id wallabag-cache-table)))
    (setq wallabag-current-cache cache)
    (if (hash-table-p cache)
        (gethash field cache)
      nil)))

(defun wallabag-create-or-update-overlay (response &optional no-save)
  "Create or update the dynamic overlay with RESPONSE.
Optional argument NO-SAVE Don't save to cache."
  (with-current-buffer "*wallabag-entry*"
    (save-excursion
      ;; Move to the designated position (after line 2)
      (if (stringp response)
          (progn
            (goto-char (point-min))
            (forward-line 2)
            (let ((start (line-beginning-position))
                  (end (line-end-position)))
              ;; Create the overlay if it doesn't exist
              (unless (overlayp wallabag-response-overlay)
                (setq wallabag-response-overlay (make-overlay start end))
                (overlay-put wallabag-response-overlay 'after-string ""))

              ;; Append new text to the existing overlay content
              (overlay-put wallabag-response-overlay
                           'after-string
                           (concat (overlay-get wallabag-response-overlay 'after-string)
                                   (propertize response 'face (list :height 0.9))))) )
        (unless no-save
          (wallabag-current-cache-save 'summary (overlay-get wallabag-response-overlay 'after-string))
          (wallabag-cache-save)
          (wallabag-cache-write)
          (message "Summary saved."))
        (overlay-put wallabag-response-overlay
                     'after-string
                     (concat (overlay-get wallabag-response-overlay 'after-string)
                             "\n"
                             (propertize (make-string (min wallabag-hr-length (window-width)) ?_) 'face 'wallabag-hr-face)
                             "\n"))))))

(defun wallabag-remove-summary-overlay ()
  "Remove the summary overlay."
  (when (overlayp wallabag-response-overlay)
    (delete-overlay wallabag-response-overlay)
    (setq wallabag-response-overlay nil)))

(defun wallabag-summary (arg)
  "Get the summary of the current entry using gptel.
Argument ARG Force to generate summary."
  (interactive "P")
  (require 'gptel)
  (let* ((summary (wallabag-get-cache 'summary))
         (prompt (if (and summary (not arg))
                     ""
                   (assoc-default
                    (completing-read "Select a prompt: " wallabag-summary-prompts nil t)
                    wallabag-summary-prompts)))
         (full-query (format "%s\n%s" prompt (buffer-string))))
    (wallabag-remove-summary-overlay)
    (if (and summary (not arg))
        (progn
          (wallabag-create-or-update-overlay summary t)
          (wallabag-create-or-update-overlay t t))
      (gptel-request full-query
        :stream t
        :callback
        (lambda (response _)
          (when response
            (wallabag-create-or-update-overlay response)
            (gptel--sanitize-model)
            (gptel--update-status " Ready" 'success)))))))

(defun wallabag-save-place ()
  "Save the current point to the cache."
  (when (eq major-mode 'wallabag-entry-mode)
    (wallabag-current-cache-save 'point (point))
    (wallabag-current-cache-save 'window-position (window-start))
    (wallabag-cache-save)
    (wallabag-cache-write)))

(provide 'wallabag-util)

;;; wallabag-util.el ends here

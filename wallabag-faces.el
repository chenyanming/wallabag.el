;;; wallabag-faces.el --- Emacs wallabag client faces -*- lexical-binding: t; -*-

(defface wallabag-current-match
  '((((class color) (background light))
     :background "#D8DEE9" :extend t)
    (((class color) (background dark))
     :background "#272c35" :extend t))
  "Face used by Ivy for highlighting the current match.")

(defface wallabag-id-face
  '((((class color) (background light))
     :foreground "#3B6EA8")
    (((class color) (background dark))
     :foreground "#81A1C1")
    (t :inherit default))
  "Face used for id."
  :group 'wallabag-faces)

(defface wallabag-title-face '((t :inherit default))
  "Face used for title on compact view."
  :group 'wallabag-faces)

(defface wallabag-tag-face
  '((((class color) (background light))
     :foreground "brown")
    (((class color) (background dark))
     :foreground "#EBCB8B")
    (t :inherit default))
  "Face used for tag."
  :group 'wallabag-faces)

(defface wallabag-domain-name-face
  '((((class color) (background light))
     :foreground "#3B6EA8")
    (((class color) (background dark))
     :foreground "#d9c6d6")
    (t :inherit default))
  "Face used for author."
  :group 'wallabag-faces)

(defface wallabag-reading-time-face
  '((((class color) (background light))
     :foreground "#8b94a5")
    (((class color) (background dark))
     :foreground "#6f7787")
    (t :inherit default))
  "Face used for size."
  :group 'wallabag-faces)

(defface wallabag-date-face
  '((((class color) (background light))
     :foreground "#29838D")
    (((class color) (background dark))
     :foreground "#8FBCBB")
    (t :inherit default))
  "Face for the date (last_modified)."
  :group 'wallabag-faces)

(defface wallabag-entry-title-face '((t :inherit default :height 3.0))
  "Face used for title on compact view."
  :group 'wallabag-faces)

(defface wallabag-mark-face '((t :inherit highlight))
  "Face for the mark candidate."
  :group 'wallabag-faces)

(defface wallabag-sidebar-face
  '((t nil))
  "Default base-level face for `wallabag-sidebar-mode' buffers.")

(provide 'wallabag-faces)

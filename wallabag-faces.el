;;; wallabag-faces.el --- Emacs wallabag client faces -*- lexical-binding: t; -*-

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

(defface wallabag-current-match
  '((((class color) (background light))
     :background "#D8DEE9" :extend t)
    (((class color) (background dark))
     :background "#272c35" :extend t))
  "Face used by Ivy for highlighting the current match."
  :group 'wallabag-faces)

(defface wallabag-id-face
  '((((class color) (background light))
     :foreground "#3B6EA8")
    (((class color) (background dark))
     :foreground "#81A1C1")
    (t :inherit default))
  "Face used for id."
  :group 'wallabag-faces)

(defface wallabag-title-face
  '((((class color) (background light))
     (:height 1.0))
    (((class color) (background dark))
     (:height 1.0))
    (t (:inherit default)))
  "Face used for title."
  :group 'wallabag-faces)

(defface wallabag-content-face '((t :inherit default))
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

(defface wallabag-entry-title-face
  '((((class color) (background light))
     (:foreground "#2e3440"
      :weight bold
      :height 1.2))
    (((class color) (background dark))
     (:foreground "pale turquoise"
      :weight bold
      :height 1.2))
    (t (:inherit default)))
  "Face used for title."
  :group 'wallabag-faces)

(defface wallabag-mark-face '((t :inherit highlight))
  "Face for the mark candidate."
  :group 'wallabag-faces)

(defface wallabag-sidebar-face '((t nil))
  "Default base-level face for `wallabag-sidebar-mode' buffers."
  :group 'wallabag-faces)

(defface wallabag-org-link '((t :inherit link :foreground "#94b533"))
  "Face for links."
  :group 'wallabag-faces)

(defface wallabag-archive-face
  '((((class color) (background light))
     :foreground "grey"
     :weight light)
    (((class color) (background dark))
     :foreground "dim grey"
     :weight light)
    (t :inherit default))
  "Face used for archive."
  :group 'wallabag-faces)

(defface wallabag-starred-face
  '((((class color) (background light))
     :foreground "black")
    (((class color) (background dark))
     :foreground "yellow")
    (t :inherit default))
  "Face used for title."
  :group 'wallabag-faces)

(defface wallabag-current-page-button-face
  '((((class color) (background light))
     (:weight bold
      :height 1.1))
    (((class color) (background dark))
     (:weight bold
      :height 1.1))
    (t (:inherit default)))
  "Face used for current page button."
  :group 'wallabag-faces)

(defface wallabag-hr-face
  '((default :inherit org-hide)
    (((background light)) :strike-through "gray70")
    (t :strike-through "gray30"))
  "Face used for horizontal ruler.")

(provide 'wallabag-faces)

;;; wallabag-faces.el ends here

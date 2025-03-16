;;; wallabag/wallbag-test.el -*- lexical-binding: t; -*-

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


;; (wallabag-db)
;;



(wallabag-insert  (append (wallabag-parse-json (json-read-file wallabag-json-file)) nil))


;; SELECT * FROM items WHERE items.id in (501, 502);
;; DELETE FROM items WHERE items.id = 501


(elp-instrument-function 'wallabag )
(elp-instrument-function 'wallabag-search-get-filtered-entries )
(elp-instrument-function 'wallabag-db-select )
(elp-instrument-function 'wallabag-search-print-entry--default )

(elp-instrument-function 'wallabag-search-update-buffer)


(elp-reset-all)
(elp-results)

(elp-restore-all)

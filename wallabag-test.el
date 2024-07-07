;;; wallabag/wallbag-test.el -*- lexical-binding: t; -*-



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

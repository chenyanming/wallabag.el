;;; wallabag/wallbag-test.el -*- lexical-binding: t; -*-



;; (wallabag-db)
;;



(wallabag-insert  (append (wallabag-parse-json (json-read-file wallabag-json-file)) nil))


;; SELECT * FROM items WHERE items.id in (501, 502);
;; DELETE FROM items WHERE items.id = 501

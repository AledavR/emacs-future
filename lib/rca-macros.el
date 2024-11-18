;; [[file:../dotemacs.org::*Macros][Macros:1]]
(provide 'rca-macros)

(defmacro toggle-p (var)
  "Toggles a boolean variable"
  `(if (booleanp ,var) 
       (setq ,var (not ,var))))
;; Macros:1 ends here

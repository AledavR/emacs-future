;; [[file:../dotemacs.org::*Tools][Tools:1]]
(provide 'rca-tools)
;; Tools:1 ends here

;; [[file:../dotemacs.org::*Ebuku][Ebuku:1]]
(use-package ebuku
  :ensure t
  :defer t
  :bind ("C-z b" . ebuku)
  :custom-face
  (ebuku-tags-face ((t (:inherit font-lock-keyword-face))))
  (ebuku-title-face ((t (:inherit font-lock-constant-face))))
  :custom
  (ebuku-results-limit 25))
;; Ebuku:1 ends here

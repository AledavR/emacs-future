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

;; [[file:../dotemacs.org::*Denote][Denote:1]]
(use-package denote
  :ensure t)
;; Denote:1 ends here

;; [[file:../dotemacs.org::*Citar][Citar:1]]
(use-package citar
  :ensure t
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :config
  (setq citar-file-note-extensions '("org")
        citar-notes-paths '("~/Documents/articles/notes/")
        citar-library-paths '("~/Documents/articles/")
        citar-library-file-extensions '("pdf")
        citar-bibliography '("~/Documents/bibliography.bib")))
;; Citar:1 ends here

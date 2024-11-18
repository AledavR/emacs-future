;; [[file:../dotemacs.org::*Project management][Project management:1]]
(provide 'rca-project)
;; Project management:1 ends here

;; [[file:../dotemacs.org::*Transient][Transient:1]]
(use-package transient
  :ensure t)
;; Transient:1 ends here

;; [[file:../dotemacs.org::*Magit][Magit:1]]
(use-package magit
  :ensure t
  :requires transient
  :defer 1)
;; Magit:1 ends here

;; [[file:../dotemacs.org::*Skeletor][Skeletor:1]]
(use-package skeletor
  :ensure t
  :custom
  (skeletor-project-directory "~/Files/workspace/projects/")
  :config
  (skeletor-define-template "latex-article"
    :title "LaTeX Article"
    :no-license? t)
  (skeletor-define-template "LaTeX-APA7"
    :title "LaTeX APA 7th Article"
    :no-license? t)
  (skeletor-define-template "latex-beamer"
    :title "LaTeX Beamer"
    :no-license? t))
;; Skeletor:1 ends here

;; [[file:../dotemacs.org::*Project management][Project management:1]]
(provide 'rca-project)
;; Project management:1 ends here

;; [[file:../dotemacs.org::*~transient~][~transient~:1]]
(use-package transient
  :ensure t)
;; ~transient~:1 ends here

;; [[file:../dotemacs.org::*~llama~][~llama~:1]]
(use-package llama
  :ensure t)
;; ~llama~:1 ends here

;; [[file:../dotemacs.org::*~magit~][~magit~:1]]
(use-package magit
  :ensure t
  :requires transient
  :defer 1)
;; ~magit~:1 ends here

;; [[file:../dotemacs.org::*~skeletor~][~skeletor~:1]]
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
;; ~skeletor~:1 ends here

;; [[file:../dotemacs.org::*Project management][Project management:1]]
(provide 'rca-project)
;; Project management:1 ends here

;; [[file:../dotemacs.org::*~transient~][~transient~:1]]
(use-package transient
  :ensure t
  :defer t)
;; ~transient~:1 ends here

;; [[file:../dotemacs.org::*~llama~][~llama~:1]]
(use-package llama
  :ensure t
  :defer t)
;; ~llama~:1 ends here

;; [[file:../dotemacs.org::*~magit~][~magit~:1]]
(use-package magit
  :ensure t
  :requires transient llama)
  ;; :defer t)
;; ~magit~:1 ends here

;; [[file:../dotemacs.org::*~skeletor~][~skeletor~:1]]
(use-package skeletor
  :ensure t
  :defer t
  :custom
  (skeletor-user-directory "~/.sync/templates/skeletons/")
  (skeletor-project-directory "~/Files/Documents/workspace/")
  (skeletor-completing-read-function 'completing-read)
  :config
  (skeletor-define-template "latex-article"
    :title "Latex Article"
    :no-license? t :no-git? t)
  (skeletor-define-template "latex-apa7"
    :title "Latex APA 7th Article"
    :no-license? t :no-git? t)
  (skeletor-define-template "latex-beamer"
    :title "Latex Beamer"
    :no-license? t :no-git? t))
;; ~skeletor~:1 ends here

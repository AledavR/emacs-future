;; -*- lexical-binding: t; -*-
(provide 'rca-project)

(use-package transient
  :ensure t
  :defer t
  :custom
  (transient-history-file (concat user-cache-directory "transient/history.el"))
  (transient-levels-file (concat user-cache-directory "transient/levels.el"))
  (transient-values-file (concat user-cache-directory "transient/values.el")))

(use-package llama
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :requires transient llama)
  ;; :defer t)

(use-package skeletor
  :ensure t
  :defer t
  :custom
  (skeletor-user-directory (concat sync-directory "templates/skeletons/"))
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

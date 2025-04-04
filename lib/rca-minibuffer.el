;; [[file:../dotemacs.org::*Minibuffer][Minibuffer:1]]
(provide 'rca-minibuffer)
;; Minibuffer:1 ends here

;; [[file:../dotemacs.org::*~vertico~][~vertico~:1]]
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cicle t)
  (vertico-count 12))
;; ~vertico~:1 ends here

;; [[file:../dotemacs.org::*~orderless~][~orderless~:1]]
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles
                                          basic
                                          partial-completion)))))
;; ~orderless~:1 ends here

;; [[file:../dotemacs.org::*~marginalia~][~marginalia~:1]]
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))
;; ~marginalia~:1 ends here

;; [[file:../dotemacs.org::*~consult~][~consult~:1]]
(use-package consult
  :ensure t
  :bind (("C-x C-b" . consult-buffer)
         ("C-x R" . consult-recent-file)
         ("C-x r i" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("M-s f" . consult-recent-file)
         ("M-s b" . consult-bookmark)
         ("M-s l" . consult-line)
         :map org-mode-map
         ("M-s s" . consult-org-heading))
  :config
  (consult-customize consult-recent-file :preview-key nil)
  (consult-customize consult-bookmark :preview-key nil))
;; ~consult~:1 ends here

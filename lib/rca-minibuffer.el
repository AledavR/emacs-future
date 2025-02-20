;; [[file:../dotemacs.org::*Minibuffer][Minibuffer:1]]
(provide 'rca-minibuffer)
;; Minibuffer:1 ends here

;; [[file:../dotemacs.org::*Vertico][Vertico:1]]
(use-package vertico
  ;; :ensure (vertico :type git
  ;;       	   :host github
  ;;       	   :repo "minad/vertico"
                   ;; :files (:defaults "extensions/*"))
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cicle t)
  (vertico-count 12))


;; (use-package vertico-directory
;;   :ensure nil
;;   :after vertico
;;   :bind (:map vertico-map
;; 	      ("RET" . vertico-directory-enter)
;; 	      ("DEL" . vertico-directory-delete-char)
;; 	      ("M-DEL" . vertico-directory-delete-word))
;;   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
;; Vertico:1 ends here

;; [[file:../dotemacs.org::*Orderless][Orderless:1]]
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles
                                          basic
                                          partial-completion)))))
;; Orderless:1 ends here

;; [[file:../dotemacs.org::*Marginalia][Marginalia:1]]
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))
;; Marginalia:1 ends here

;; [[file:../dotemacs.org::*Consult][Consult:1]]
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
;; Consult:1 ends here

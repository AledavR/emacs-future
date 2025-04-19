;; [[file:../dotemacs.org::*Programming environment][Programming environment:1]]
(provide 'rca-prog)
;; Programming environment:1 ends here

;; [[file:../dotemacs.org::*Terminal][Terminal:1]]
(use-package vterm
  :ensure t
  :defer t
  :bind (("C-x t V" . vterm)
         ("C-x t v" . vterm-other-tab))
  :preface
  (defun vterm-other-tab ()
    (interactive)
    (let* ((dir (file-name-nondirectory (directory-file-name default-directory)))
           (buffer (concat "*" dir "-shell*")))
      (if (get-buffer buffer)
          (if (tab-bar-get-buffer-tab buffer)
              (tab-switch buffer)
            (switch-to-buffer-other-tab buffer))
        (other-tab-prefix)
        (vterm buffer)))))
;; Terminal:1 ends here

;; [[file:../dotemacs.org::*FORTRAN][FORTRAN:1]]
(use-package fortran
  :ensure nil
  :config
  (add-hook 'f90-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command)
                   (format "gfortran %s && ./a.out" (file-name-nondirectory buffer-file-name))))))
;; FORTRAN:1 ends here

;; [[file:../dotemacs.org::*Gnu plot][Gnu plot:1]]
(use-package gnuplot
  :ensure t
  :defer t)
;; Gnu plot:1 ends here

;; [[file:../dotemacs.org::*LUA][LUA:1]]
(use-package lua-mode
  :ensure t
  :defer t)

(use-package lua-ts-mode
  :ensure nil
  ;; :defer t
  :mode "\\.lua\\'"
  :bind (:map lua-ts-mode-map ("C-c C-c" . lua-send-buffer)))
;; LUA:1 ends here

;; [[file:../dotemacs.org::*Julia][Julia:1]]
(use-package julia-mode
  :ensure t
  :defer t
  :bind (:map julia-mode-map ("`" . julia-insert-unicode-symbol))
  :init  
  (defvar julia-unicode-symbols-alist
    '((?a . "α") (?b . "β")
      (?\C-a . "ₐ")
      (?0 . "₀")
      (?1 . "₁")
      (?2 . "₂")
      (?3 . "₃")
      (?4 . "₄"))
    "List of unicode symbols to be inserted in julia-mode")

  (defun julia-insert-unicode-symbol ()
    (interactive)
    (let* ((char (read-char "Insert symbol: "))
           (entry (assoc char julia-unicode-symbols-alist))
           (symbol (cdr entry)))
      (if (equal nil entry)
          (error "The symbol is not mapped")
        (insert symbol)))))

(use-package julia-snail
  :ensure t
  :defer t
  :hook (julia-mode . julia-snail-mode))
;; Julia:1 ends here

;; [[file:../dotemacs.org::*Python][Python:1]]
(use-package python-mode
  :ensure nil
  :defer t
  :bind (:map python-mode-map
              ("C-c v" . python-set-venv))
  :init
  (defun python-set-venv (interpreter)
    (interactive "fPython interpreter:")
    (setq python-interpreter interpreter
          python-shell-interpreter interpreter))
  :config
  (setq-default python-eldoc-get-doc nil))
;; Python:1 ends here

;; [[file:../dotemacs.org::*Markdown][Markdown:1]]
(use-package markdown-mode
  :ensure t)
;; Markdown:1 ends here

;; [[file:../dotemacs.org::*HTML][HTML:1]]
(use-package mhtml-mode
  :ensure nil
  :defer t
  :preface
  (defun sgml-delete-tagged-text ()
    "Delete text between the tags that contain the current point"
    (interactive)
    (let ((b (point)))
      (sgml-skip-tag-backward 1)
      (when (not (eq b (point)))
        ;; moved somewhere, should be at front of a tag now
        (save-excursion 
          (forward-sexp 1)
          (setq b (point)))
        (sgml-skip-tag-forward 1)
        (backward-sexp 1)
        (delete-region b (point))
        (meow-insert))))
  :bind (("C-c C-i" . sgml-delete-tagged-text)))
  ;; :config
  ;; (define-key mhtml-mode-map (kbd "C-c C-i") 'sgml-delete-tagged-text))
;; HTML:1 ends here

;; [[file:../dotemacs.org::*Tree-sitter][Tree-sitter:1]]
(use-package toml-ts-mode
  :ensure nil
  :mode "\\.toml\\'")
;; Tree-sitter:1 ends here

;; [[file:../dotemacs.org::*~eglot~][~eglot~:1]]
(use-package eglot
  :ensure nil
  :defer t
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-config '(:size 0))
  (defun eglot-open-link ()
    "Open markdown link at point in the `eldoc-doc-buffer'."
    (interactive)
    (let ((url (get-text-property (point) 'help-echo)))
      (if url
          (browse-url-xdg-open url)
        (message "No URL found at point")))))
;; ~eglot~:1 ends here

;; -*- lexical-binding: t; -*-
(provide 'rca-completion)

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil))

;; Configure Tempel
(use-package tempel
  :ensure t
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (add-hook 'eglot-managed-mode-hook 'tempel-setup-capf)
  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection
  :ensure t)

;; Optional: Add tempel-eglot bridge
(use-package eglot-tempel
  :ensure t
  :defer t
  ;; :preface (eglot-tempel-mode)
  :init
  (eglot-tempel-mode t))

(use-package corfu
  :ensure t
  :bind
  (("C-<tab>" . completion-at-point)
   :map corfu-map
   ("S-SPC" . corfu-insert-separator)
   :map corfu-popupinfo-map
   ("M-n" . corfu-popupinfo-scroll-up)
   ("M-p" . corfu-popupinfo-scroll-down))
  :init
  (global-corfu-mode)
  :custom
  (corfu-min-width 70)
  (corfu-max-width 70)
  (corfu-popupinfo-mode 1)
  (corfu-popupinfo-delay '(1.0 . 1.2))
  (corfu-on-exact-match nil)
  ;; (corfu-auto-prefix 4)
  (corfu-separator ?\s)
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-quit-no-match 'separator))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons nil)
  ;; (kind-icon-blend-background t)
  ;; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-hook 'after-enable-theme-hook  #'kind-icon-reset-cache)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :ensure t
  :init
  ;; Make dabbrev use the correct case
  (defun my-cape--dabbrev-fix-expansion (expansion)
    "Return the downcased EXPANSION.
 Removes trailing non-alphanumeric characters if present."
    (let ((downcased (downcase expansion)))
      (substring downcased 0 (string-match-p "[^[:alnum:]]+$" downcased))))
  
  (defun my-cape--dabbrev-list (input)
    "Find all dabbrev expansions for INPUT. "
    (cape--silent
      ;; Don't search all buffers. Only those with the same major-mode.
      (let ((dabbrev-check-other-buffers t)
            (dabbrev-check-all-buffers nil))
        (dabbrev--reset-global-variables))
      (cons
       (apply-partially #'string-prefix-p input)
       (cl-loop for w in (mapcar #'my-cape--dabbrev-fix-expansion
                                 (dabbrev--find-all-expansions input t))
                if (>= (length w) cape-dabbrev-min-length) collect
                (cape--case-replace t input w)))))

  (advice-add 'cape--dabbrev-list :override #'my-cape--dabbrev-list)

  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package dabbrev
  :bind (("C-." . dabbrev-expand)
         ("C-:" . dabbrev-completion))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package smartparens
  :ensure t
  :hook (prog-mode org-mode)
  :config
  (require 'smartparens-config))

(use-package jinx
  :ensure t
  :hook (org-mode . jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :custom (jinx-languages "es estec en_US"))

(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo))

(use-package eldoc-box
  :ensure t
  :custom
  (eldoc-box-max-pixel-width 550)
  (eldoc-box-max-pixel-height 400)
  :bind (("M-ñ" . eldoc-box-help-at-point)
         ("M-n" . eldoc-box-scroll-up)
         ("M-p" . eldoc-box-scroll-down)))

(use-package lite
  :ensure (:host github :repo "amno1/lite")
  :custom (lite-template-dirs (list  (concat sync-directory "templates/files/")))
  :config
  (defun lite-insert-template-in-current-file (template-file-name)
    "Insert contents of TEMPLATE-FILE-NAME into `current-buffer'"
    (interactive
     (list (completing-read
            "Template file: " (directory-files-recursively
                               (if (length> lite-template-dirs 1)
                                   (completing-read "Select directory: " lite-template-dirs)
                                 (car lite-template-dirs))
                               ".*"))))
    (lite-generate-from-template (file-name-nondirectory template-file-name) (buffer-file-name))
    (revert-buffer t t))

  (defun lite-insert-template-at-point (template-file-name)
    "Insert contents of TEMPLATE-FILE-NAME at point"
    (interactive
     (list (completing-read
            "Template file: " (directory-files-recursively
                               (if (length> lite-template-dirs 1)
                                   (completing-read "Select directory: " lite-template-dirs)
                                 (car lite-template-dirs))
                               ".*"))))
    (lite-insert-template (file-name-nondirectory template-file-name))))

;; -*- lexical-binding: t; -*-
(provide 'rca-emacs)

(use-package emacs
  :ensure nil
  :bind (("C-x C-k C-x C-k" . kill-emacs)
         ("C-x B" . ibuffer)
         ("M-z" . zap-up-to-char)
         ("C-z" . nil)
         ("C-x C-r" . nil)
         ("C-x r v" . view-register)
         ("C-z C-j" . rc/file-find-config)
         ("C-z j" . rc/find-stow-file)
         ("<f5>" . recompile)
         ("C-x C-z" . nil)
         ("C-x t h" . tab-bar-mode)
         ("M-o" . other-window)
         ("C-c P" . find-file-at-point)
         ("C-x K" . (lambda () (interactive) (kill-buffer (current-buffer))))
         ("C-z K" . +save-n-kill-buffer-delete-frame)
         ("C-x C-c" . nil)
         ("C-h h" . nil)
         ("M-`" . nil)
         ("<insert>" . nil)
         ("<menu>" . nil))
  :custom
  (initial-scratch-message nil)
  (inhibit-initial-startup-message t)
  (ring-bell-function 'ignore)
  (dired-listing-switches "-alh")
  (column-number-mode t)
  (blink-cursor-mode nil)
  (which-key-mode t)
  (help-window-select t)
  (use-dialog-box nil)
  (auto-save-default nil)
  (auto-save-interval 200)
  (auto-save-timeout 20)
  (history-length 25)
  (auto-save-list-file-prefix nil)
  (backup-directory-alist `(("." . ,backup-directory)))
  (recentf-save-file (concat user-cache-directory "recentf"))
  (recentf-exclude history-excluded-filetypes)
  (tramp-persistency-file-name (concat user-cache-directory "tramp"))
  (project-list-file (concat user-cache-directory "projects"))
  (bookmark-default-file (concat user-cache-directory "bookmarks"))
  (savehist-file (concat user-cache-directory "history"))
  (savehist-additional-variables (list 'register-alist))
  (x-select-enable-clipboard t)
  ;; (read-file-name-completion-ignore-case t)
  (async-shell-command-buffer 'confirm-kill-process)
  (server-client-instructions nil)
  (register-use-preview t)
  (vc-follow-symlinks nil)
  (auth-sources `(,(concat sync-directory ".authinfo.gpg") "~/.authinfo.gpg" "~/.authinfo"))
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  (setq history-excluded-filetypes '(".*gz" ".*pdf" "bookmarks" "recentf" "init.el"
                                     ".*gitignore" "early-init.el" ".*log" ".*png"
                                     ".*jpg" ".*mp4" ".*gif" ".*tmp/lua.*"
                                     ".*agenda/.*" ".*mod/.*" ".*lib/.*" ".*ext/.*"
                                     ".*_db"))
  (defalias 'yes-or-no-p 'y-or-n-p)
  (recentf-mode 1)
  (savehist-mode 1)
  (global-auto-revert-mode 1)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'shell-mode-hook 'rc/truncate-lines-off)
  
  (when (file-exists-p custom-file)
    (load custom-file))
  (when (not (file-exists-p user-cache-directory))
    (make-directory user-cache-directory))
  (when (not (file-exists-p backup-directory))
    (make-directory backup-directory))
  
  (defun +save-n-kill-buffer-delete-frame ()
    (interactive)
    (save-buffer)
    (kill-buffer (current-buffer))
    (delete-frame))

  ;; Greentext mode
  (setq greentext-font-lock
        '(("^>.*" . 'success)))

  (define-derived-mode greentext-mode text-mode "🍀"
    "Major mode for display faces in greentext stories. Derived from `text-mode'."
    (setq font-lock-defaults '(greentext-font-lock))
    (olivetti-mode))
  )

(use-package calendar
  :ensure nil
  :bind (("<f6> c" . calendar))
  :mode ("diary" . diary-mode)
  :custom
  (calendar-latitude -12.0)
  (calendar-longitude -77.1)
  (calendar-mark-diary-entries-flag t)
  (calendar-mark-holidays-flag t)
  (holiday-bahai-holidays nil)
  (holiday-bahai-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-islamic-holidays nil))

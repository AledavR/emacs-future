;; [[file:../dotemacs.org::*Emacs module][Emacs module:1]]
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
         ("C-x C-z" . nil)
         ("C-x t h" . tab-bar-mode)
         ("M-o" . other-window)
         ("C-c P" . find-file-at-point)
         ("C-x K" . (lambda () (interactive) (kill-buffer (current-buffer))))
         ("C-x C-c" . nil)
         ("C-h h" . nil)
         ("M-`" . nil)
         ("<insert>" . nil)
         ("<menu>" . nil))

  :preface
  (setq history-excluded-filetypes '(".*gz" ".*pdf" "bookmarks" "recentf"
    			             "init.el" ".*gitignore" "early-init.el"
    			             ".*log" ".*png" ".*jpg" ".*mp4" ".*gif" ".*tmp/lua.*"
    			             ".*agenda/.*" ".*mod/.*" ".*lib/.*" ".*ext/.*" ".*_db"))
  (setq temporal-directory
        (locate-user-emacs-file "temporal/"))
  (setq snippets-directory
        (locate-user-emacs-file "snippets/"))
  (setq backup-directory
        (rc/locate-or-create-directory  "saves/"))
  (setq undo-history-directory
        (rc/locate-or-create-directory  "undohist/"))
  (setq recentf-file
        (locate-user-emacs-file  "recentf"))
  (setq emacs-config-files-dirs
        '("" "lib/"))
  (setq stow-files
        (concat (getenv "HOME") "/dotfiles/"))
  (put 'eval 'safe-local-variable #'booleanp)
  :custom
  ;; (initial-buffer-choice t)
  (recentf-save-file recentf-file)
  (initial-scratch-message nil)
  (inhibit-initial-startup-message t)
  (ring-bell-function 'ignore)
  (dired-listing-switches "-alh")
  (column-number-mode t)
  (blink-cursor-mode nil)
  (help-window-select t)
  (use-dialog-box nil)
  (auto-save-default nil)
  (auto-save-interval 200)
  (auto-save-timeout 20)
  (history-length 25)
  (auto-save-list-file-prefix nil)
  (backup-directory-alist `(("." . ,backup-directory)))
  (recentf-exclude history-excluded-filetypes)
  (x-select-enable-clipboard t)
  (read-file-name-completion-ignore-case t)
  (async-shell-command-buffer 'confirm-kill-process)
  (server-client-instructions nil)
  (savehist-additional-variables (list 'register-alist))
  (register-use-preview t)
  :config
  (setq emacs-config-files
        (rc/list-merge-sublists
         (mapcar (lambda (dir)
                   (rc/list-append-str
                    dir (rc/file-get-el
                         (concat user-emacs-directory dir))))
                 emacs-config-files-dirs)))
  (recentf-mode 1)
  (savehist-mode 1)
  (global-auto-revert-mode 1)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'shell-mode-hook 'rc/truncate-lines-off)
  (setq-default custom-file
                (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))
  (when (not (file-exists-p temporal-directory))
    (make-directory temporal-directory))

  ;; Greentext mode
  (setq greentext-font-lock
        '(("^>.*" . 'success)))

  (define-derived-mode greentext-mode text-mode "🍀"
    "Major mode for display faces in greentext stories. Derived from `text-mode'."
    (setq font-lock-defaults '(greentext-font-lock))
    (olivetti-mode))

  ;; (add-to-list 'default-frame-alist '(height . 37))
  )

(use-package calendar
  :ensure nil
  :bind (("<f6> c" . calendar))
  :mode ("diary" . diary-mode)
  :custom
  (diary-file "~/.sync/org_files/agenda/diary")
  (calendar-latitude -12.0)
  (calendar-longitude -77.1)
  (calendar-mark-diary-entries-flag t)
  (calendar-mark-holidays-flag t)
  (holiday-bahai-holidays nil)
  (holiday-bahai-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-islamic-holidays nil))
;; Emacs module:1 ends here

;; -*- lexical-binding: t; -*-
(provide 'rca-ui)

(use-package emacs
  :ensure nil
  :init
  (set-face-attribute 'default nil :family "Aporetic Sans Mono" :height 130)
  (set-face-attribute 'fixed-pitch nil :family "Aporetic Sans Mono")
  (set-face-attribute 'variable-pitch nil :family "Aporetic Serif Mono")
  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
  :custom
  (frame-resize-pixelwise t)
  (modus-themes-italic-constructs t)
  (fill-column 80)
  (indent-tabs-mode nil)
  (display-line-numbers-width 3)
  (display-line-numbers-grow-only t)
  (tab-bar-format '(tab-bar-format-history
                    tab-bar-format-tabs-groups
                    tab-bar-separator
                    tab-bar-format-add-tab
                    tab-bar-format-align-right
                    tab-bar-format-global)))

(use-package emacs
  :ensure nil
  :config
  (defun org-babel-detangle-no-buffer-pop-up (orig-fun &rest args)
    (save-excursion
      (let ((display-buffer-alist
             '((".*" (display-buffer-no-window) (allow-no-window . t)))))
        (apply orig-fun args))))
  (advice-add 'org-babel-detangle :around #'org-babel-detangle-no-buffer-pop-up)
  (setq display-buffer-alist
        '(((derived-mode . shell-mode)
           (display-buffer-reuse-mode-window
            display-buffer-below-selected)
           (window-height . 12)
           (dedicated . t)
           (window-parameters . ((no-other-window . t)
  			         (mode-line-format . none))))
          ("^\\*\\(Help\\|Info\\|Man\\)"
           (display-buffer-in-side-window)
           (side . right)
           (slot . 0)
           (window-width . 0.40))
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-reuse-mode-window
            display-buffer-at-bottom)
           (window-height . 10)
           (window-parameters . ((mode-line-format . none))))
          ("\\*\\(Agenda Commands\\|Org Agenda\\|Org Select\\).*"
           (display-buffer-reuse-mode-window
            display-buffer-at-bottom)
           (window-parameters . ((mode-line-format . none))))
          ("\\*compilation\\*"
           (display-buffer-reuse-mode-window
            display-buffer-below-selected)
           (window-height . 12)
           (dedicated . t))
          ("\\*\\(Python\\|vterm\\)\\*"
           (display-buffer-reuse-mode-window
            display-buffer-below-selected)
           (window-height . 20)
           (dedicated . t))
          ("\\*undo-tree\\*"
           (display-buffer-in-side-window)
           (side . right)
           (dedicated . t)
           (window-width . 0.25)))))

(use-package emacs
  :ensure nil
  :init
  (defcustom wallpaper-files (concat sync-directory "pix/wallpaper")
    "Folder where wallpaper files are stored."
    :type 'directory))

(use-package diminish
  :ensure t
  :config
  (diminish 'which-key-mode nil)
  (diminish 'eldoc-mode nil))

(use-package auto-dim-other-buffers
  :ensure t
  :init (auto-dim-other-buffers-mode))

(use-package ef-themes
  :ensure t
  :init
  (setq themes
        '((parsee ef-reverie ef-elea-dark)
          (yuuma ef-tritanopia-light ef-rosa)
          (nazrin2 ef-light ef-owl)
          (youmu ef-elea-light ef-elea-dark)
          (tsukasa ef-eagle ef-dream)
          (satori ef-trio-light ef-trio-dark)))
  (setq theme-character 'tsukasa)
  :config
  (setq ef-themes-mixed-fonts t)
  (setq ef-themes-headings
        '((0 . (1.6))
          (1 . (1.5))
          (2 . (1.3))
          (agenda-date . (1.3))
          (agenda-structure . (1.8))
          (t . (1.1)))))

(use-package circadian
  :ensure t
  :after (:all ef-themes emacs calendar auto-dim-other-buffers)
  :hook
  (server-after-make-frame . (lambda () (enable-theme (car custom-enabled-themes))))
  :config
  (let* ((theme-colors (cdr (assoc theme-character themes)))
         (sunrise (car theme-colors))
         (sunset (cdr theme-colors)))
    (setq circadian-themes `((:sunrise . ,sunrise)
                             (:sunset . ,sunset))))
  (circadian-setup))

(use-package olivetti
  :ensure t
  :hook (Info-mode . olivetti-mode)
  :custom (olivetti-body-width 110))

(use-package spacious-padding
  :ensure t
  :custom
  (spacious-padding-widths
   '( :internal-border-width 3
      :header-line-width 3
      :mode-line-width 3
      :tab-width 3
      :right-divider-width 10
      :scroll-bar-width 2
      :fringe-width 2))
  (spacious-padding-subtle-mode-line
   '( :mode-line-active spacious-padding-subtle-mode-line-active
      :mode-line-inactive spacious-padding-subtle-mode-line-inactive))
  :config
  ;; (setq-default header-line-format
  ;;               '("%e" mode-line-front-space
  ;;                 (:propertize
  ;;                  display (min-width (6.0)))
  ;;                 "%b" mode-line-end-spaces))
  
  ;; (setq spacious-padding-subtle-mode-line
  ;;       `( :mode-line-active 'default
  ;;          :mode-line-inactive vertical-border))
  (spacious-padding-mode 1))

(defun my-inhibit-startup-screen-file ()
  "Startup screen inhibitor for `command-line-functions`.
Inhibits startup screen on the first unrecognised option which
names an existing file."
  (ignore
   (setq inhibit-startup-screen
	 (file-exists-p
	  (expand-file-name argi command-line-default-directory)))))

;; (add-hook 'command-line-functions #'my-inhibit-startup-screen-file)
(setq command-line-functions #'my-inhibit-startup-screen-file)

(use-package dashboard
  :ensure t
  :preface
  (defun protect-dashboard ()
    (define-key
     dashboard-mode-map (kbd "q") 'dashboard-refresh-buffer))
  (defun rc/refresh-buffer-maybe ()
    (when (equal "*dashboard*" (buffer-name))
      (revert-buffer)))
  ;; Files don't open from command line if this is in init
  ;; TODO Check if there is any other problem in this section
  :init
  (setq banner-images
        (directory-files (locate-user-emacs-file "img") t ".*g$"))
  (setq banner-image-size (if (equal system-name "acer") 500 550))
  :hook
  (elpaca-after-init . dashboard-insert-startupify-lists)
  (elpaca-after-init . dashboard-initialize)
  (dashboard-mode . protect-dashboard)
  (dashboard-after-initialize . dashboard-refresh-buffer)
  (server-after-make-frame . rc/refresh-buffer-maybe)
  ;; (server-after-make-frame . (lambda () (set-frame-font "Aporetic Sans Mono 13")))
  :custom
  (dashboard-center-content t)
  (dashboard-startup-banner `(,(locate-user-emacs-file (concat "img/" (symbol-name theme-character) ".png"))))
  ;; (dashboard-startup-banner `(,(rc/list-select-random banner-images)))
  ;; (dashboard-startup-banner banner-images)
  (dashboard-set-navigator t)
  (dashboard-navigator-buttons `(((nil "Agenda" "Open detailed agenda buffer"
                                       (lambda (&rest _) (org-agenda nil "a")))
                                  (nil "Notes" "Open note directory"
                                       (lambda (&rest _) (dired denote-directory)))
                                  (nil "Ideas" "Open ideas notebook"
                                       (lambda (&rest _) (find-file my/org-idea-notebook))))))
  (dashboard-image-banner-max-height banner-image-size)
  (dashboard-startupify-list '(dashboard-insert-banner
                               dashboard-insert-newline
                               dashboard-insert-banner-title
                               dashboard-insert-newline
                               dashboard-insert-navigator
                               dashboard-insert-init-info
                               dashboard-insert-items
                               dashboard-insert-newline
                               dashboard-insert-footer))
  (dashboard-banner-logo-title nil)
  (dashboard-match-agenda-entry "-class")
  (dashboard-set-footer nil)
  (dashboard-footer-messages (list nil))
  (tab-bar-new-tab-choice "*dashboard*")
  (dashboard-items '((agenda . 10)))
  ;; (dashboard-agenda-tags-format 'ignore)
  :init
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice
        (lambda () (get-buffer-create "*dashboard*"))))
  ;; :config
  ;; (add-hook server-after-make-frame-hook 'revert-buffer))

(use-package rainbow-mode
  :ensure t
  :defer t)

(use-package breadcrumb
  :ensure t
  :config
  
  ;; (setq-default mode-line-format
  ;;               '("%e" mode-line-front-space
  ;;                 (:propertize
  ;;                  ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
  ;;                   mode-line-window-dedicated)
  ;;                  display (min-width (6.0)))
  ;;                 mode-line-frame-identification "   "
  ;;                 mode-line-position (project-mode-line project-mode-line-format)
  ;;                 (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
  
  ;; (set-face-attribute 'header-line-active nil :inherit 'mode-line-active)
  
  (breadcrumb-mode))

;; https://www.rahuljuliato.com/posts/emacs-tab-bar-groups
(use-package tab-bar
  :ensure nil
  :defer t
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-auto-width nil)
  (tab-bar-separator " ")
  (tab-bar-format '(tab-bar-format-tabs-groups
		    Tab-bar-format-tabs tab-bar-separator
		    tab-bar-format-add-tab))
  :init
  ;;; --- OPTIONAL INTERNAL FN OVERRIDES TO DECORATE NAMES
  (defun tab-bar-tab-name-format-hints (name _tab i)
    (if tab-bar-tab-hints (concat (format "»%d«" i) "") name))

  (defun tab-bar-tab-group-format-default (tab _i &optional current-p)
    (propertize
     (concat (funcall tab-bar-tab-group-function tab))
     'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))


  ;;; --- UTILITIES FUNCTIONS
  (defun emacs-solo/tab-group-from-project ()
    "Call `tab-group` with the current project name as the group."
    (interactive)
    (when-let* ((proj (project-current))
		(name (file-name-nondirectory
		       (directory-file-name (project-root proj)))))
      (tab-group (format "[%s]" name))))

  (defun emacs-solo/tab-switch-to-group ()
    "Prompt for a tab group and switch to its first tab.
Uses position instead of index field."
    (interactive)
    (let* ((tabs (funcall tab-bar-tabs-function)))
      (let* ((groups (delete-dups (mapcar (lambda (tab)
					    (funcall tab-bar-tab-group-function tab))
					  tabs)))
	     (group (completing-read "Switch to group: " groups nil t)))
	(let ((i 1) (found nil))
	  (dolist (tab tabs)
	    (let ((tab-group (funcall tab-bar-tab-group-function tab)))
	      (when (and (not found)
			 (string= tab-group group))
		(setq found t)
		(tab-bar-select-tab i)))
	    (setq i (1+ i)))))))

  ;;; --- EXTRA KEYBINDINGS
  (global-set-key (kbd "C-x t P") #'emacs-solo/tab-group-from-project)
  (global-set-key (kbd "C-x t g") #'emacs-solo/tab-switch-to-group)

  ;;; --- TURNS ON BY DEFAULT
  (tab-bar-mode 1))

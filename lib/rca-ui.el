;; [[file:../dotemacs.org::*User interface][User interface:1]]
(provide 'rca-ui)
;; User interface:1 ends here

;; [[file:../dotemacs.org::*User interface general options][User interface general options:1]]
(use-package emacs
  :ensure nil
  :init
  ;; (setq text-font "Lexend")
  (setq default-font "mononoki Nerd Font")
  (set-face-attribute 'default nil
        	      :family default-font
        	      :height 110)
  (set-face-attribute 'italic nil
        	      :family default-font)
  :custom
  (frame-resize-pixelwise t)
  (modus-themes-italic-constructs t)

  :config
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (setq-default fill-column 80)
  (setq-default indent-tabs-mode nil)
  (setq-default display-line-numbers-width 3)
  (setq-default display-line-numbers-grow-only t))
;; User interface general options:1 ends here

;; [[file:../dotemacs.org::*Buffer display options][Buffer display options:1]]
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
        '(
          ((derived-mode . shell-mode)
           (display-buffer-reuse-mode-window
            display-buffer-below-selected)
           (window-height . 12)
           (dedicated . t)
           (window-parameters . ((no-other-window . t)
  			         (mode-line-format . none))))
          ("\\*Help\\*"
           (display-buffer-in-side-window)
           (side . right)
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
          ("\\*vterm\\*"
           (display-buffer-reuse-mode-window
            display-buffer-below-selected)
           (window-height . 20)
           (dedicated . t))
          ("\\*Python\\*"
           (display-buffer-reuse-mode-window
            display-buffer-below-selected)
           (window-height . 20)
           (dedicated . t))
          ("\\*undo-tree\\*"
           (display-buffer-in-side-window)
           (side . right)
           (dedicated . t)
           (window-width . 0.25)))))
;; Buffer display options:1 ends here

;; [[file:../dotemacs.org::*User interface variables][User interface variables:1]]
(use-package emacs
  :ensure nil
  :init

  (defcustom wallpaper-files
    (concat (getenv "HOME") "./sync/pix/wallpaper/")
    "Folder where wallpaper files are stored."
    :type 'directory)

  )
;; User interface variables:1 ends here

;; [[file:../dotemacs.org::*Diminish][Diminish:1]]
(use-package diminish
  :ensure t
  :config
  (diminish 'which-key-mode nil)
  (diminish 'eldoc-mode nil))
;; Diminish:1 ends here

;; [[file:../dotemacs.org::*Themes][Themes:1]]
(use-package ef-themes
  :ensure t
  :init
  (setq themes
        '((parsee ef-reverie ef-elea-dark)
          (yuuma ef-tritanopia-light ef-rosa)
          (nazrin2 ef-light ef-owl)
          (satori ef-trio-light ef-trio-dark)))
  (setq theme-character 'yuuma)
  :config
  (setq ef-themes-headings
        '((0 . (1.6))
          (1 . (1.5))
          (2 . (1.3))
          (agenda-date . (1.3))
          (agenda-structure . (1.8))
          (t . (1.1)))))
;; Themes:1 ends here

;; [[file:../dotemacs.org::*Circadian][Circadian:1]]
(use-package circadian
  :ensure t
  :after (:all ef-themes emacs calendar)
  :hook
  (server-after-make-frame . (lambda () (enable-theme (car custom-enabled-themes))))
  :config
  (let* ((theme-colors (cdr (assoc theme-character themes)))
         (sunrise (car theme-colors))
         (sunset (cdr theme-colors)))
    (setq circadian-themes `((:sunrise . ,sunrise)
                             (:sunset . ,sunset))))
  (circadian-setup))
;; Circadian:1 ends here

;; [[file:../dotemacs.org::*Olivetti][Olivetti:1]]
(use-package olivetti
  :ensure t
  :hook (Info-mode . olivetti-mode))
;; Olivetti:1 ends here

;; [[file:../dotemacs.org::*Spacious-Padding][Spacious-Padding:1]]
(use-package spacious-padding
  :ensure t
  :config
  (setq-default header-line-format
                '("%e" mode-line-front-space
                  (:propertize
                   display (min-width (6.0)))
                  "%b" mode-line-end-spaces))

  (setq-default mode-line-format
                '("%e" mode-line-front-space
                  (:propertize
                   ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
                    mode-line-window-dedicated)
                   display (min-width (6.0)))
                  mode-line-frame-identification "   "
                  mode-line-position (project-mode-line project-mode-line-format)
                  (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

  (setq spacious-padding-widths
        '( :internal-border-width 10
           :header-line-width 4
           :mode-line-width 4
           :tab-width 4
           :right-divider-width 25
           :scroll-bar-width 8
           :fringe-width 8))
  (setq spacious-padding-subtle-mode-line
        `( :mode-line-active 'default
           :mode-line-inactive vertical-border))

  (spacious-padding-mode 1)

  ;; Set a key binding if you need to toggle spacious padding.
  (define-key global-map (kbd "<f8>") #'spacious-padding-mode))
;; Spacious-Padding:1 ends here

;; [[file:../dotemacs.org::*Dashboard][Dashboard:1]]
(defun my-inhibit-startup-screen-file ()
  "Startup screen inhibitor for `command-line-functions`.
Inhibits startup screen on the first unrecognised option which
names an existing file."
  (ignore
   (setq inhibit-startup-screen
	 (file-exists-p
	  (expand-file-name argi command-line-default-directory)))))

(add-hook 'command-line-functions #'my-inhibit-startup-screen-file)

(use-package dashboard
  :ensure t
  :preface
  (defun protect-dashboard ()
    (define-key
     dashboard-mode-map (kbd "q") 'dashboard-refresh-buffer))
  :init
  (setq banner-images
        (directory-files (locate-user-emacs-file "img") t ".*g$"))
  (setq banner-image-size (if (equal system-name "acer") 500 550))
  :hook
  (elpaca-after-init . dashboard-insert-startupify-lists)
  (elpaca-after-init . dashboard-initialize)
  (dashboard-mode . protect-dashboard)
  (dashboard-after-initialize . dashboard-refresh-buffer)
  :custom
  (dashboard-center-content t)
  (dashboard-items nil)
  (dashboard-startup-banner `(,(locate-user-emacs-file (concat "img/" (symbol-name theme-character) ".png"))))
  ;; (dashboard-startup-banner `(,(rc/list-select-random banner-images)))
  ;; (dashboard-startup-banner banner-images)
  (dashboard-image-banner-max-height banner-image-size)
  (dashboard-banner-logo-title nil)
  (dashboard-set-footer nil)
  (dashboard-footer-messages (list nil))
  (tab-bar-new-tab-choice "*dashboard*")
  :init
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice
        (lambda () (get-buffer-create "*dashboard*"))))
;; Dashboard:1 ends here

;; [[file:../dotemacs.org::*Rainbow mode][Rainbow mode:1]]
(use-package rainbow-mode
  :ensure t
  :defer t)
;; Rainbow mode:1 ends here

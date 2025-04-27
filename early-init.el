(setopt package-enable-at-startup nil)

(setopt gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; [[file:../dotemacs.org::*Tools][Tools:1]]
(provide 'rca-tools)
;; Tools:1 ends here

;; [[file:../dotemacs.org::*~ebuku~][~ebuku~:1]]
(use-package ebuku
  :ensure t
  :defer t
  :bind ("C-z b" . ebuku)
  :custom-face
  (ebuku-tags-face ((t (:inherit font-lock-keyword-face))))
  (ebuku-title-face ((t (:inherit font-lock-constant-face))))
  :custom
  (ebuku-results-limit 25))
;; ~ebuku~:1 ends here

;; [[file:../dotemacs.org::*~denote~][~denote~:1]]
(use-package denote
  :ensure t
  :custom
  (denote-known-keywords '("matematica" "informatica"))
  (denote-directory "~/.sync/archive/notes")
  (denote-dired-directories '("~/.sync/archive/notes" "~/.sync/archive/journal" "/home/rcaled/Files/Downloads/universidad"))
  :config
  (add-hook 'dired-mode-hook 'denote-dired-mode-in-directories))

(use-package denote-journal
  :ensure t
  :custom
  (denote-journal-title-format 'day-date-month-year)
  (denote-journal-directory "~/.sync/archive/journal"))

(use-package denote-search
  :ensure t
  :defer t)
;; ~denote~:1 ends here

;; [[file:../dotemacs.org::*~embark~][~embark~:1]]
(use-package embark
  :ensure t
  :bind (("C-c o" . embark-act)
         :map embark-general-map
         ("G" . +embark-google-search)
         :map embark-url-map
         ("M" . +org-link-remote-open-in-mpv)
         :map embark-file-map
         ("M" . +org-link-open-in-mpv))
  :init
  (defun +embark-google-search (term)
    (interactive "sSearch Term: ")
    (browse-url (format "https://google.com/search?q=%s" term)))
  :config
  (add-to-list 'display-buffer-alist '("\\*Embark Actions\\*" (display-buffer-pop-up-window))))

(use-package embark-consult
  :ensure t)
;; ~embark~:1 ends here

;; [[file:../dotemacs.org::*~citar~][~citar~:1]]
(use-package citar
  :ensure t
  :bind (("C-z c o" . citar-open)
         ("C-z c c" . citar-insert-citation)
         ("C-z c r" . citar-insert-reference)
         ("C-z c b" . citar-insert-bibtex)
         ("C-z c k" . citar-insert-keys))
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :config
  (let ((documents-path (xdg-user-dir "DOCUMENTS"))
        (archive-path "~/.sync/archive/"))
    (setq org-cite-global-bibliography
          (mapcar (lambda (entry) (concat archive-path entry)) '("articles.bib" "books.bib")))
    (setq citar-library-paths
          (mapcar (lambda (entry) (concat documents-path entry)) '("/library/articles/" "/library/books/"))
          citar-notes-paths (list (concat archive-path "bibnotes/")))
    (setq citar-file-note-extensions '("org")
          citar-library-file-extensions '("pdf")
          citar-bibliography org-cite-global-bibliography))
  (defcustom +citar-remote-library-path nil "Remote path that contains the library"
    :type '(string))
  (defcustom +citar-library-server nil "Remote library ssh server"
    :type '(string))
  (defcustom +citar-library-port nil "Remote library ssh server port"
    :type '(string))
  (defcustom +citar-local-library-path nil "Local path that contains the library"
    :type '(string))
  (defun +citar-open-file-externally (citekey)
    "Opens associated file in the default system reader"
    (let ((file (car (car (hash-table-values (citar-get-files citekey))))))
      (if file (start-process "open pdf" nil "xdg-open" file)
        (message "No pdf file found with this citekey"))))
  (defun +citar-scholar-search (citekey)
    "Search the entry in google scholar"
    (browse-url (format "https://scholar.google.com/scholar?q=%s"
                        (citar-get-value "title" citekey))))
  (defun +citar-download-file (citekey)
    "Downloads file from the remote server and stores it in the library"
    (start-process "citar-download-file" nil "download-from-archive"
                   +citar-library-server +citar-library-port
                   +citar-remote-library-path citekey +citar-local-library-path)))

(use-package citar-embark
  :ensure t
  :diminish
  :after (citar embark)
  :defer nil
  :bind (:map citar-embark-map
              ("g" . +citar-scholar-search)
              ("d" . +citar-download-file)
              ("F" . +citar-open-file-externally)
              :map citar-embark-citation-map
              ("g" . +citar-scholar-search)
              ("d" . +citar-download-file)
              ("F" . +citar-open-file-externally))
  :config
  (citar-embark-mode))
;; ~citar~:1 ends here

;; [[file:../dotemacs.org::*~ebib~][~ebib~:1]]
(use-package ebib
  :ensure t
  :defer t
  :custom
  (ebib-preload-bib-files '("~/.sync/archive/articles.bib" "~/.sync/archive/books.bib"))
  (ebib-file-search-dirs '("~/Files/Documents/library/articles" "~/Files/Documents/library/books"))
  (ebib-file-associations '(("ps" . "gv")))
  :config
  (define-key ebib-index-mode-map (kbd "O") '+ebib-open-file-externally)
  (define-key ebib-index-mode-map (kbd "L") '+ebib-scholar-search)
  (defun +ebib-open-file-externally () ; Maybe I can define args?
    (interactive)
    (let ((file (ebib--expand-file-name (ebib--select-file nil 1 (ebib--get-key-at-point)))))
      (if file (start-process "open pdf" nil "xdg-open" file)
        (message "No PDF file found with this citekey"))))
  (defun +ebib-scholar-search ()
    (interactive)
    (browse-url (format "https://scholar.google.com/scholar?q=%s"
                        (ebib-get-field-value "title" (ebib--get-key-at-point) ebib--cur-db nil t)))))
;; ~ebib~:1 ends here

;; [[file:../dotemacs.org::*~alert~][~alert~:1]]
(use-package alert
  :ensure t
  :config
  (defun rc/alert-notifications-notify (info)
    "Show the alert defined by INFO with `notifications-notify'."
    (let ((id (notifications-notify :title "Reminder"
                                    :body  (plist-get info :message)
                                    :app-icon (plist-get info :icon)
                                    :app-name "emacs"
                                    :timeout (if (plist-get info :persistent) 0 -1)
                                    :replaces-id (gethash (plist-get info :id) alert-notifications-ids)
                                    :urgency (cdr (assq (plist-get info :severity)
                                                        alert-notifications-priorities))
                                    :actions '("default" "Open corresponding buffer")
                                    :on-action (lambda (id action)
                                                 (when (string= action "default")
                                                   (switch-to-buffer (plist-get info :buffer)))))))
      (when (plist-get info :id)
        (puthash (plist-get info :id) id alert-notifications-ids)))
    (alert-message-notify info))
  (alert-define-style 'rc-style :title "My custom style" :notifier 'rc/alert-notifications-notify)
  (setq alert-default-style 'rc-style)

  ;; Short reminder commands
  (defun short-reminder (time message)
    (interactive "nMinutes: \nsMessage: ")
    (run-at-time (* (/ (float time) 2) 60) nil #'alert (concat message " (Timer at half)"))
    (run-at-time (* time 60) nil #'alert message)))

(use-package org-wild-notifier
  :ensure t
  :requires alert
  :config
  (setq org-wild-notifier-alert-time '(4320 2880 1440 720 360 180 120 60 15 5 1))
  ;; (setq org-wild-notifier-keyword-whitelist nil)
  (org-wild-notifier-mode))
;; ~alert~:1 ends here

;; [[file:../dotemacs.org::*~dired~][~dired~:1]]
(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode))

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map ("\/" . dired-narrow)))
;; ~dired~:1 ends here

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
    (denote-dired-directories '("~/.sync/archive/notes" "~/.sync/archive/journal" "~/.sync/archive/posts"))
    :init
    (defvar-keymap denote-prefix-map
      :doc "Denote commands"
      "n" #'denote
      "o" #'denote-open-or-create
      "l" #'denote-link
      "L" #'denote-link-or-create
      "s" #'denote-grep)
    (defalias 'denote-prefix denote-prefix-map)
    :config
    (setq denote-org-front-matter
          "#+title:      %s
#+date:       %s
#+filetags:   %s
#+identifier: %s
#+signature:  %s
#+startup:    nofold
#+startup:    hideblocks
\n")
    (add-hook 'dired-mode-hook 'denote-dired-mode-in-directories)
    (global-set-key (kbd "C-z C-n") 'denote-prefix))

  (use-package denote-journal
    :ensure t
    :defer t
    :bind (:map denote-prefix-map ("j" . denote-journal-new-or-existing-entry))
    :custom
    (denote-journal-title-format 'day-date-month-year)
    (denote-journal-directory "~/.sync/archive/journal"))
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
         ("C-z c B" . citar-insert-bibtex)
         ("C-z c k" . citar-insert-keys))
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :config
  (let ((documents-path (xdg-user-dir "DOCUMENTS"))
        (archive-path "~/.sync/archive/bibliography/"))
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
  :bind (("C-z c b" . ebib))
  :custom
  (ebib-preload-bib-files '("~/.sync/archive/bibliography/articles.bib" "~/.sync/archive/bibliography/books.bib"))
  (ebib-file-search-dirs '("~/Files/Documents/library/articles" "~/Files/Documents/library/books"))
  (ebib-file-associations '(("ps" . "gv")))
  :config
  (define-key ebib-index-mode-map (kbd "O") '+ebib-open-file-externally)
  (define-key ebib-index-mode-map (kbd "L") '+ebib-scholar-search)
  (defun +ebib-open-file-externally () ; Maybe I can define args?
    (interactive)
    (let* ((file (ebib-get-field-value "file" (ebib--get-key-at-point) ebib--cur-db t 'unbraced))
           (filename (if file (ebib--expand-file-name file)
                       (ebib--expand-file-name (ebib--create-file-name-from-key (ebib--get-key-at-point) "pdf")))))
      (if filename
          (start-process "open pdf" nil "xdg-open" filename)
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
  :hook (dired-mode . dired-hide-details-mode)
  :custom (dired-dwim-target t))

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map ("\/" . dired-narrow)))
;; ~dired~:1 ends here

;; [[file:../dotemacs.org::*0x0][0x0:1]]
(use-package 0x0
  :ensure t
  :config
  (defun +htmlize-buffer-with-theme (theme)
    (load-theme theme t)
    (write-file (buffer-name (htmlize-buffer (current-buffer) t)) nil)
    (when (car (cdr custom-enabled-themes))
      (load-theme (car (cdr custom-enabled-themes)) t)
      (disable-theme theme))
    (kill-buffer))

  (defun +htmlize-buffer-dark-theme ()
    (interactive)
    (+htmlize-buffer-with-theme (car (cdr (cdr (assoc theme-character themes))))))

  (defun +htmlize-buffer-light-theme ()
    (interactive)
    (+htmlize-buffer-with-theme (car (cdr (assoc theme-character themes)))))

  (defun +0x0-htmlize-and-send ()
    (interactive)
    (+htmlize-buffer-dark-theme)
    (let* ((server (0x0--choose-server))
           (file-name (expand-file-name (concat (buffer-name) ".html")))
           (size (file-attribute-size (file-attributes file-name)))
           (resp (0x0--send server file-name)))
      (0x0--handle-resp server size resp))))
;; 0x0:1 ends here

;; [[file:../dotemacs.org::*~erc~][~erc~:1]]
(use-package erc
  :ensure nil
  :defer t
  :hook (erc-join . erc-give-me-more-irc-history)
  :custom
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  (erc-fill-column 120)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 20)
  (erc-track-shorten-start 5)
  :preface
  (defun erc-libera ()
    "Connect to Libera.Chat IRC server."
    (interactive)
    (erc-tls
     :nick "rcaled"
     :server "rcaled.mooo.com"
     :port "6697"
     :user "rcaled/irc.libera.chat@emacs"
     :password (cadr (auth-source-user-and-password "rcaled.mooo.com" "rcaled"))))
  :config
  (defun erc-give-me-more-irc-history ()
    "Get more history for current IRC buffer (IRCv3 only).

Defaults to 100 lines of history; when C-u prefixed, asks user for
number of lines to fetch.

If using an IRCv3 capable server/bouncer (like chat.sr.ht), fetch the
chat history via the IRCv3 chathistory extension. (Currently, only
soju-based servers implement this feature; see:
https://ircv3.net/software/clients)

For more on chathistory, see:
 - https://man.sr.ht/chat.sr.ht/bouncer-usage.md#chat-history-logs
 - https://ircv3.net/specs/extensions/chathistory
 - https://soju.im/doc/soju.1.html"
    (interactive)
    (if (not (member
              (with-current-buffer (current-buffer)
                major-mode)
              '(erc-mode
                circe-mode
                rcirc-mode)))
        (message "not an IRC buffer; ignoring")
      (let ((lines 100)
            (channel (buffer-name)))
        (when current-prefix-arg
          (progn
            (setq lines
                  (read-number (format "How many lines to fetch: ") lines))))
        (erc-send-input
         (concat "/quote CHATHISTORY LATEST " channel " * " (number-to-string lines))
         t)))))
;; ~erc~:1 ends here

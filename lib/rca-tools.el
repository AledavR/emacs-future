;; -*- lexical-binding: t; -*-
(provide 'rca-tools)

(use-package ebuku
  :ensure t
  :defer t
  :bind ("C-z b" . ebuku)
  :custom-face
  (ebuku-tags-face ((t (:inherit font-lock-keyword-face))))
  (ebuku-title-face ((t (:inherit font-lock-constant-face))))
  :custom
  (ebuku-results-limit 25))

(use-package denote
  :ensure t
  :custom
  (denote-known-keywords '("matematica" "informatica"))
  (denote-directory (concat sync-directory "archive/notes"))
  (denote-dired-directories (mapcar (lambda (dir) (concat sync-directory "archive/" dir )) '("notes" "journal" "posts")))
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
  (defun org-yank-image-denote-file-name-function ()
    (if (y-or-n-p "Rename file? ")
        (let ((title (denote-sluggify 'title (denote-title-prompt nil "File name: ")))
              (keywords (denote-keywords-combine (denote-sluggify-keywords (denote-keywords-prompt "Keywords: "))))
              (date (format-time-string "%Y%m%dT%H%M%S")))
          (concat date "--" title "__" keywords))
      (format-time-string "%Y%m%dT%H%M%S--clipboard")))

  (setq org-yank-image-file-name-function 'org-yank-image-denote-file-name-function)

  
  (defun my-remove-accents (text)
    (let ((slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768 ; U+0300 COMBINING GRAVE ACCENT
                             769 ; U+0301 COMBINING ACUTE ACCENT
                             770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771 ; U+0303 COMBINING TILDE
                             772 ; U+0304 COMBINING MACRON
                             774 ; U+0306 COMBINING BREVE
                             775 ; U+0307 COMBINING DOT ABOVE
                             776 ; U+0308 COMBINING DIAERESIS
                             777 ; U+0309 COMBINING HOOK ABOVE
                             778 ; U+030A COMBINING RING ABOVE
                             779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                             780 ; U+030C COMBINING CARON
                             795 ; U+031B COMBINING HORN
                             803 ; U+0323 COMBINING DOT BELOW
                             804 ; U+0324 COMBINING DIAERESIS BELOW
                             805 ; U+0325 COMBINING RING BELOW
                             807 ; U+0327 COMBINING CEDILLA
                             813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814 ; U+032E COMBINING BREVE BELOW
                             816 ; U+0330 COMBINING TILDE BELOW
                             817 ; U+0331 COMBINING MACRON BELOW
                             )))
      (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars)))
        (string-glyph-compose
         (apply #'string
                (seq-remove #'nonspacing-mark-p
                            (string-glyph-decompose text)))))))

  (defun my/denote-sluggify-title (str)
    (downcase
     (denote-slug-hyphenate
      (my-remove-accents
       (replace-regexp-in-string "[][{}!@#$%^&*()+'\"?,.\|;:~`‘’“”/=]*" "" str)))))

  (setq denote-file-name-slug-functions
        '((title . my/denote-sluggify-title)
          (signature . denote-sluggify-signature)
          (keywords . denote-sluggify-keywords)))

  (setq denote-org-front-matter
        "#+title:      %s\n#+date:       %s\n#+filetags:   %s\n#+identifier: %s\n#+startup:    nofold\n#+startup:    hideblocks\n")
  (add-hook 'dired-mode-hook 'denote-dired-mode-in-directories)
  (global-set-key (kbd "C-z C-n") 'denote-prefix))

(use-package denote-org
  :ensure t
  :defer t)

(use-package denote-journal
  :ensure t
  :defer t
  :bind (:map denote-prefix-map ("j" . denote-journal-new-or-existing-entry))
  :custom
  (denote-journal-title-format 'day-date-month-year)
  (denote-journal-directory  (concat sync-directory "archive/journal")))

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
        (archive-path (concat sync-directory "archive/bibliography/")))
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

(use-package ebib
  :ensure t
  :defer t
  :bind (("C-z c b" . ebib))
  :custom
  (ebib-preload-bib-files (mapcar (lambda (file) (concat sync-directory "archive/bibliography/" file)) '("articles.bib" "books.bib")))
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

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :custom (dired-dwim-target t))

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map ("\/" . dired-narrow)))

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

(use-package erc
  :ensure nil
  :defer t
  ;; :hook (erc-join . erc-give-me-more-irc-history)
  :custom
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  (erc-fill-column 120)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 20)
  (erc-track-shorten-start 5)
  (erc-keywords
    '(("'\\([^\n]+\\)' \\[\\([0-9:]+\\)\\]" . erc-keyword-face)))
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
                circe-channel-mode
                rcirc-mode)))
        (message "not an IRC buffer; ignoring")
      (let ((lines 200)
            (channel (buffer-name)))
        (when current-prefix-arg
          (progn
            (setq lines
                  (read-number (format "How many lines to fetch: ") lines))))
        (erc-send-input
         (concat "/quote CHATHISTORY LATEST " channel " * " (number-to-string lines))
         t)))))

(use-package erc-hl-nicks
  :ensure t
  :after erc
  :custom
  (erc-hl-nicks-minimum-contrast-ratio 4.5))

(use-package consult-erc
  :ensure (:host codeberg :repo "mekeor/consult-erc")
  :bind (("M-s e" . consult-erc-dwim))
  :after erc)

(use-package gptel
  :ensure t
  :commands (gptel gptel-send)
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model 'deepseek-chat)
  :bind (("M-s g" . rc/find-gptel-file))
  :config
  (defun rc/find-gptel-file ()
    "Find config file interactively"
    (interactive)
    (find-file (locate-user-emacs-file
                (completing-read "Select config file: "
                                 (directory-files-recursively (concat sync-directory "archive/llm/") ".*" nil)))))
  (dolist (directive
           '((Asistente . "Eres un modelo de lenguaje asistente especializado en programación el cual esta contenido en el editor de texto Emacs. Debes explicar tu respuesta de manera concisa.")
             (Generador . "Eres un modelo de lenguaje y un programador eficiente. Solo genera código y solo código como única salida sin ningún tipo de texto adicional.")
             (Matematico . "Eres un modelo de lenguaje y un instructor de matemática. Define de manera concisa los pasos usados en la resolución de problemas. Usa notación Latex con los símbolos \( y \) cuando sea necesario.")))
    (add-to-list 'gptel-directives directive))
  (setq gptel-backend (gptel-make-deepseek "Deepseek"
                        :stream t
                        :key #'gptel-api-key-from-auth-source)))

(use-package elfeed
  :ensure t
  :defer t
  :custom
  (elfeed-db-directory "~/.elfeed/")
  (elfeed-feeds '(("https://planet.emacslife.com/atom.xml" emacs)
                  ("https://emacsredux.com/atom.xml" emacs)
                  ("https://linmob.net/feed.xml" linux)
                  "https://blog.lx.oliva.nom.br/index.en.atom"
                  ("https://xkcd.com/rss.xml" comic)
                  ("https://samwho.dev/rss.xml" programming)
                  ("https://www.localfirstnews.com/rss/" programming)
                  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCiiTssXxklIDeDBWq0tPHUA" youtube music)
                  )))

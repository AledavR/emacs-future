;; [[file:../dotemacs.org::*Org-mode][Org-mode:1]]
(provide 'rca-org)
;; Org-mode:1 ends here

;; [[file:../dotemacs.org::*Org general options][Org general options:1]]
(use-package org
  :ensure nil
  :bind ("C-c a" . org-agenda)
  :hook (org-capture-mode . org-align-tags)
  :custom
  ;; (org-todo-keywords '((sequence "IDEA" "TODO" "|" "DONE" "DROP")))
  (org-highlight-latex-and-related '(latex script entities))
  (org-agenda-files '("~/.sync/org_files/agenda/" "~/.sync/org_files/notes/"))
  (org-log-done 'time)
  (org-confirm-babel-evaluate nil)
  ;; (org-cite-global-bibliography '("~/Documents/bibliography.bib"))
  (org-image-actual-width nil)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+")))
  (org-agenda-time-grid '((daily today require-timed)
                          (800 1000 1200 1400 1600 1800 2000 2200)
                          "......"
                          "-----------------"))
  :config
  (let ((documents-path (xdg-user-dir "DOCUMENTS")))
    (setq org-cite-global-bibliography `(,(concat documents-path "/bibliography.bib"))))
  (setq org-hide-emphasis-markers t)
  (setq modus-themes-headings
        '((1 . (1.5))
          (2 . (1.3))
          (agenda-date . (1.3))
          (agenda-structure . (1.8))
          (t . (1.1))))
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (setq org-safe-remote-resources
        '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (julia . t)
     (shell . t)
     (calc . t)
     (octave . t)))
  (defun browse-steam-page (steam-id)
    (browse-url (concat "steam://advertise/" steam-id)))
  
  (org-link-set-parameters "steam"
                           :follow 'browse-steam-page)
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-agenda-mode-hook 'hl-line-mode)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (add-hook 'org-babel-after-execute-hook 'org-toggle-inline-images))
;; Org general options:1 ends here

;; [[file:../dotemacs.org::*Org-capture][Org-capture:1]]
(use-package org-capture
  :ensure nil
  :after org
  :bind ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  :preface
  (defvar my/org-academic-agenda "~/.sync/org_files/agenda/academic.org")
  (defvar my/org-personal-agenda "~/.sync/org_files/agenda/personal.org")
  (defvar my/org-idea-notebook "~/.sync/org_files/notes/ideas.org")
  (defvar my/org-dream-diary "~/.sync/org_files/notes/dreams.org")
  
  (defvar my/org-created-property
    "\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:END:")

  (defvar my/org-file-link
    "\n\nArchivo: [[%L][%f]]")
  
  (defun rc/refile-to (file headline)
    "Move current headline to specified location"
    (let ((pos (save-excursion
		 (find-file file)
		 (org-find-exact-headline-in-buffer headline))))
      (org-refile nil nil (list headline file nil pos)))
    (org-save-all-org-buffers)
    (switch-to-buffer (current-buffer)))
  
  (defun rc/idea-to-task (class)
    "Promotes an idea to a pending task"
    (interactive
     (list (completing-read "Tipo de tarea:" '("Universidad" "Personal"))))
    (org-todo "TODO")
    (my/refile-to my/org-agenda-file class))
  
  :custom
  (org-capture-templates `(
                           ("a" "academic task")
                           ("ae" "exam" entry (file+headline my/org-academic-agenda "Exam"), (concat "* TODO %^{Exam} %^g\nSCHEDULED: %^T" my/org-created-property) :empty-lines 1)
                           ("ap" "project" entry (file+headline my/org-academic-agenda "Project"), (concat "* TODO %^{Project} %^g\nDEADLINE:%^T" my/org-created-property) :empty-lines 1)
                           ("ah" "homework" entry (file+headline my/org-academic-agenda "Homework"), (concat "* TODO %^{Homework} %^g\nDEADLINE:%^T" my/org-created-property) :empty-lines 1)
                           ("p" "personal task")
                           ("pc" "constructive" entry (file+headline my/org-personal-agenda "Constructive"), (concat "* TODO %^{Task}\nDEADLINE: %^T" my/org-created-property) :empty-lines 1)
                           ("pm" "mundane" entry (file+headline my/org-personal-agenda "Mundane"), (concat "* TODO %^{Task}\nDEADLINE: %^T" my/org-created-property) :empty-lines 1)
                           ("n" "note")
                           ("ni" "idea" entry (file my/org-idea-notebook), (concat "* %^{Idea}" my/org-created-property "\n%?") :empty-lines 1)
                           ("nd" "dream" entry (file my/org-dream-diary), (concat"* %^{Dream}" my/org-created-property "\n%?") :empty-lines 1)
                           ))
  )
;; Org-capture:1 ends here

;; [[file:../dotemacs.org::*Org export options][Org export options:1]]
(use-package org
  :ensure nil
  :config
  ;; Code extracted from
  ;; https://pragmaticemacs.wordpress.com/2017/03/13/export-org-mode-headlines-to-separate-files/
  (defun org-export-headlines-to-pdf ()
    "Export all subtrees that are *not* tagged with :noexport: to
separate files.

Subtrees that do not have the :EXPORT_FILE_NAME: property set
are exported to a filename derived from the headline text."
    (interactive)
    (save-buffer)
    (let ((modifiedp (buffer-modified-p)))
      (save-excursion
        (goto-char (point-min))
        (goto-char (re-search-forward "^*"))
        (set-mark (line-beginning-position))
        (goto-char (point-max))
        (org-map-entries
         (lambda ()
           (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME")))
             (unless export-file
               (org-set-property
                "EXPORT_FILE_NAME"
                (replace-regexp-in-string " " "_" (nth 4 (org-heading-components)))))
             (deactivate-mark)
             (org-latex-export-to-pdf nil t)
             (unless export-file (org-delete-property "EXPORT_FILE_NAME"))
             (set-buffer-modified-p modifiedp)))
         "-noexport" 'region-start-level)))))
;; Org export options:1 ends here

;; [[file:../dotemacs.org::*Org export packages][Org export packages:1]]
(use-package htmlize
  :ensure t)

(use-package ox-pandoc
  :ensure t
  :custom
  (org-pandoc-options
   '((lua-filter . "pagebreak.lua")
     (standalone . t)
     (highlight-style . "tango"))))
;; Org export packages:1 ends here

;; [[file:../dotemacs.org::*Org latex preview][Org latex preview:1]]
(use-package org-latex-preview
  :config
  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)

  ;; Use dvisvgm to generate previews
  ;; You don't need this, it's the default:
  (setq org-latex-preview-process-default 'dvisvgm)
  
  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

  ;; Block C-n, C-p etc from opening up previews when using auto-mode
  (setq org-latex-preview-auto-ignored-commands
        '(next-line previous-line mwheel-scroll
                    scroll-up-command scroll-down-command))

  ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered t)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-live-debounce 0.25)
  (defun my/org-latex-preview-uncenter (ov)
    (overlay-put ov 'before-string nil))
  (defun my/org-latex-preview-recenter (ov)
    (overlay-put ov 'before-string (overlay-get ov 'justify)))
  (defun my/org-latex-preview-center (ov)
    (save-excursion
      (goto-char (overlay-start ov))
      (when-let* ((elem (org-element-context))
                  ((or (eq (org-element-type elem) 'latex-environment)
                       (string-match-p "^\\\\\\[" (org-element-property :value elem))))
                  (img (overlay-get ov 'display))
                  (prop `(space :align-to (- center (0.55 . ,img))))
                  (justify (propertize " " 'display prop 'face 'default)))
        (overlay-put ov 'justify justify)
        (overlay-put ov 'before-string (overlay-get ov 'justify)))))
  (define-minor-mode org-latex-preview-center-mode
    "Center equations previewed with `org-latex-preview'."
    :global nil
    (if org-latex-preview-center-mode
        (progn
          (add-hook 'org-latex-preview-overlay-open-functions
                    #'my/org-latex-preview-uncenter nil :local)
          (add-hook 'org-latex-preview-overlay-close-functions
                    #'my/org-latex-preview-recenter nil :local)
          (add-hook 'org-latex-preview-overlay-update-functions
                    #'my/org-latex-preview-center nil :local))
      (remove-hook 'org-latex-preview-overlay-close-functions
                   #'my/org-latex-preview-recenter)
      (remove-hook 'org-latex-preview-overlay-update-functions
                   #'my/org-latex-preview-center)
      (remove-hook 'org-latex-preview-overlay-open-functions
                   #'my/org-latex-preview-uncenter))))
;; Org latex preview:1 ends here

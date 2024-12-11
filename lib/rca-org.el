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
  (org-agenda-files '("~/.sync/org_files/agenda/" "~/.sync/org_files/notes/"))
  (org-log-done 'time)
  (org-confirm-babel-evaluate nil)
  (org-cite-global-bibliography '("~/Documents/bibliography.bib"))
  (org-agenda-time-grid '((daily today require-timed)
                          (800 1000 1200 1400 1600 1800 2000 2200)
                          "......"
                          "-----------------"))
  :config
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
     (octave . t)))
  (defun browse-steam-page (steam-id)
    (browse-url (concat "steam://advertise/" steam-id)))
  
  (org-link-set-parameters "steam"
                           :follow 'browse-steam-page)

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

;; [[file:../dotemacs.org::*Org latex options][Org latex options:1]]
(use-package org
  :ensure nil
  :custom
  (org-highlight-latex-and-related '(latex script entities))
  (org-latex-compiler "lualatex")
  :config
  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted")))
  (setq org-latex-pdf-process
        '("lualatex -shell-escape -interaction nonstopmode %f"
          "lualatex -shell-escape -interaction nonstopmode %f"))
  (setq luamagick '(luamagick :programs ("lualatex" "magick")
                              :description "pdf > png"
                              :message "you need to install lualatex and imagemagick."
                              :use-xcolor t
                              :image-input-type "pdf"
                              :image-output-type "png"
                              :image-size-adjust (1.0 . 1.0)
                              :latex-compiler ("lualatex -interaction nonstopmode -output-directory %o %f")
                              :image-converter ("magick convert -density %D -trim -antialias %f -quality 100 %O")))
  (add-to-list 'org-preview-latex-process-alist luamagick)
  (setq org-preview-latex-default-process 'luamagick))
;; Org latex options:1 ends here

;; -*- lexical-binding: t; -*-
(provide 'rca-org)

(use-package org
  :ensure nil
  :bind (("C-z C-a" . org-agenda)
         :map org-mode-map
         ("C-c C-x 1" . rc/org-update-idea)
         ("C-M-<return>" . +org-insert-math-subtree))
  :hook ((org-capture-mode . org-align-tags)
         (org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode)
         (org-agenda-mode . hl-line-mode)
         (org-babel-after-execute . org-redisplay-inline-images)
         (org-babel-after-execute . org-toggle-inline-images))
  :custom
  (org-agenda-files `(,(concat sync-directory "archive/agenda/")))
  (org-log-done 'time)
  (org-confirm-babel-evaluate nil)
  (org-agenda-skip-deadline-if-done t)
  (org-src-window-setup 'other-frame)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-scheduled-repeats-after-deadline t)
  (org-highlight-latex-and-related '(native))
  (org-image-actual-width nil)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+")))
  (org-agenda-time-grid '((daily today require-timed)
                          (800 1000 1200 1400 1600 1800 2000 2200)
                          "......"
                          "-----------------"))
  (modus-themes-headings '((1 . (1.5)) (2 . (1.3))
                           (agenda-date . (1.3))
                           (agenda-structure . (1.8))
                           (t . (1.1))))
  (org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
  :config
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (set-face-attribute 'org-latex-and-related nil :family "Aporetic Sans Mono")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (julia . t)
     (shell . t)
     (calc . t)
     (octave . t)))
  
  (add-to-list 'org-structure-template-alist '("p" . "proof"))

  ;; Org link extensions
  (defun +org-link-mpv-complete-file ()
    (let ((file (read-file-name "File: "))
    	  (pwd (file-name-as-directory (expand-file-name ".")))
    	  (pwd1 (file-name-as-directory (abbreviate-file-name
    				         (expand-file-name ".")))))
      (cond ((string-match
              (concat "^" (regexp-quote pwd1) "\\(.+\\)") file)
             (concat "mpv:" (match-string 1 file)))
    	    ((string-match
              (concat "^" (regexp-quote pwd) "\\(.+\\)")
              (expand-file-name file))
             (concat "mpv:" (match-string 1 (expand-file-name file))))
    	    (t (concat "mpv:" file)))))

  (defun +org-link-open-in-mpv (file)
    "Opens linked file in an new mpv process"
    (start-process "open file" nil "mpv" "--title=mpv_emacs" (expand-file-name file)))
  
  (defun +org-link-remote-open-in-mpv (url)
    "Opens linked file in an new mpv process"
    (start-process "open url" nil "mpv" "--title=mpv_emacs" url))
  
  (defun +browse-steam-page (steam-id)
    (browse-url (concat "steam://advertise/" steam-id)))
  
  (org-link-set-parameters "steam" :follow 'browse-steam-page)
  (org-link-set-parameters "mpv" :complete '+org-link-mpv-complete-file :follow '+org-link-open-in-mpv)
  (org-link-set-parameters "mpv-url" :follow '+org-link-remote-open-in-mpv)

  ;; Org notes functions
  
  (defvar +org-math-bodies '("Proposición" "Teorema" "Corolario" "Nota"))
  
  (defun +org-get-top-header-title ()
    (let ((title (substring-no-properties
                  (if (= (org-outline-level) 1)
                      (org-get-heading) (org-display-outline-path)))))
      (replace-regexp-in-string " - Definición" "" title)))

  (defun +org-insert-math-subtree (type)
    (interactive (list (completing-read "Tipo: " +org-math-bodies nil t)))
    (let ((title (+org-get-top-header-title)))
      (if (= (org-outline-level) 1)
          (org-insert-subheading 4)
        (org-insert-heading))
      (insert (concat title " - " type " "))))


  (defvar +org-table-replacement-alist
    '(("v" . "\\\\downarrow")
      ("^" . "\\\\uparrow")
      (">" . "\\\\xrightarrow")
      ("<" . "\\\\xleftarrow")
      ("<>" . "\\\\xrightleftharpoons")
      ("q" . "\\\\quad"))
    "List of values replaced in org-table custom export
  commands")

  (defun +org-table-to-commutative-diagram ()
    (interactive)
    (unless (org-at-table-p) (user-error "Not at a table"))
    (mapc (lambda (x)
            (replace-regexp-in-region
             (concat "~" (car x) "~") (cdr x) (org-table-begin) (org-table-end)))
          +org-table-replacement-alist)
    (let* ((table (org-table-to-lisp))
           (params '(:backend latex :raw t :environment "array"))
           (replacement-table
            (replace-regexp-in-string
             "  +" " "
             (replace-regexp-in-string
              "{array}{\\(l+\\)}"
              (lambda (match) (concat "{array}{" (make-string (- (length match) 9) ?c) "}")) (orgtbl-to-latex table params)))))
      (kill-region (org-table-begin) (org-table-end))
      (open-line 1)
      (push-mark)
      (insert "\\[" replacement-table "\\]")))

  (defun +org-table-from-latex-table ()
    (interactive)
    (search-backward "\[")
    (kill-whole-line)
    (set-mark (point))
    (search-forward "\]")
    (kill-whole-line)
    (backward-char)
    (activate-mark)
    (let ((beg (region-beginning))
          (end (region-end)))
      (replace-regexp-in-region "^\\|\\\\\\\\\\|&" "|" beg end)
      (goto-char beg)
      (org-table-next-field)))

  )

(use-package org-capture
  :ensure nil
  :after org
  :bind (("C-z C-c" . org-capture)
         ("C-z C-l" . org-store-link))
  :init
  (defvar my/org-academic-agenda  (concat (car org-agenda-files) "academic.org"))
  (defvar my/org-personal-agenda (concat (car org-agenda-files) "personal.org"))
  (defvar my/org-idea-notebook (concat sync-directory "archive/notebooks/ideas.org"))
  (defvar my/org-dream-diary (concat sync-directory "archive/notebooks/dreams.org"))
  
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
    (rc/refile-to my/org-personal-agenda class))
  
  :custom
  (org-capture-templates `(
                           ("a" "academic task")
                           ("ae" "exam" entry (file+headline my/org-academic-agenda "Exam") ,(concat "* TODO %^{Exam} %^g\nSCHEDULED: %^T" my/org-created-property) :empty-lines 1)
                           ("ap" "project" entry (file+headline my/org-academic-agenda "Project") ,(concat "* TODO %^{Project} %^g\nDEADLINE:%^T" my/org-created-property) :empty-lines 1)
                           ("ah" "homework" entry (file+headline my/org-academic-agenda "Homework") ,(concat "* TODO %^{Homework} %^g\nDEADLINE:%^T" my/org-created-property) :empty-lines 1)
                           ("p" "personal task")
                           ("pc" "constructive" entry (file+headline my/org-personal-agenda "Constructive") ,(concat "* TODO %^{Task}\nDEADLINE: %^T" my/org-created-property) :empty-lines 1)
                           ("pm" "mundane" entry (file+headline my/org-personal-agenda "Mundane") ,(concat "* TODO %^{Task}\nDEADLINE: %^T" my/org-created-property) :empty-lines 1)
                           ("n" "note")
                           ("ni" "idea" entry (file my/org-idea-notebook) ,(concat "* %^{Idea}" my/org-created-property "\n%?") :empty-lines 1)
                           ("nd" "dream" entry (file my/org-dream-diary) ,(concat"* %^{Dream}" my/org-created-property "\n%?") :empty-lines 1)
                           ("i" "ideas management")
                           ("ic" "make constructive task from idea" entry (file+headline my/org-personal-agenda "Constructive") ,(concat "* TODO %a \nDEADLINE %^T" my/org-created-property "\n%?") :empty-lines 1)
                           ("im" "make mundane task from idea" entry (file+headline my/org-personal-agenda "Mundane") ,(concat "* TODO %a \nDEADLINE %^T" my/org-created-property "\n%?") :empty-lines 1)
                           ))
  )

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

(use-package htmlize
  :ensure t)

(use-package ox-pandoc
  :ensure t
  :custom
  (org-pandoc-options
   '((lua-filter . "pagebreak.lua")
     (standalone . t)
     (highlight-style . "tango"))))

(use-package org-latex-preview
  :config
  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)

  ;; Use dvisvgm to generate previews
  ;; You don't need this, it's the default:
  (setq org-latex-preview-process-default 'dvisvgm)

  ;;(setq-default  org-latex-preview-preamble
  ;;               "\\documentclass{minimal}\12[DEFAULT-PACKAGES]\12[PACKAGES]\12\\usepackage{amsmath}\12\\usepackage{amssymb}\12\\usepackage{xcolor}\12\\DeclareMathOperator{\\Dom}{Dom}\12\\DeclareMathOperator{\\Ran}{Ran}")
  (setq-default  org-latex-preview-preamble
                 "\\documentclass{minimal}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{mathtools}
\\usepackage{xcolor}")
  
  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

  ;; Block C-n, C-p etc from opening up previews when using auto-mode
  (setq org-latex-preview-auto-ignored-commands
        '(next-line previous-line mwheel-scroll
                    scroll-up-command scroll-down-command))

  ;; Enable consistent equation numbering
  (setq org-latex-preview-cache 'temp)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)
  
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

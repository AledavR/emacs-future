;; -*- lexical-binding: t; -*-
(provide 'rca-tex)

(defvar rc/latex-subdir-plural
  '("figure" "table" "image" "section")
  "List of latex filetypes which need a plural form")

(defun rc/is-main-latex-file ()
  "Returns t if the current file is the main tex file, nil otherwise"
  (when (equal (file-name-base buffer-file-name) "main") t))

(defun rc/latex-file-subdirectory (filetype)
  "Define the subdirectory in a latex project for the filetype submitted as
input"
  (let* ((file-path-prefix
          (if (rc/is-main-latex-file) "./" "../"))
         (file-type-subdir
          (if (member filetype rc/latex-subdir-plural)
              (concat filetype "s/")
            (concat filetype "/")))
         (file-type-subdir-with-prefix
          (concat file-path-prefix file-type-subdir))
         (file-path
          (read-file-name "File: " file-type-subdir-with-prefix "" t))
         (file-relative-path
          (replace-regexp-in-string
           (concat ".*" file-type-subdir "\\(.*\\)")
           (concat file-type-subdir-with-prefix "\\1") file-path)))
    (format "%s" file-relative-path)))

(defun rc/latex-insert-file (&optional filetype)
  "Insert the relative path to a latex extra file in a subdirectory"
  (interactive "P")
  (if filetype
      (let ((filepath
             (rc/latex-file-subdirectory filetype)))
        (insert filepath))
    (let* ((filetype
            (completing-read "File type: "
                             '("image" "figure" "table" "code" "section") nil t))
           (filepath
            (rc/latex-file-subdirectory filetype)))
      (insert filepath))))

(defun rc/cdlatex-pos-cursor-insert-file (&optional filetype)
  "Function to use in cdlatex command completion"
  (cdlatex-position-cursor)
  (if filetype
      (rc/latex-insert-file filetype)
    (let ((filetype (completing-read
                     "File type: " '("figure" "table" "section") nil t)))
      (rc/latex-insert-file filetype))))

(defun rc/latex-array-separation ()
  (when (line-contains? "&")
    (progn
      (replace-regexp-in-line "&" " & ")
      (LaTeX-indent-line)
      (beginning-of-line-text)
      (left-char 1))))

(advice-add 'LaTeX-insert-item :after #'rc/latex-array-separation)

(use-package tex
  :ensure auctex
  :after pdf-tools
  :preface
  (defun rc/latex-init ()
    "Defines what modes are activated by default when entering AuCtex mode"
    (prettify-symbols-mode)
    (turn-on-cdlatex)
    (outline-minor-mode)  
    ;; (rc/auctex-macros)
    (TeX-source-correlate-mode t)
    (tex-fold-mode 1)
    (TeX-PDF-mode t)
    (reftex-mode t)
    (LaTeX-math-mode t))
  :init
  ;; Correct way to call hooks for auctex
  (add-hook 'LaTeX-mode-hook 'rc/latex-init)
  (setopt
   TeX-fold-macro-spec-list
   '(("{1}" ("emph")) ("{1}" ("textbf"))
     ("{1}" ("textit")) ("[1]:||►" ("item"))
     ("§ {1}" ("section" "section*"))
     ("[f]→‖{1}‖" ("footnote" "marginpar"))
     ("[c]→‖{1}‖" ("cite")) ("[l]→‖{1}‖" ("label"))
     ("[r]→‖{1}‖" ("ref" "pageref" "eqref" "footref"))
     ("[i]→‖{1}‖" ("index" "glossary"))
     ("§§ {1}" ("subsection" "subsection*"))
     ("§§§ {1}" ("subsubsection" "subsubsection*"))
     ("¶¶ {1}" ("subparagraph" "subparagraph*"))
     ("¶ {1}" ("paragraph" "paragraph*"))))
  :custom
  (TeX-parse-self t "Enable parse on load")
  (TeX-auto-save t "Enable parse on save")
  (TeX-arg-input-file-search 'nil "Find file manually")
  :config
  (setq-default preview-scale 1.4
                prettify-symbols-unprettify-at-point 'right-edge
                preview-scale-function (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale))
                TeX-source-correlate-method 'synctex
                TeX-source-correlate-start-server t
                TeX-master nil
                TeX-view-program-selection '((output-pdf "PDF Tools")))

  ;; LaTeX math delimiters functions

  (defun +TeX--modify-math-delimiters (open-replacement close-replacement)
    (when (string-equal (match-string 0) "\\[")
      (replace-match open-replacement)
      (re-search-forward "\\\\\\]")
      (replace-match close-replacement))
    (when (string-equal (match-string 0) "\\(")
      (replace-match open-replacement)
      (re-search-forward "\\\\)")
      (replace-match close-replacement)))

  (defun +TeX-remove-math-delimiters ()
    "Removes math delimiters from the math block at point"
    (interactive)
    (unless (texmathp) (user-error "Not in math expression"))
    (save-mark-and-excursion
      (if (texmathp-match-environment nil)
          (progn
            (LaTeX-mark-environment)
            (re-search-forward "\\\\\\[\\|\\\\(" (region-end) t)
            (+TeX--modify-math-delimiters "" ""))
        (re-search-backward "\\\\\\[\\|\\\\(")
        (+TeX--modify-math-delimiters "" ""))))
  
  (defun +TeX-change-math-delimiter ()
    (interactive)
    (unless (texmathp) (user-error "Not in math expression"))
    (save-excursion
      (re-search-backward "\\\\\\[\\|\\\\(")
      (when (string-equal (match-string 0) "\\[")
        (+TeX--modify-math-delimiters "\\\\(" "\\\\)"))
      (when (string-equal (match-string 0) "\\(")
        (+TeX--modify-math-delimiters "\\\\[" "\\\\]"))))

  (defun +Tex-mark-math-block ()
    (interactive)
    (unless (texmathp) (user-error "Not in math expression"))
    (re-search-backward "\\\\\\[\\|\\\\(")
    (push-mark (point) t t)
    (when (string-equal (match-string 0) "\\[")
      (re-search-forward "\\\\\\]"))
    (when (string-equal (match-string 0) "\\(")
      (re-search-forward "\\\\)")))
  
  ;; Math block minor mode
  (defun +LaTeX-math-texmathp () t)
  (defun +LaTeX-math-texmathp-advice (fun)
    (if LaTeX-math-block-mode
        (+LaTeX-math-texmathp)
      (funcall fun)))
  
  (define-minor-mode LaTeX-math-block-mode
    "Mode for entering math blocks in external programs."
    :lighter " Math Block"
    :keymap `(
              (,(kbd "C-x C-s") . +save-n-kill-buffer-delete-frame)
              )
    (advice-add 'texmathp :around #'+LaTeX-math-texmathp-advice)))

(use-package cdlatex
  :ensure t
  :defer t
  :init
  (defvar rc/cdlatex-env-list
    '(("axiom" "\\begin{axiom}\nLABEL\n?\n\\end{axiom}\n" nil)
      ("theorem" "\\begin{theorem}\nLABEL\n?\n\\end{theorem}\n" nil))
    "cdlatex enviroments")
  (defvar rc/cdlatex-command-list
    '(
      ;; ("ref"
      ;;  "Insert a new reference"
      ;;  "" consult-reftex-insert-reference nil t nil)
      ("gph"
       "Insert an image"
       "\\includegraphics[width=0.6\\linewidth]{?}"
       rc/cdlatex-pos-cursor-insert-file ("image") t nil)
      ("inp"
       "Input a file"
       "\\input{?}"
       rc/cdlatex-pos-cursor-insert-file nil t nil)
      ("inc"
       "Include a file"
       "\\include{?}"
       rc/cdlatex-pos-cursor-insert-file nil t nil)
      ("dm"
       "Insert a math display block"
       "\\[ ? \\]" cdlatex-position-cursor nil t nil)
      ("mm"
       "Insert an inline math block"
       "\\( ? \\)" cdlatex-position-cursor nil t nil)
      ("int"
       "Insert simple integral"
       "\\int_{?}" cdlatex-position-cursor nil nil t)
      ("oint"
       "Insert closed integral"
       "\\oint_{?}" cdlatex-position-cursor nil nil t)
      ("dv"
       "Insert a spaced differential variable"
       "\\, d?" cdlatex-position-cursor nil nil t)
      ("d."
       "Insert dots"
       "\\dots" cdlatex-position-cursor nil nil t)
      ("t."
       "Insert therefore symbol"
       "\\therefore" cdlatex-position-cursor nil nil t)
      ("intd"
       "Insert a definite integral limits"
       "\\biggr\\vert_{?}^{}" cdlatex-position-cursor nil nil t)
      ("int2"
       "Insert a definite integral limits"
       "\\iint" cdlatex-position-cursor nil nil t)
      ("int3"
       "Insert a definite integral limits"
       "\\iiint" cdlatex-position-cursor nil nil t)
      ("br"
       "Insert an escaped pair of braquets"
       "\\{ ? \\}" cdlatex-position-cursor nil nil t)
      ("sci"
       "Insert scientific notation"
       "\\times 10^{?}" cdlatex-position-cursor nil nil t))
    "cdlatex custom commands")
  (setq cdlatex-env-alist rc/cdlatex-env-list
        cdlatex-command-alist rc/cdlatex-command-list)
  :custom
  (cdlatex-paired-parens "$([{")
  (cdlatex-math-modify-alist '((111 "\\operatorname" nil t nil nil)
                               (66 "\\mathbb" nil t nil nil)))
  (cdlatex-math-symbol-alist '((61 ("\\Leftrightarrow" "\\Longleftrightarrow" "\\coloneq"))))
  :bind ( :map cdlatex-mode-map
          ("C-<return>" . nil)
          ("´" . cdlatex-math-symbol)
          ("<tab>" . cdlatex-tab)))

(use-package pdf-tools
  :ensure t
  ;; :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook ((pdf-view-mode . pdf-links-minor-mode)
         (pdf-view-mode . pdf-view-themed-minor-mode)
         (pdf-view-mode . pdf-sync-minor-mode))
  :init
  (pdf-tools-install)
  :custom
  (pdf-view-display-size 'fit-page "Fit to page by default")
  (pdf-annot-activate-created-annotations t "Activate annotations")
  :config
  (defvar mode-line-format--old nil
    "Variable to store last mode line format to restore it
when deactivating presentation-mode")

  (define-minor-mode presentation-mode
    "Remove visual elements for presentation"
    :global nil
    (if presentation-mode
        (progn
          (setq mode-line-format--old mode-line-format)
          (setq mode-line-format nil)
          (tab-bar-mode -1))
      (setq mode-line-format mode-line-format--old)
      (tab-bar-mode)))
  (define-key pdf-view-mode-map (kbd "<f5>") 'presentation-mode)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward))

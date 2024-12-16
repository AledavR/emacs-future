;; [[file:../dotemacs.org::*Tools][Tools:1]]
(provide 'rca-tools)
;; Tools:1 ends here

;; [[file:../dotemacs.org::*Ebuku][Ebuku:1]]
(use-package ebuku
  :ensure t
  :defer t
  :bind ("C-z b" . ebuku)
  :custom-face
  (ebuku-tags-face ((t (:inherit font-lock-keyword-face))))
  (ebuku-title-face ((t (:inherit font-lock-constant-face))))
  :custom
  (ebuku-results-limit 25))
;; Ebuku:1 ends here

;; [[file:../dotemacs.org::*Denote][Denote:1]]
(use-package denote
  :ensure t
  :config
  (setq denote-directory "~/.sync/archive/notes"))
;; Denote:1 ends here

;; [[file:../dotemacs.org::*Embark][Embark:1]]
(use-package embark
  :ensure t
  :bind (("C-c o" . embark-act)
         :map embark-general-map
         ("G" . embark-google-search))
  :preface
  (defun embark-google-search (term)
    (interactive "sSearch Term: ")
    (browse-url (format "https://google.com/search?q=%s" term)))
  :config
  (add-to-list 'display-buffer-alist '("\\*Embark Actions\\*" (display-buffer-pop-up-window))))

(use-package embark-consult
  :ensure t)
;; Embark:1 ends here

;; [[file:../dotemacs.org::*Citar][Citar:1]]
(use-package citar
  :ensure t
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
          citar-notes-paths
          (mapcar (lambda (entry) (concat documents-path entry)) '("/library/notes/")))
    (setq citar-file-note-extensions '("org")
          citar-library-file-extensions '("pdf")
          citar-bibliography org-cite-global-bibliography)))

(use-package citar-embark
  :ensure t
  :diminish
  :after citar embark
  :config
  (defun open-pdf-file-externally (citekey)
    (let ((file (car (car (hash-table-values (citar-get-files citekey))))))
      (if file (start-process "sioyek" nil "sioyek" file)
        (message "No pdf file found with this citekey"))))
  (define-key citar-embark-citation-map (kbd "F") #'open-pdf-file-externally)
  (define-key citar-embark-map (kbd "F") #'open-pdf-file-externally)
  (citar-embark-mode))
;; Citar:1 ends here

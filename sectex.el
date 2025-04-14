;; (require 'auctex)

(defcustom sectex-latex-subdir-plural
  '("figure" "table" "image" "section")
  "Type of files that use plural forms.
Use singular forms when customizing the variable"
  :type '(repeat string))

(defun sectex--is-master-file ()
  "Check if current file is the main project file."
  (when (equal TeX-master t) t))

(defun sectex--add-plural-form (filetype)
  (concat filetype (if (member filetype sectex-latex-subdir-plural) "s/" "/")))

(defun sectex--get-parent-level ()
  "Get the parent level relative to the main file.
Returns a `string'."
  (if (not (sectex--is-master-file))
    (file-name-directory TeX-master) "./"))

(defun sectex--latex-file-directory (filetype)
  "Define the subdirectory in a latex project for the filetype submitted as input"
  (concat (sectex--get-parent-level)
          (concat filetype (if (member filetype sectex-latex-subdir-plural) "s/" "/"))))

(defun sectex--ask-for-file (filetype)
  (let ((file-subdir (sectex--latex-file-directory filetype)))
    (concat file-subdir
            (replace-regexp-in-string (expand-file-name file-subdir) ""
                                      (expand-file-name (read-file-name "File: " file-subdir "" t))))))

(defun sectex-insert-file (&optional filetype)
  (interactive "P")
  (if filetype
      (insert (sectex--ask-for-file filetype))
    (insert (sectex--ask-for-file
             (completing-read "File type: " '("image" "figure" "table" "code" "section") nil t)))))

;; TODO Think of a better way of making this thing valuable

;; (defun sectex-append-region-to-section (start end)
;;   "Append file to an existing section file."
;;   (interactive "r")
;;   (append-to-file start end 
;;                   (sectex--ask-for-file "section"))
;;   (kill-region start end))

;; (defun sectex-move-region-to-section (start end section)
;;   "Moves a region in a tex to a section file, deletes the region and
;; includes the new file in the main file."
;;   (interactive "r\nsSection: ")
;;   (setq file-path-prefix (if (sectex--is-master-file) "./" "../"))
;;   (setq section (concat file-path-prefix "sections/" section ".tex"))
;;   (write-region start end section t)
;;   (kill-region start end)
;;   (insert (concat "\\include{" section "}")))

(defun search-regexp-in-file (file regexp)
  "Search for REGEXP in FILE and return the position of the match.
If no match is found, returns nil."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward regexp nil t)
        (progn (beginning-of-line) (backward-char))
      (end-of-buffer))
    (point)))

;; (defun prueba-de-append (start end)
;;   (interactive "r")
;;   (write-region start end (read-file-name "File: " nil "" t)
;;                 (save-excursion
;;                   (let ((local-var-block (search-forward-regexp (concat comment-start "+ Local Variables") nil t)))
;;                     (if local-var-block
;;                         (progn (beginning-of-line) (backward-char))
;;                       (end-of-buffer))
;;                     (point)))))

(defun prueba-de-append (start end)
  (interactive "r")
  (let ((file (read-file-name "File: " nil "" t)))
    (write-region start end file (search-regexp-in-file file ";; Local Variables"))))

                  
(provide 'sectex)

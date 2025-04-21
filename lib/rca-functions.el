;; [[file:../dotemacs.org::*Custom functions][Custom functions:1]]
(provide 'rca-functions)
;; Custom functions:1 ends here

;; [[file:../dotemacs.org::*File manipulation][File manipulation:1]]
(defun rc/file-find-config ()
  "Find config file interactively"
  (interactive)
  (find-file (locate-user-emacs-file
              (completing-read "Select config file: " emacs-config-files))))

(defun rc/file-get-el (dir)
  "Get all elisp files from a directory"
  (directory-files dir nil "^[^.].*el$"))

(defun rc/find-stow-file ()
  (interactive)
  (find-file
   (completing-read "Select config file: "
                    (directory-files-recursively
                     stow-files ".*" nil
                     (lambda (dir)
                       (not (string-match-p ".*git.*" dir)))))))

(defun find-file-at-point-other-window ()
  (interactive)
  (let ((ffap-file-finder #'find-file-other-window))
    (find-file-at-point)))


(defun rc/insert-wallpaper-file ()
  (interactive)
  (insert
   (file-name-sans-extension
    (file-name-nondirectory
     (completing-read "Select wallpaper: "
                      (directory-files-recursively
                       wallpaper-files ".*"))))))

(defun rc/locate-or-create-directory (dir)
  "Search for a directory and create it if doesn't exists"
  (let ((dir_ (locate-user-emacs-file dir)))
     (when (not (file-directory-p dir_))
       (make-directory dir_)) dir_))

(defun rc/config-insert-footer ()
  (let ((inhibit-message t))
    (goto-char (point-max))
    (insert "\n;; Local Variables:\n;; eval: (add-hook 'after-save-hook (lambda ()(org-babel-detangle)) nil t)\n;; End:")
    (save-buffer)))
;; File manipulation:1 ends here

;; [[file:../dotemacs.org::*List manipulation][List manipulation:1]]
(defun rc/list-append-str (string list &optional position)
  "Appends a string to each element of a list.
If POSITION is nil appends to the beginning of each element."
  (mapcar (lambda (element)
            (if position
                (concat element string)
              (concat string element)))
          list))

(defun rc/list-merge-sublists (list)
  "Merge all the sublists in a list"
  (let (value)
    (dolist (elt list value)
      (setq value (append value elt)))))

(defun rc/list-select-random (items)
  "Selects a random element from a list"
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))
;; List manipulation:1 ends here

;; [[file:../dotemacs.org::*Miscellaneous][Miscellaneous:1]]
(defun rc/number-between (number bot top)
  "Determines if a number is within a range"
  (if (< number top)
      (if (> number bot)
          t nil)
    nil))

(defun rc/time-is-day ()
  "Determines if the current time is considered day"
  (if (rc/number-between
       (nth 2 (decode-time (current-time)))
       8 18)
      t nil))

(defun rc/format-time-string-es (format &optional time)
  "Formatea la hora en español, capitalizando solo los nombres de días y meses."
  (let* ((system-time-locale "es_ES.UTF-8")
         (raw (format-time-string format time))
         ;; Lista de días y meses en español
         (dias '("lunes" "martes" "miércoles" "jueves" "viernes" "sábado" "domingo"))
         (meses '("enero" "febrero" "marzo" "abril" "mayo" "junio"
                  "julio" "agosto" "septiembre" "octubre" "noviembre" "diciembre")))
    ;; Capitalizar si palabra coincide con día o mes
    (replace-regexp-in-string
     "\\b\\w+\\b"
     (lambda (word)
       (if (member word (append dias meses))
           (capitalize word)
         word))
     raw)))

(defun rc/truncate-lines-off ()
  "Command to set truncate-lines to t in mode hooks"
  (setq truncate-lines t))

(defun rc/export-code-block-for-message (start end)
  "Copy current region and format it to a markdown codeblock"
  (interactive "r")
  (setq code-block (buffer-substring start end))
  (setq code-block-formatted (concat "```\n" code-block "```"))
  (deactivate-mark)
  (kill-new code-block-formatted))

(defun line-contains? (string)
  (s-contains? string
               (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))

(defun replace-regexp-in-line (regexp replacement)
  (replace-regexp regexp replacement nil
                  (line-beginning-position)
                  (line-end-position)))

(defun current-line-empty-p ()
  "Return t if the current line is empty otherwise returns nil"
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:blank:]]*$")))

(defun rc/wrap-in-question-marks ()
  (interactive)
  (let ((inicio (region-beginning))
        (fin (region-end)))
    (save-excursion
      (goto-char inicio)
      (insert "¿")
      (goto-char (+ fin 1))
      (when (eq (char-before) ?.) 
        (delete-char -1))
      (insert "?"))))

(defun rc/org-update-idea ()
  "Adds a timestamp at the end of the current subtree."
  (interactive)
  (org-mark-subtree)
  (exchange-point-and-mark)
  (deactivate-mark)
  (previous-line)
  (open-line 1)
  (newline)
  (insert "UPDATE ")
  (org-insert-timestamp (current-time) t t)
  (insert ": ")
  (bookmark-set "org-last-updated-idea"))

(defun +diary-schedule-class (start-month start-day end-month end-day year days-of-week)
  (and (diary-block start-month start-day year
                    end-month end-day year)
       (or (cl-some (lambda (p) (= p (calendar-day-of-week date)))
                    days-of-week))))
;; Miscellaneous:1 ends here

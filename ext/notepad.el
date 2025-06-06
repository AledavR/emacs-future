;;;; SEARCH-REGEXP-IN-FILE

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


;;;; ESHELL/DO

(defun eshell/do (&rest args)
  "Execute a command sequence over a collection of file elements.
Separate the sequence and the elements with a `::' string.
For instance:

    do chown _ angela :: *.org(u'oscar')

The function substitutes the `_' sequence to a single filename
element, and if not specified, it appends the file name to the
command. So the following works as expected:

    do chmod a+x :: *.org"
  (seq-let (forms elements) (-split-on "::" args)
    (dolist (element (-flatten (-concat elements)))
      (message "Working on %s ... %s" element forms)
      (let* ((form (if (-contains? forms "_")
                       (-replace "_" element forms)
                     (-snoc forms element)))
             (cmd  (car form))
             (args (cdr form)))
        (eshell-named-command cmd args)))))


;;;; ORG-STEAM-LINK-PREVIEW

(defun org-steam-link-preview (ov path link)
  "Show steam link appname"
  (overlay-put ov 'display (concat "Game: " (get-steam-app-name path))))

(require 'json)

(defun get-steam-app-name (appid)
  "Obtiene el nombre de la aplicación de Steam usando su appid."
  (let* ((url (concat "https://store.steampowered.com/api/appdetails/?appids=" appid "&filters=basic"))
         (json-response (with-current-buffer (url-retrieve-synchronously url)
                          ;; Buscar el punto donde terminan las cabeceras HTTP
                          (goto-char (point-min))
                          (re-search-forward "\n\n")  ;; Salta las cabeceras HTTP
                          (prog1
                              (buffer-substring-no-properties (point) (point-max))  ;; Coge solo el cuerpo JSON
                            (kill-buffer)))))
    ;; Procesamos la respuesta JSON
    (let ((json-object-type 'hash-table)  ; Para asegurar que la respuesta se lea como un hash-table
          (json-array-type 'list))
      (let ((json-data (json-read-from-string json-response)))
        ;; Accedemos al hash-table correspondiente al appid
        (let ((data (gethash  appid json-data)))
          ;; Accedemos a los datos dentro de "data" usando gethash
          (if (and data (gethash "data" data))
              (let ((app-data (gethash "data" data)))
                (if (gethash "name" app-data)
                    (gethash "name" app-data)
                  (message "No se encontró el nombre de la aplicación")))
            (message "No se encontraron datos para el appid")))))))

;; Ejemplo de uso:
(get-steam-app-name 3224600)  ;; Esto debería devolver el nombre de la aplicación


;;;; ORG-AGENDA-CUSTOM-COMMANDS

(setq org-agenda-custom-commands
      '(("c" "College classes filter"
         ((agenda "" ((org-agenda-time-grid nil))))
         ((org-agenda-tag-filter-preset '("+class")))
         ("~/clases_semana.txt"))))

;;;; ADD WORD TO TECHNICAL DICTIONARY

(defun add-word-to-technical-dictionary ()
  "Remove the first line and add the word at point to the end of the dictionary."
  (interactive)
  (let ((word (thing-at-point 'word t))  ; Get the word under the cursor
        (file "/home/rcaled/.config/enchant/hunspell/estec.dic"))
    (if (not word)
        (message "No word at point.")
      (if (file-exists-p file)
          (with-temp-buffer
            (insert-file-contents file)
            ;; Remove the first line (which contains '1')
            (goto-char (point-min))
            (delete-region (point-min) (line-end-position))
            (insert (number-to-string (count-lines (point-min) (point-max))))
            ;; Add the word at the end of the file
            (goto-char (point-max))
            (newline)
            (insert word)
            (write-file file)
            (message "Word '%s' added to dictionary." word))
        (message "Dictionary file does not exist.")))))

;;;; TABLE TO COMM DIAG CONVERSION

(defvar org-table-replacement-alist
  '(("v" . "\\\\downarrow")
    ("^" . "\\\\uparrow")
    (">" . "\\\\xrightarrow")
    ("<" . "\\\\xleftarrow")
    ("<>" . "\\\\xrightleftharpoons")
    ("q" . "\\\\quad"))
  "List of values replaced in org-table custom export
commands")

(defun org-table-to-commutative-diagram ()
  (interactive)
  (unless (org-at-table-p) (user-error "Not at a table"))
  (mapc (lambda (x)
          (replace-regexp-in-region
           (concat "~" (car x) "~") (cdr x) (org-table-begin) (org-table-end)))
        org-table-replacement-alist)
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

(defun org-table-from-latex-table ()
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

(defun org-table-test ()
  (interactive)
  (message (orgtbl-to-latex (org-table-to-lisp) '(:backend latex :raw t :environment "array"))))

(defun org-table-test-2 ()
  (interactive)
  (org-table-to-lisp))


;;; REGION FORMATING

(defun rc/act-on-region-or-line ()
  (if mark-active (list (region-beginning) (region-end))
    (list (line-beginning-position) (line-beginning-position 2))))

(defun rc/delete-whitespace-and-indent (start end)
  (interactive (rc/act-on-region-or-line))
  (replace-regexp-in-region "[[:space:]]+" " " start end)
  (indent-region start end))

(defun rc/fix-semicolon-and-commas (start end)
  (interactive (rc/act-on-region-or-line))
  (replace-regexp-in-region "[[:space:]]*\\([,;]\\)" "\\1" start end)
  (replace-regexp-in-region "\\([,;]\\)\\([[:graph:]]\\)" "\\1 \\2" start end))

(defun rc/format-region-or-line (start end)
  (interactive (rc/act-on-region-or-line))
  (rc/delete-whitespace-and-indent start end)
  (rc/fix-semicolon-and-commas start end))

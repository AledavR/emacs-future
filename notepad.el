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

;; [[file:../dotemacs.org::*Package management][Package management:1]]
;; (provide 'package-manager)

;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (package-initialize)

(provide 'package-manager)

(defvar elpaca-installer-version 0.10)

(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))

(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (call-process "git" nil buffer t "clone"
                                        (plist-get order :repo) repo)))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; (use-package org
;;   :defer
;;   :ensure `(org
;;             :remotes ("tecosaur"
;;                       :repo "https://code.tecosaur.net/tec/org-mode.git"
;;                       :branch "dev")
;;             :files (:defaults ("etc/styles/" "etc/styles/*" "doc/*.texi"))
;;             :build t
;;             :pre-build
;;             (progn
;;               (with-temp-file "org-version.el"
;;                 (require 'lisp-mnt)
;;                 (let ((version
;;                        (with-temp-buffer
;;                          (insert-file-contents "lisp/org.el")
;;                          (lm-header "version")))
;;                       (git-version
;;                        (string-trim
;;                         (with-temp-buffer
;;                           (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
;;                           (buffer-string)))))
;;                   (insert
;;                    (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
;;                    (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
;;                    "(provide 'org-version)\n")))
;;               (require 'elpaca-menu-org)
;;               (elpaca-menu-org--build))
;;             :pin nil))

(use-package org
  :defer
  :ensure `(org :repo "https://code.tecosaur.net/tec/org-mode.git/"
                :branch "dev"))

(elpaca-wait)

;; (defun +elpaca-unload-seq (e)
;;   "Unload seq before continuing the elpaca build, then continue to build the recipe E."
;;   (and (featurep 'seq) (unload-feature 'seq t))
;;   (elpaca--continue-build e))

;; (elpaca `(seq :build ,(append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
;;                                           elpaca--pre-built-steps
;;                                         elpaca-build-steps))
;;                              (list '+elpaca-unload-seq 'elpaca--activate-package))))


(unless (equal system-type 'gnu/linux)
  (elpaca-no-symlink-mode))
;; Package management:1 ends here

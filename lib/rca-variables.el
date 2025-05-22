;; -*- lexical-binding: t; -*-
(provide 'rca-variables)

(defcustom sync-directory (expand-file-name "~/.sync/")
  "Directory where synchonized files are stored")

(defcustom user-cache-directory (locate-user-emacs-file "cache/")
  "Directory where temporal user files are stored")

(defcustom backup-directory (concat user-cache-directory "saves/")
  "Directory where backup files are stored")

(defcustom emacs-config-dirs '("" "lib/" "ext/")
  "Directories where emacs configuration files are stored")

(defcustom dotfiles-dirs (expand-file-name "~/dotfiles/")
  "Directory where user configuration files are stored")

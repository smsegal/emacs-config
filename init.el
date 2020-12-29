;;; init.el -*- lexical-binding: t; -*-

;; load straight so we can use the correct version of org to load the literate config
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)

    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Load up to date org here, so we don't have different versions loaded.
;; Also load all files in contrib
;; (straight-use-package
;;  '(org :host github
;;    :repo "emacs-straight/org-mode"
;;    :files ("*.el" "lisp/*.el" "contrib/lisp/*.el")))

;; (load-file (expand-file-name "config.el" user-emacs-directory))
(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

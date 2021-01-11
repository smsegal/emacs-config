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

;;; Personal Information
(setq user-full-name "Shane Segal"
      user-mail-address "shane@smsegal.ca")
(setq auth-sources '("~/.authinfo.gpg"))

;;; Package Management and Use-Package Setup
;; Using straight.el (loaded in ~init.el~) to load use-package and setup integration.

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(use-package straight-x :straight nil) ;; loading the additional functions from straight-x

;; Add custom code to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;;; load custom modules
(require '+core-setup)

;; load evil and the general.el package for setting keybindings
;; (this is just core evil stuff, configuration of evil takes place later)
(require '+core-keys)

;; additional packages
(require '+evil) ;; load evil configuration
(require '+ui)
(require '+project)
(require '+narrowing) ;; incremental narrowing + utils (selectrum+consult+marginalia)
(require '+spellcheck)
(require '+modeline)
(require '+buffers)
(require '+windows) ;; window management, not the OS
(require '+autocomplete) ;; + syntax checking
(require '+files) ;; file management + related
(require '+text-tools)
(require '+search-narrow) ;; searching and narrowing to page
(require '+vc) ;; magit and related
(require '+vterm)
(require '+langs) ;; general language support that doesn't belong elsewhere
(require '+lsp)
(require '+python)
(require '+latex-pdf)
(require '+web)
(require '+haskell)
(require '+org)
(require '+helpful) ;; more helpful help buffers
(require '+utils)

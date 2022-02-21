;;; init.el -*- lexical-binding: t; -*-

;; load straight so we can use the correct version of org to load the literate config
(defvar bootstrap-version)
                                        ; (setq straight-repository-branch "develop")
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
;; Using straight.el  to load use-package and setup integration.

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(require 'straight-x) ;; loading the additional functions from straight-x

;; Add custom code to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;;; load custom modules
(require '+core-setup)

;; load evil and the general.el package for setting keybindings
;; (this is just core evil stuff, configuration of evil takes place later)
(require '+core-keys)

;; additional packages
(require '+evil)          ;; load evil configuration
(require '+ui)
(require '+tty)           ;; support for terminal operation
(require '+filetree)
(require '+project)
(require '+minibuffer)     ;; incremental narrowing + utils
(require '+autocomplete)  ;; + syntax checking
(require '+spellcheck)
(require '+modeline)
(require '+buffers)
(require '+windows)       ;; window management, not the OS
(require '+files)         ;; file management + related
(require '+snippets)
(require '+text-tools)
(require '+search-narrow) ;; searching and narrowing to page
(require '+vc)            ;; magit and related
(require '+vterm)
(require '+langs)         ;; general language support that doesn't belong elsewhere
;; (require '+lsp)
(require '+eglot)
(require '+python)
;; (require '+latex-pdf)
(require '+web)
;; (require '+haskell)
(require '+org)
(require '+helpful)       ;; more helpful help buffers
(require '+utils)


;; Direnv
;; Direnv automatically adjusts the environment for you when entering a
;; directory with a ~.envrc~ file that contains the appropriate commands.
;; This should be at/near the bottom since you want this hook to be run
;; before others. Hooks are apparently a stack.
;; needs to run last as global hooks are popped off a stack
(use-package envrc
  :init (envrc-global-mode))

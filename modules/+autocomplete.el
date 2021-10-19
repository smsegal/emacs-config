;; -*- lexical-binding: t; -*-

;; Autocomplete and Syntax Checking
;; Using company for auto completion and flycheck.

;;; autocomplete
(use-package company
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  :hook (emacs-startup . global-company-mode)
  :general
  (general-imap "C-SPC" 'company-complete)
  (:keymaps 'company-search-map
   "C-s" #'company-filter-candidates))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

(use-package company-posframe
  :unless IS-MAC
  :hook (company-mode . company-posframe-mode))

;; syntax checking
(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-indication-mode 'right-fringe)
  (delq 'new-line flycheck-check-syntax-automatically)

  ;; A non-descript, left-pointing arrow
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)
  :ghook ('after-init-hook #'global-flycheck-mode))

(use-package flycheck-inline
  :after flycheck
  :ghook ('flycheck-mode-hook #'flycheck-inline-mode))

(provide '+autocomplete)

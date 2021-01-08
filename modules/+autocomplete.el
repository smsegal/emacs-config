;; -*- lexical-binding: t; -*-

;; Autocomplete and Syntax Checking
;; Using company for auto completion and flycheck.

;;; autocomplete
(use-package company
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :hook (emacs-startup . global-company-mode)
  :general
  (general-imap "C-SPC" 'company-complete)
  (:keymaps 'company-search-map
            "C-s" #'company-filter-candidates))
(use-package company-prescient
  :hook (company-mode . company-prescient-mode))
(use-package company-box
  :hook (company-mode . company-box-mode))
(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))
(use-package company-posframe
  :unless IS-MAC
  :hook (company-box-mode . company-posframe-mode))

;; syntax checking
(use-package flycheck
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook (after-init . global-flycheck-mode)
  :general
  (:prefix-map '+code-map
               "x" '(flycheck-list-errors :which-key "show errors")))

(provide '+autocomplete)

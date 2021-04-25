;; -*- lexical-binding: t; -*-

;; Autocomplete and Syntax Checking
;; Using company for auto completion and flycheck.

;;; autocomplete
;; (use-package company
;;   :disabled
;;   :config
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-idle-delay 0.0)
;;   :hook (emacs-startup . global-company-mode)
;;   :general
;;   (general-imap "C-SPC" 'company-complete)
;;   (:keymaps 'company-search-map
;;    "C-s" #'company-filter-candidates))

(use-package corfu
  :hook ((prog-mode . corfu-mode)
         (org-mode . corfu-mode))
  :init
  (setq tab-always-indent 'complete)
  :general
  ;; (general-imap :keymaps 'override
  ;;   "TAB" 'corfu-complete)
  (:keymaps 'corfu-map
   "C-j" #'corfu-next
   "TAB" #'corfu-next
   "C-k" #'corfu-previous
   "S-TAB" #'corfu-previous))

;; (use-package company-tabnine
;;   :config
;;   (add-to-list 'company-backends #'company-tabnine))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; (use-package company-quickhelp
;;   :hook (company-mode . company-quickhelp-mode))

;; (use-package company-posframe
;;   :unless IS-MAC
;;   :hook (company-mode . company-posframe-mode))

;; syntax checking
(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :ghook ('after-init-hook #'global-flycheck-mode))

(use-package flycheck-inline
  :after flycheck
  :ghook ('flycheck-mode-hook #'flycheck-inline-mode))

(provide '+autocomplete)

;; -*- lexical-binding: t; -*-

;; Autocomplete and Syntax Checking
;; Using company for auto completion and flycheck.

;;; autocomplete
;; (use-package company
;;   :config
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-idle-delay 0.0)
;;   :hook (emacs-startup . global-company-mode)
;;   :general
;;   (general-imap "C-SPC" 'company-complete)
;;   (:keymaps 'company-search-map
;;    "C-s" #'company-filter-candidates))

;; ;; (use-package company-box
;; ;;   :hook (company-mode . company-box-mode))

;; (use-package company-quickhelp
;;   :hook (company-mode . company-quickhelp-mode))

;; (use-package company-posframe
;;   :unless IS-MAC
;;   :hook (company-mode . company-posframe-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package company
  :after cape
  :init
  (let ((cape-yasnippet (cape-company-to-capf #'company-yasnippet)))
    (add-to-list completion-at-point-functions #'cape-yasnippet)))


(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  ;; (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  ;; (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (corfu-global-mode)

  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package corfu-doc-mode
  :straight (:host github :repo "galeo/corfu-doc")
  :hook (corfu-mode . corfu-doc-mode))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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

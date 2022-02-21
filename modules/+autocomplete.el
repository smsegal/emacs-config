;; -*- lexical-binding: t; -*-

;; Autocomplete and Syntax Checking
;; Using corfu for auto completion and flycheck.

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  ;; (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  ;; (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-echo-documentation t) ;; Disable documentation in the echo area
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
  (setq tab-always-indent 'complete)
  :general
  (:keymaps 'corfu-map
   "C-j" #'corfu-next
   "C-k" #'corfu-previous))

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

  ;; A non-descript, left-pointing arrow
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)
  :ghook ('after-init-hook #'global-flycheck-mode))

(use-package flycheck-inline
  :after flycheck
  :ghook ('flycheck-mode-hook #'flycheck-inline-mode))

(provide '+autocomplete)

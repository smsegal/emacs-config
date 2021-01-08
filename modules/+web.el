;; -*- lexical-binding: t; -*-

(require '+lsp)

;; {Java,Type}Script
;; The different web languages. I've also enabled LSP-mode for them above.
(use-package js2-mode
  :interpreter "node"
  :commands js2-line-break
  :hook ((js-mode . js2-minor-mode)
         (js2-mode . lsp-deferred))
  :custom
  (js-chain-indent t)
  ;; Don't mishighlight shebang lines
  (js2-skip-preprocessor-directives t)
  ;; let flycheck handle this
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil)
  ;; Flycheck provides these features, so disable them: conflicting with
  ;; the eslint settings.
  (js2-strict-trailing-comma-warning nil)
  (js2-strict-missing-semi-warning nil)
  ;; maximum fontification
  (js2-highlight-level 3)
  (js2-highlight-external-variables t)
  (js2-idle-timer-delay 0.1))

(use-package js2-refactor
  :hook (js2-minor-mode . js2-refactor-mode)
  :general
  (general-nvmap
    :keymaps 'js2-mode
    "," '(:keymap js2-refactor-mode-map)))

(use-package rjsx-mode
  :mode "/.*\\.js\\'")

(use-package json-mode)
(use-package yaml-mode
  :hook (yaml-mode . lsp-deferred))
(use-package typescript-mode)

(provide '+web)

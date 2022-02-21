;; -*- lexical-binding: t; -*-


;; Haskell
;; Basic syntax highlighting.
(use-package haskell-mode)
(use-package hlint-refactor
  :commands (hlint-refactor-refactor-buffer hlint-refactor-refactor-at-point)
  :general
  (+local-leader-def :keymaps 'haskell-mode-map
    "r" '(nil :which-key "refactor")
    "rr" #'hlint-refactor-refactor-at-point
    "rR" #'hlint-refactor-refactor-buffer))

(use-package flycheck-haskell
  :hook (flycheck-mode . flycheck-haskell-setup))

(use-package dante
  :disabled
  :after haskell-mode
  :commands 'dante-mode
  :ghook ('haskell-mode-hook '(flycheck-mode-hook 'dante-mode))
  :config
  (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint)))

;; Set up language server support.

(use-package lsp-haskell
  :custom (lsp-haskell-server-path "~/.ghcup/bin/haskell-language-server-wrapper")
  :hook (haskell-mode . lsp-deferred))

(provide '+haskell)

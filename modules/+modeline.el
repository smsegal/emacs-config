;; -*- lexical-binding: t; -*-

;; Modeline
;; I use the moody modeline (by the author of magit). I also use minions
;; to hide all the other modes active instead of diminishing them.
;; I also depend on smart-mode-line for the buffer-naming.

;; used for buffer identification in moody modeline
(use-package smart-mode-line)
(use-package minions
  :config (minions-mode 1))
(use-package moody
  :after smart-mode-line
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-sml/mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; Anzu highlights current search results in the modeline.
(use-package anzu
  :hook (after-init . global-anzu-mode))
(use-package evil-anzu)


(provide '+modeline)

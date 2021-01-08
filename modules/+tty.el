;; -*- lexical-binding: t; -*-

;; Change the shape of the cursor when running in the tty. Also enable the mouse.
(use-package evil-terminal-cursor-changer
  :straight (:host github :repo "kisaragi-hiu/evil-terminal-cursor-changer")
  :hook (tty-setup . evil-terminal-cursor-changer-activate))

(use-package xterm-mouse-mode
  :straight (:type built-in)
  :hook (tty-setup . xterm-mouse-mode))

(provide '+tty)

;; -*- lexical-binding: t; -*-

;; Buffer management packages and keybindings

;; Burly is a lightweight workspace manager that builds on top of emacs
;; bookmark system.
(use-package burly
  :straight (:host github :repo "alphapapa/burly.el")
  :general
  (:prefix-map '+buffer-map
   "o" 'burly-open-bookmark
   "w" 'burly-bookmark-windows
   "F" 'burly-bookmark-frames))

;; Switch to the scratch buffer
(use-package switch-to-buffer
  :straight (:type built-in)
  :preface
  (defun +switch-to-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))
  :general
  (:keymaps 'global-map
   (kbd "<mouse-9>") 'next-buffer
   (kbd "<mouse-8>") 'previous-buffer)
  (:prefix-map '+buffer-map
   "s" #'+switch-to-scratch))

(provide '+buffers)

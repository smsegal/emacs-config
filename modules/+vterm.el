;; -*- lexical-binding: t; -*-

;; Integrated Terminal
;; Vterm is by far the best terminal emulator. It now has support for
;; evil-mode motions and such.
(use-package vterm
  :custom
  (vterm-shell "fish")
  (vterm-buffer-name-string "vterm: %s"))

(use-package vterm-toggle
  :commands (vterm-toggle)
  :general
  (+leader-def
    "'" #'vterm-toggle)
  (:prefix-map '+open-map
   "t" #'vterm-toggle
   "T" #'vterm)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (bufname _)
                   (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 (direction . bottom)
                 (dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(provide '+vterm)

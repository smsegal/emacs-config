;; -*- lexical-binding: t; -*-

;; Buffer management packages and keybindings

;; These are both packages written by alphapapa, a prolific emacs package
;; dev. Bufler organizes buffers by mode and project and provides a good
;; UI for switching among them.

;; disabled for now in favour of consult.el
(use-package bufler
  :disabled
  :hook (after-init . bufler-mode)
  :commands bufler-ex
  :general
  (general-nvmap
    :keymaps 'bufler-list-mode-map
    "RET" #'bufler-list-buffer-switch
    ;; "TAB" #'bufler-ex
    (kbd "<escape>") #'quit-window
    "q" #'quit-window)
  (:prefix-map '+buffer-map
               "b" '(bufler-switch-buffer :which-key "switch buffer")
               "B" '(bufler-list :which-key "buffer list")))

;; Burly is a lightweight workspace manager that builds on top of emacs
;; bookmark system.
(use-package burly
  :general
  (:prefix-map '+buffer-map
               "o" 'burly-open-bookmark
               "w" 'burly-bookmark-windows
               "F" 'burly-bookmark-frames))

;; Switch to the scratch buffer

(use-package switch-to-buffer
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

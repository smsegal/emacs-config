;; -*- lexical-binding: t; -*-

;; Integrated Terminal
;; Vterm is by far the best terminal emulator. It now has support for
;; evil-mode motions and such.
(use-package vterm
  :straight nil
  :preface
  ;; Add evil specific bindings that work with vterm mode
  (defun vterm-evil-insert ()
    (interactive)
    (vterm-goto-char (point))
    (call-interactively #'evil-insert))
  (defun vterm-evil-append ()
    (interactive)
    (vterm-goto-char (1+ (point)))
    (call-interactively #'evil-append))
  (defun vterm-evil-delete ()
    "Provide similar behavior as `evil-delete'."
    (interactive)
    (let ((inhibit-read-only t))
      (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
        (call-interactively 'evil-delete))))
  (defun vterm-evil-change ()
    "Provide similar behavior as `evil-change'."
    (interactive)
    (let ((inhibit-read-only t))
      (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
        (call-interactively 'evil-change))))
  ;; (defun +evil-vterm-hook ()
  ;;   (evil-local-mode 1)
  ;;   (evil-define-key 'normal 'local "a" 'vterm-evil-append)
  ;;   (evil-define-key 'normal 'local "x" 'vterm-evil-delete)
  ;;   (evil-define-key 'normal 'local "i" 'vterm-evil-insert)
  ;;   (evil-define-key 'normal 'local "c" 'vterm-evil-change))
  ;; :ghook ('vterm-mode-hook #'+evil-vterm-hook)
  :custom
  (vterm-buffer-name-string "vterm: %s")
  :general
  (+leader-def "'" #'vterm-other-window)
  (:prefix-map '+open-map
   "t" #'vterm-other-window
   "T" #'vterm)
  (general-nmap :keymaps 'vterm-mode-map
    "a" 'vterm-evil-append
    "d" 'vterm-evil-delete
    "i" 'vterm-evil-insert
    "c" 'vterm-evil-change)
  (general-imap :keymaps 'vterm-mode-map
    "C-i" #'vterm-send-escape))

(use-package vterm-toggle
  :disabled
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

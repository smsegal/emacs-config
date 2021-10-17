;; -*- lexical-binding: t; -*-

;; Window Management
;; Undo and redo window configurations.
(use-package winner
  :straight (:type built-in)
  :hook (after-init . winner-mode)
  :general
  (:prefix-map 'evil-window-map
   "u" 'winner-undo
   "r" 'winner-redo))

;; Switch to a window with the keyboard like avy.
(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 3.0)
  :general (:prefix-map 'evil-window-map
            "w" #'ace-window
            "W" #'ace-swap-window))

;; Window Enlargement
(use-package zoom
  :custom
  (zoom-size '(0.7 . 0.7))
  (zoom-ignored-major-modes '(dired-mode
                              vterm-mode
                              help-mode
                              helpful-mode
                              rxt-help-mode
                              help-mode-menu
                              org-mode))
  (zoom-ignored-buffer-names '("*scratch*" "*info*" "*helpful variable: argv*"))
  (zoom-ignored-buffer-name-regexps '("^\\*calc" "\\*helpful variable: .*\\*"))
  (zoom-ignore-predicates (list (lambda () (< (count-lines (point-min) (point-max)) 20))))
  :general
  (:prefix-map '+toggle-map
   "z" #'zoom-mode))

(defun +window-enlargen (&optional arg)
  "Enlargen the current window (i.e. shrinks others) so you can focus on it.
Use `winner-undo' to undo this."
  (interactive "P")
  (let* ((window (selected-window))
         (dedicated-p (window-dedicated-p window))
         (preserved-p (window-parameter window 'window-preserved-size))
         (ignore-window-parameters t)
         (window-resize-pixelwise nil)
         (frame-resize-pixelwise nil))
    (unwind-protect
        (progn
          (when dedicated-p
            (set-window-dedicated-p window nil))
          (when preserved-p
            (set-window-parameter window 'window-preserved-size nil))
          (maximize-window window))
      (set-window-dedicated-p window dedicated-p)
      (when preserved-p
        (set-window-parameter window 'window-preserved-size preserved-p)))))

(general-def
  :prefix-map 'evil-window-map
  "o" #'+window-enlargen
  "O" #'delete-other-windows)

(provide '+windows)

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
  (zoom-ignored-major-modes '(dired-mode vterm-mode
                              help-mode helpful-mode
                              rxt-help-mode help-mode-menu
                              org-mode))
  (zoom-ignored-buffer-names '("*scratch*" "*info*" "*helpful variable: argv*"))
  (zoom-ignored-buffer-name-regexps '("^\\*calc" "\\*helpful variable: .*\\*"))
  (zoom-ignore-predicates (list (lambda () (< (count-lines (point-min) (point-max)) 20))))
  :general
  (:prefix-map '+toggle-map
               "z" #'zoom-mode))

;;;###autoload
(defun +window-enlargen ()
  "Enlargen the current window to focus on this one. Does not close other
windows. Activate again to undo."
  (interactive)
  (setq +window-enlargened
        (if (and +window-enlargened
                 (assq ?_ register-alist))
            (ignore (ignore-errors (jump-to-register ?_)))
          (window-configuration-to-register ?_)
          (let* ((window (selected-window))
                 (dedicated-p (window-dedicated-p window))
                 (preserved-p (window-parameter window 'window-preserved-size))
                 (ignore-window-parameters t))
            (unwind-protect
                (progn
                  (when dedicated-p
                    (set-window-dedicated-p window nil))
                  (when preserved-p
                    (set-window-parameter window 'window-preserved-size nil))
                  (maximize-window window))
              (set-window-dedicated-p window dedicated-p)
              (when preserved-p
                (set-window-parameter window 'window-preserved-size preserved-p)))
            t))))

(general-def
  :prefix-map 'evil-window-map
  "o" #'+window-enlargen
  "O" #'delete-other-windows)

(provide '+windows)

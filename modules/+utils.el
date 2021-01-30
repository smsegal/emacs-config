;; -*- lexical-binding: t; -*-

;;; Writeroom Mode
;; Enable a nicer writing environment
(use-package writeroom-mode
  :config
  (setq writeroom-global-effects '(writeroom-set-bottom-divider-width))
  :config
  (general-add-advice 'text-scale-adjust :after
                      #'visual-fill-column-adjust)
  :general
  (:prefix-map '+toggle-map "w" 'writeroom-mode))

;; TRAMP Support
;; Remote file support.
(use-package tramp
  :custom
  (tramp-default-method "sshx"))

;; Restart emacs
; hacky replacement for the restart-emacs package that works with a nix-based config
(defun +restart-emacs ()
  (interactive)
  (save-some-buffers)
  (shell-command (concat "kill -SIGKILL " (number-to-string (emacs-pid)) "; setsid -f emacs")))

(general-def :prefix-map '+quit-restart-map
  "r" #'+restart-emacs)

;; Calc mode
;; set calc mode to start in algebraic (ie normal) mode
(use-package calc
  :hook (calc-mode . calc-algebraic-mode)
  :general
  (:prefix-map '+open-map
   "c" #'calc-dispatch))

(provide '+utils)

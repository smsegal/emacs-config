;; -*- lexical-binding: t; -*-

;;; Writeroom Mode
;; Enable a nicer writing environment
(use-package writeroom-mode
  :custom
  (writeroom-global-effects '(writeroom-set-bottom-divider-width))
  :config
  (general-add-advice 'text-scale-adjust :after
                      #'visual-fill-column-adjust)
  :general
  (:prefix-map '+toggle-map "w" 'writeroom-mode))

;; TRAMP Support
;; Remote file support.
(use-package tramp
  :straight (:type built-in)
  :custom
  (tramp-default-method "sshx"))

;; Restart emacs
(use-package restart-emacs
  :general
  (:prefix-map '+quit-restart-map "r" 'restart-emacs))

;; Calc mode
;; set calc mode to start in algebraic (ie normal) mode
(use-package calc
  :straight (:type built-in)
  :hook (calc-mode . calc-algebraic-mode)
  :general
  (:prefix-map '+open-map
               "c" #'calc-dispatch))

(provide '+utils)

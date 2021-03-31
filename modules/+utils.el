;; -*- lexical-binding: t; -*-

;;; Writeroom Mode
;; Enable a nicer writing environment
(use-package writeroom-mode
  :config
  (setq writeroom-global-effects nil)
  (setq writeroom-maximize-window nil)
  (general-add-advice 'text-scale-adjust :after
                      #'visual-fill-column-adjust)
  (defvar +writeroom--line-num-was-enabled -1)
  (defun +writeroom--maybe-disable-line-numbers ()
    (setq-local +writeroom--line-num-was-enabled display-line-numbers-mode)
    (when display-line-numbers-mode
      (display-line-numbers-mode -1)))
  (general-add-hook 'writeroom-mode-enable-hook #'+writeroom--maybe-disable-line-numbers)
  (general-add-hook 'writeroom-mode-disable-hook (lambda ()
                                                   (when +writeroom--line-num-was-enabled
                                                     (display-line-numbers-mode +1))))
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
  :hook (calc-mode . calc-algebraic-mode)
  :general
  (:prefix-map '+open-map
   "c" #'calc-dispatch))

(provide '+utils)

;; -*- lexical-binding: t; -*-

;; Text Manipulations
;; Rotating text lets you toggle things under point where that makes sense.
;; Subword mode lets you navigate camelCase words etc.
;; We also want to clean up whitespace in prog-mode.
(use-package rotate-text
  :straight (:host github :repo "debug-ito/rotate-text.el")
  :config
  (add-to-list 'rotate-text-words '("true" "false"))
  (add-to-list 'rotate-text-symbols '("+" "-"))
  :general
  (general-nmap
    "]r" #'rotate-text
    "[r" #'rotate-text-backward))

(use-package subword
  :hook (prog-mode . subword-mode)
  :general
  (:prefix-map '+toggle-map
               "s" #'subword-mode))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; Set up automatic pairing of {(<>)}
(use-package electric-pair
  :straight (:type built-in)
  :hook (emacs-startup . electric-pair-mode))

;; Match the indentation of wrapped lines
(use-package adaptive-wrap
  :general
  (:prefix-map '+toggle-map
               "a" #'adaptive-wrap-prefix-mode))


(provide '+text-tools)

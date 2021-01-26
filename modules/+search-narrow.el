;; -*- lexical-binding: t; -*-

;; Searching / Narrowing
;; deadgrep lets us search the specified director with ripgrep. Provides a good UI
(use-package deadgrep
  :general
  (:prefix-map '+search-map
               "d" #'deadgrep))

;; narrow-to-region etc is defined in builtin package page
(use-package page
  ;; :straight (:type built-in)
  :init
  (put 'narrow-to-page 'disabled nil)
  :general
  (:prefix-map '+narrow/notes-map
               "n" #'narrow-to-region
               "p" #'narrow-to-page
               "d" #'narrow-to-defun
               "w" #'widen))

(provide '+search-narrow)

;; -*- lexical-binding: t; -*-

;; Searching / Narrowing

;; narrow-to-region etc is defined in builtin package page
(use-package page
  :straight (:type built-in)
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  :general
  (:prefix-map '+narrow/notes-map
   "n" #'narrow-to-region
   "p" #'narrow-to-page
   "d" #'narrow-to-defun
   "w" #'widen))

(provide '+search-narrow)

;; -*- lexical-binding: t; -*-

(require '+autocomplete)

(use-package eglot
  :ghook
  ('python-mode-hook 'eglot-ensure)
  :config
  (+leader-def
    :keymap 'eglot-mode-map
    "cr" #'eglot-rename
    "ca" #'eglot-code-actions))

(use-package consult-eglot
  :general
  (:keymap 'eglot-mode-map [remap xref-find-apropos] #'consult-eglot-symbols))

(provide '+eglot)

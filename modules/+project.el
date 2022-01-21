;; -*- lexical-binding: t; -*-

;; Project.el
;; This is built in and simpler than projectile.
(use-package project
  :config
  (setq project-vc-merge-submodules nil)
  (setq project-switch-commands
        '((?f "File" project-find-file)
          (?g "Grep" project-find-regexp)
          (?d "Dired" project-dired)
          (?b "Buffer" project-switch-to-buffer)
          (?m "Magit" magit-status)))
  :general
  (+leader-def
    "p" '(:keymap project-prefix-map
          :package project
          :which-key "projects"))
  (general-def :prefix-map 'project-prefix-map
    "s" #'consult-git-grep))

(provide '+project)

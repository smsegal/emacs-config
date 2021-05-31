;; -*- lexical-binding: t; -*-

(use-package treemacs
  :defer t
  :commands (treemacs-git-mode)
  :preface
  (defun +treemacs/toggle ()
    "Initialize or toggle treemacs.

Ensures that only the current project is present and all other projects have
been removed.

Use `treemacs' command for old functionality."
    (interactive)
    (require 'treemacs)
    (pcase (treemacs-current-visibility)
      (`visible (delete-window (treemacs-get-local-window)))
      (_ (if (project-current)
             (treemacs-add-and-display-current-project)
           (treemacs)))))
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window nil
        treemacs-sorting 'alphabetic-case-insensitive-asc)
  (treemacs-git-mode 'deferred)
  :general
  (:prefix-map '+open-map
   "p" #'+treemacs/toggle))

(use-package treemacs-all-the-icons
  :after (treemacs)
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-evil
  :after (treemacs evil)
  :general
  (:prefix-map 'evil-treemacs-state-map
   "SPC" 'leader))

(use-package lsp-treemacs
  :after (treemacs treemacs-all-the-icons lsp)
  :commands (lsp-treemacs-errors-list
             lsp-treemacs-symbols)
  :init
  (lsp-treemacs-sync-mode +1)
  :general
  (:prefix-map '+code-map
   "X" #'lsp-treemacs-errors-list))

(use-package treemacs-magit
  :after (treemacs magit))

(provide '+filetree)

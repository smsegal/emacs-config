;; -*- lexical-binding: t; -*-

;; VC / Git
;; Magit is probably the single best emacs package.
;; We also use the build-int VC mode for some things like ediff.

(use-package vc
  :straight (:type built-in)
  :custom
  (vc-command-messages t)
  (vc-follow-symlinks t)
  ;; don't make an extra frame for the ediff control panel
  ;; (doesn't work well in tiling wms)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;; Magit Itself
;; We need to fix a missing binding that should be set by ~evil-collection~.
;; Submodules get opened by ~"~ inside the magit status buffer.
(use-package magit
  :after evil-collection
  :custom
  (magit-diff-refine-hunk t)
  :preface
  (defun +magit/fix-submodule-binding ()
    ;; evil-magit seems to be overriding or setting this wrong
    ;; somehow, so fix it here
    (transient-append-suffix 'magit-dispatch "\""
      '("'" "Submodules" magit-submodule)))
  :gfhook ('magit-mode-hook #'(+magit/fix-submodule-binding
                               visual-line-mode))
  :config
  (transient-bind-q-to-quit)
  (define-advice magit-list-refs (:around (orig &optional namespaces format sortby)
                                          prescient-sort)
    "Apply prescient sorting when listing refs."
    (let ((res (funcall orig namespaces format sortby)))
      (if (or sortby
              magit-list-refs-sortby
              (not selectrum-should-sort-p))
          res
        (prescient-sort res))))
  :general
  (:prefix-map '+vc-map
               "g" #'magit-status
               "C" #'magit-clone)
  (general-nmap
    :keymaps 'magit-section-mode-map
    "TAB" #'magit-section-toggle
    "j" #'magit-section-forward
    "k" #'magit-section-backward)
  (+local-leader-def
    :keymaps 'with-editor-mode-map
    "," 'with-editor-finish
    "k" 'with-editor-cancel))

;; C dynamic module bindings for speeding up magit
(use-package libgit
  :disabled
  :straight (:host github :repo "magit/libegit2"))

;; Magit Extras
;; Forge lets us access PR's and other collaborative git features from
;; inside Magit.  We also set up todo's to be shown from the codebase all
;; centralized inside the status buffer. It's kinda slow so disabled for
;; now.

(use-package forge
  :after magit)

(use-package magit-todos
  :disabled
  :after magit
  :config (magit-todos-mode))

(use-package git-gutter
  :config (global-git-gutter-mode +1))

;; TODO: needs evil keybindings
(use-package git-timemachine
  :commands git-timemachine)

(provide '+vc)

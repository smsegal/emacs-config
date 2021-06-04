;; -*- lexical-binding: t; -*-

;; VC / Git
;; Magit is probably the single best emacs package.
;; We also use the build-int VC mode for some things like ediff.

(use-package vc
  :straight (:type built-in)
  :init
  (setq vc-command-messages t)
  (setq vc-follow-symlinks t)
  ;; don't make an extra frame for the ediff control panel
  ;; (doesn't work well in tiling wms)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; Magit Itself
;; We need to fix a missing binding that should be set by ~evil-collection~.
;; Submodules get opened by ~"~ inside the magit status buffer.
(use-package magit
  :after (evil-collection)
  :preface
  (defun +magit/fix-submodule-binding ()
    ;; evil-magit seems to be overriding or setting this wrong
    ;; somehow, so fix it here
    (transient-append-suffix 'magit-dispatch "\""
      '("'" "Submodules" magit-submodule)))
  :gfhook ('magit-mode-hook #'(+magit/fix-submodule-binding visual-line-mode))
  :init
  (setq magit-diff-refine-hunk t)
  :config
  (transient-bind-q-to-quit)
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
    "," #'with-editor-finish
    "k" #'with-editor-cancel))

;; Magit Extras
;; Forge lets us access PR's and other collaborative git features from
;; inside Magit.
(use-package forge
  :after magit)

;; We also set up todo's to be shown from the codebase all
;; centralized inside the status buffer. It's kinda slow so disabled for
;; now.
(use-package magit-todos
  :disabled
  :after magit
  :config (magit-todos-mode))

(use-package diff-hl
  :commands (diff-hl-flydiff-mode)
  :init
  (setq fringe-mode '(1 . 1))
  (setq fringes-outside-margins t)
  (let* ((height (frame-char-height))
         (width 1)
         (ones (1- (expt 2 width)))
         (bits (make-vector height ones)))
    (define-fringe-bitmap 'my-diff-hl-bitmap bits height width))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) 'my-diff-hl-bitmap))
  (diff-hl-flydiff-mode +1)
  (global-diff-hl-mode +1))

;; TODO: needs evil keybindings
(use-package git-timemachine
  :commands git-timemachine)

(provide '+vc)

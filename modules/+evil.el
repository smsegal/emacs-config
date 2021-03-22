;; -*- lexical-binding: t; -*-

(require 'evil)

;; ~evil-collection~ provides evil keybindings for almost every package.
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-outline-bind-tab-p t)
  :config
  (evil-collection-init))

;; I also set ~evil-escape~ to really exit things with ~C-g~

(use-package evil-escape
  :custom
  (evil-escape-delay 0.1)
  (evil-escape-key-sequence "fd")
  :init
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  (evil-escape-mode +1))

;; evil keybindings
(general-nvmap
  "gD" #'xref-find-references
  "gd" #'xref-find-definitions)

;;;###autoload
(evil-define-operator +evil:apply-macro (beg end)
  "Apply macro to each line."
  :move-point nil
  (interactive "<r>")
  (let ((register (or evil-this-register (read-char)))
        macro)
    (cond ((or (and (eq register ?@) (eq evil-last-register ?:))
               (eq register ?:))
           (setq macro (lambda () (evil-ex-repeat nil))
                 evil-last-register ?:))
          ((eq register ?@)
           (unless evil-last-register
             (user-error "No previously executed keyboard macro."))
           (setq macro (evil-get-register evil-last-register t)))
          ((setq macro (evil-get-register register t)
                 evil-last-register register)))
    (unless macro
      (user-error "No macro recorded in %c register" register))
    (evil-change-state 'normal)
    (evil-with-single-undo
      (let ((lines (count-lines beg end)))
        (message "Applied macro in %c register %d times" register lines)
        (apply-macro-to-region-lines beg end macro)
        (message "Applied macro in %c register %d times...DONE" register lines)))))

(evil-ex-define-cmd "@" #'+evil:apply-macro)
(general-vmap "@" #'+evil:apply-macro)
(general-mmap "g@" #'+evil:apply-macro)

;; Surround text objects with different delimiters
(use-package evil-surround
  :config
  (global-evil-surround-mode +1))
(use-package evil-embrace
  :after evil-surround
  :commands (evil-embrace-enable-evil-surround-integration)
  :init (evil-embrace-enable-evil-surround-integration))

;; Search forwards with ~S~, ~f~, ~t~
(use-package evil-snipe
  :after evil
  :custom (evil-snipe-use-vim-sneak-bindings t)
  :config
  (push 'magit-mode evil-snipe-disabled-modes)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

;; Search for the text objext under the point with ~*~.
(use-package evil-visualstar
  :config (global-evil-visualstar-mode))

;; Allows you to {un}comment any objects with ~gc~.
;; Move around with ~gs{motion}~.

(use-package evil-nerd-commenter
  :commands evilnc-comment-operator
  :general
  (general-nvmap "gc" 'evilnc-comment-operator))

(use-package evil-easymotion
  :general
  (general-nmap
    "gs" '(:keymap evilem-map
           :which-key "easymotion")))

;; Align text objects on specified char.

(use-package evil-lion
  :general
  (general-nvmap
    "gl" 'evil-lion-left
    "gL" 'evil-lion-right))

;; Some visual cues for yanking and deleting objects.

(use-package evil-goggles
  :demand t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; Swap objects with ~gx~, then another ~gx~ to select the target.

(use-package evil-exchange
  :config (evil-exchange-install))

;; Code Folding

(use-package vimish-fold :after evil)
(use-package evil-vimish-fold
  :after vimish-fold
  :custom
  (evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
  :hook (after-init . global-evil-vimish-fold-mode))


(provide '+evil)

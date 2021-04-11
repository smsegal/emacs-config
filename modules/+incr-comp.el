;; -*- lexical-binding: t; -*-

(require '+project)

;; Incremental Completion
;; We're using Selectrum and the associated ecosystem. They have a bigger
;; focus on creating and extending basic APIs vs alternative like Ivy.

(use-package selectrum
  :disabled
  :commands (selectrum-next-candidate selectrum-previous-candidate)
  :hook (emacs-startup . selectrum-mode)
  :config
  (setq selectrum-fix-vertical-window-height t)
  :general
  (general-imap "C-k" nil)
  (:keymaps 'selectrum-minibuffer-map
   "C-j" 'selectrum-next-candidate
   "C-k" 'selectrum-previous-candidate))

(use-package vertico
  :hook (after-init . vertico-mode)
  :general
  (general-imap "C-k" nil)
  (:keymaps 'vertico-map
   "C-j" 'vertico-next
   "C-k" 'vertico-previous)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Grow and shrink minibuffer
  ;; (setq resize-mini-windows t)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(general-after-init (savehist-mode +1))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))


;; (use-package orderless
;;   :init
;;   ;; completion style should be set when not using selectrum
;;   (setq completion-styles '(orderless))
;;   (setq selectrum-refine-candidates-function #'orderless-filter
;;         selectrum-highlight-candidates-function #'orderless-highlight-matches)
;;   (setq orderless-skip-highlighting (lambda () selectrum-is-active))
;;   (advice-add #'completion--category-override :filter-return
;;               (defun completion-in-region-style-setup+ (res)
;;                 "Fallback to default styles for region completions with orderless."
;;                 (or res
;;                     ;; Don't use orderless for initial candidate gathering.
;;                     (and completion-in-region-mode-predicate
;;                          (not (minibufferp))
;;                          (equal '(orderless) completion-styles)
;;                          '(basic partial-completion emacs22))))))

;; Consult is to selectrum as counsel is to Ivy.
;; Marginalia is a bit of extra eye-candy on top of Consult.
(use-package consult
  :preface
  (defun +consult-fdfind (&optional dir)
    (interactive "P")
    (let ((consult-find-command '("fd" "--color=never" "--full-path")))
      (consult-find dir)))
  :init
  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur)
  (fset 'goto-line #'consult-goto-line)
  :config
  ;; Configure register preview function.
  ;; This gives a consistent display for both `consult-register' and
  ;; the register preview when editing registers.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-preview)

  ;; Optionally tweak the register preview window.
  ;; This adds zebra stripes, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;;project root
  ;; (setq consult-project-root-function
  ;;       (lambda () (locate-dominating-file "." ".git")))
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  :general
  (:prefix-map 'help-map
   "a" #'consult-apropos
   ;; t is usually the tutorial, but this emacs is so customized it's useless
   "t" 'consult-theme)
  (:prefix-map '+insert-map
   "y" #'consult-yank)
  (:prefix-map '+file-map
   "w" #'consult-file-externally
   "r" #'consult-recent-file)
  (:prefix-map '+buffer-map
   "b" #'consult-buffer)
  (:prefix-map '+search-map
   "i" #'consult-imenu
   "s" #'consult-line
   "r" #'consult-ripgrep
   "f" #'+consult-fdfind
   "o" #'consult-outline))

(use-package consult-flycheck
  :general
  (:prefix-map '+code-map
   "x" #'consult-flycheck))

(use-package embark
  :config
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)
  :general
  ("C-S-a" #'embark-act))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package marginalia
  :commands (marginalia-mode)
  :init
  (marginalia-mode)

  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))

  ;; use heavy annotators (keybindings, descriptions etc.)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(provide '+incr-comp)
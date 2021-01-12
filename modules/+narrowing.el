;; -*- lexical-binding: t; -*-

(require '+project)

;; Incremental Completion
;; We're using Selectrum and the associated ecosystem. They have a bigger
;; focus on creating and extending basic APIs vs alternative like Ivy.

(use-package selectrum
  :commands (selectrum-next-candidate selectrum-previous-candidate)
  :hook
  (emacs-startup . selectrum-mode)
  :general
  (general-imap "C-k" nil)
  (:keymaps 'selectrum-minibuffer-map
            "C-j" 'selectrum-next-candidate
            "C-k" 'selectrum-previous-candidate))

;; ;; Prescient is a sorting/filtering package that orders results by "frecency".
;; (use-package prescient
;;   :hook (after-init . prescient-persist-mode))

;; (use-package selectrum-prescient
;;   :hook (selectrum-mode . selectrum-prescient-mode))

(use-package orderless
  ;; :disabled
  :custom (completion-styles '(orderless))
  :init
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (advice-add #'completion--category-override :filter-return
              (defun completion-in-region-style-setup+ (res)
                "Fallback to default styles for region completions with orderless."
                (or res
                    ;; Don't use orderless for initial candidate gathering.
                    (and completion-in-region-mode-predicate
                         (not (minibufferp))
                         (equal '(orderless) completion-styles)
                         '(basic partial-completion emacs22))))))

;; Consult is to selectrum as counsel is to Ivy.
;; Marginalia is a bit of extra eye-candy on top of Consult.
(use-package consult
  :straight (:host github :repo "minad/consult")
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
  (setq consult-project-root-function #'+get-project-root)
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
               "o" #'consult-outline)
  (:prefix-map '+code-map
               "x" #'consult-flycheck))

(use-package consult-selectrum)
(use-package consult-flycheck)

(use-package marginalia
  :straight (:host github :repo "minad/marginalia" :branch "main")
  :init
  (marginalia-mode)

  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(provide '+narrowing)

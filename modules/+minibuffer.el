;; -*- lexical-binding: t; -*-

(require '+project)

;; Incremental Completion
;; We're using vertico and the associated ecosystem. They have a bigger
;; focus on creating and extending basic APIs vs alternative like Ivy.

(use-package vertico
  :hook (after-init . vertico-mode)
  :general
  (:keymaps 'minibuffer-local-map
   "C-j" 'next-line
   "C-k" 'previous-line))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package consult
  :init
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any))

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
   "o" #'consult-outline))

(use-package consult-flycheck
  :general
  (:prefix-map '+code-map
   "x" #'consult-flycheck))

(use-package affe
  :after orderless
  :general
  (:prefix-map '+search-map
   "g" #'affe-grep
   "f" #'affe-find)
  :config
  (defun affe-orderless-regexp-compiler (input _type)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)

  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(use-package embark
  :general
  (:states '(normal insert)
   :keymaps 'global
   "C-." #'embark-act
   "C-;" #'embark-dwim)
  (:keymaps 'help-map
   "B" #'embark-bindings)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
		 (window-parameters (mode-line-format . none))))
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymape
		(which-key--hide-popup-ignore-command)
		(which-key--show-keymap
		 (if (eq (caar targets) 'embark-become)
		     "Become"
		   (format "Act on %s '%s'%s"
			   (plist-get (car targets) :type)
			   (embark--truncate-target (plist-get (car targets) :target))
			   (if (cdr targets) "…" "")))
		 (if prefix
		     (pcase (lookup-key keymap prefix 'accept-default)
		       ((and (pred keymapp) km) km)
		       (_ (key-binding prefix 'accept-default)))
		   keymap)
		 nil nil t)))))

  (setq embark-indicators
	'(embark-which-key-indicator
	  embark-highlight-indicator
	  embark-isearch-highlight-indicator)))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package emacs
  :straight nil
  :init
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (when (>= emacs-major-version 28)
    (setq read-extended-command-predicate #'command-completion-default-include-p))

  (setq enable-recursive-minibuffers t))

(use-package marginalia
  :init (marginalia-mode))

(provide '+minibuffer)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; macos needs a few different tweaks
(defvar IS-MAC (eq system-type 'darwin))

;; garbage collection hacks
(use-package gcmh
  :diminish gcmh-mode
  :init (gcmh-mode 1))

;; Personal Information
(setq user-full-name "Shane Segal"
      user-mail-address "shane@smsegal.ca")

;; load custom settings from a seperate file instead of polluting this file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

;; set font
;; macos needs a larger font due to hidpi
(set-face-attribute 'default nil
		    :family "Cascadia Code"
		    :height (if IS-MAC
				140
			      120))
 
;; EVIL Mode (Can't do the emacs keybindings, hurts my pinkies
(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-respect-visual-line-mode t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-escape
  :custom
  (evil-escape-delay 0.1)
  (evil-escape-key-sequence "fd")
  :hook (after-init . evil-escape-mode))

;; general keybindings
(use-package general
  :commands general-define-key general-def general-swap-key general-create-definer
  :init (general-evil-setup)
  )

;; leader key setup
(general-create-definer +leader-def
  :prefix "SPC"
  :states '(normal visual))

(general-def :prefix-map '+file-map
  "f" 'find-file
  "s" 'save-buffer
  "r" 'recentf-open-files+)

(general-def :prefix-map '+code-map
  "e" 'eval-buffer)

(general-def :prefix-map '+quit-restart-map
  "q" 'kill-emacs
  "r" 'restart-emacs)

(general-def :prefix-map '+buffer-map
  "b" 'switch-to-buffer)

(+leader-def
  "SPC" '(execute-extended-command :which-key "M-x")
  "w" '(:keymap evil-window-map :which-key "windows")
  "b" '(:keymap +buffer-map :which-key "buffers")
  "q" '(:keymap +quit-restart-map :which-key "quit/restart")
  "c" '(:keymap +code-map :which-key "code")
  "f" '(:keymap +file-map :which-key "files")
  "h" '(:keymap help-map :which-key "help"))

;; incremental narrowing a la ivy
(use-package selectrum
  :commands selectrum-next-candidate selectrum-previous-candidate
  :init
  (general-def :states 'insert
    "C-k" nil)
  (selectrum-mode +1)
  :general
  (:keymaps 'selectrum-minibuffer-map
	    "C-j" 'selectrum-next-candidate
	    "C-k" 'selectrum-previous-canidate))

(use-package prescient)
(use-package selectrum-prescient
  :after prescient
  :init
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))
 
;; buffers
(defalias 'list-buffers 'ibuffer-other-window)

;; what the hell do i press next?
(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode +1))

;; UI Tweaks
(setq confirm-kill-emacs 'y-or-n-p
      confirm-nonexistent-file-or-buffer nil
      mouse-yank-at-point t
      ;; fringes
      indicate-buffer-boundaries nil
      indicate-empty-lines nil

      ;; window resizing
      window-resize-pixelwise t
      frame-resize-pixelwise t)

(unless (assq 'menu-bar-lines default-frame-alist)
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars)))

;; ui cruft
(setq menu-bar-mode   nil
      tool-bar-mode   nil
      scroll-bar-mode nil)

(winner-mode +1)

(use-package diminish)

;; smart modeline
(use-package smart-mode-line
  :custom (sml/theme 'respectful)
  :init (sml/setup))

;; (show-paren-mode 1)
(use-package highlight-parentheses
  :hook ((prog-mode LaTex-mode) . highlight-parentheses-mode))

(use-package hl-line
  :straight nil
  :hook ((prog-mode text-mode conf-mode special-mode) . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

;; themes
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-zenburn)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package fira-code-mode
  :custom
  (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x" "www" ":", "x"))
  (fira-code-mode-enable-hex-literal nil)
  :hook prog-mode)

;; line numbers?
(use-package display-line-numbers
  :straight nil
  :init (setq display-line-numbers-type 'relative)
  :hook (prog-mode . display-line-numbers-mode))

;; scrolling
(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

;; visual fill column
(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t)
  :commands visual-fill-column-mode)

;;autocomplete
(use-package company
  :commands company-complete-common company-manual-begin company-grab-line
  :custom
  (company-idle-delay 0.0)
  :hook (after-init . global-company-mode)
  :config (company-tng-mode +1))

;; syntax highlighting
(use-package flycheck
  :diminish flycheck-mode
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook (after-init . global-flycheck-mode))

;; lsp-mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook (((python-mode TeX-mode LaTeX-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred)
(use-package lsp-ui :commands lsp-ui-mode)

;; utilities
(use-package restart-emacs
  :bind ("C-x R" . restart-emacs))

(defun recentf-open-files+ ()
    "Use `completing-read' to open a recent file."
    (interactive)
    (let ((files (mapcar 'abbreviate-file-name recentf-list)))
      (find-file (completing-read "Find recent file: " files nil t))))

(use-package recentf-mode
  :straight nil
  :hook (after-init . recentf-mode)
  :config
  (run-at-time nil (* 5 60) 'recentf-save-list)
  :bind ("C-x C-r" . 'recentf-open-files+))

;; latex

(use-package tex-site
  :straight auctex
  :hook (TeX-mode . visual-line-mode)
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  (setq-default TeX-master t)
  (setq TeX-parse-self t ; parse on load
	TeX-auto-save t  ; parse on save
	;; automatically insert braces after sub/superscript in math mode
	TeX-electric-sub-and-superscript t
	bibtex-dialect 'biblatex
	bibtex-align-at-equal-sign t
	bibtex-text-indentation 20))

(use-package auctex-latexmk
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :init
  (auctex-latexmk-setup))

(use-package reftex
  :straight nil
  :hook ((TeX-mode . reftex-mode)
	 (LaTeX-mode . reftex-mode))
  :init
  (setq reftex-cite-format
        '((?a . "\\autocite[]{%l}")
          (?b . "\\blockcquote[]{%l}{}")
          (?c . "\\cite[]{%l}")
          (?f . "\\footcite[]{%l}")
          (?n . "\\nocite{%l}")
          (?p . "\\parencite[]{%l}")
          (?s . "\\smartcite[]{%l}")
          (?t . "\\textcite[]{%l}"))
        reftex-plug-into-AUCTeX t
        reftex-toc-split-windows-fraction 0.3))

(use-package company-auctex
  :init (company-auctex-init))
(use-package company-reftex)
(use-package company-math)
(use-package company-bibtex)

(defun my-latex-mode-setup ()
  (setq-local company-backends
	      (append '(company-reftex-citations
			company-reftex-labels
			company-math-symbols-latex
			company-math-symbols-unicode
			company-latex-commands)
	      company-backends)))

(add-hook 'LaTeX-mode-hook 'my-latex-mode-setup)

;; vc-mode tweaks
(setq vc-follow-symlinks t)

(use-package magit
  :bind ("C-c C-g" . magit-status))

(use-package evil-magit
  :after magit)

;; projectile 
(use-package projectile
  :general
  (+leader-def
    "p" '(:keymap projectile-command-map :package projectile :which-key "projects")))

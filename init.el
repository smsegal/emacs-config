;;; init.el -*- lexical-binding: t; -*-

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
(setq straight-use-package-by-default t
      straight-fix-flycheck t
      straight-check-for-modifications '(watch-files find-when-checking))

;; macos needs a few different tweaks
(defvar IS-MAC (eq system-type 'darwin))

;; garbage collection hacks
(use-package gcmh
  :diminish gcmh-mode
  :init (gcmh-mode 1))

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package recentf-mode
  :straight nil
  :after no-littering
  :hook (after-init . recentf-mode)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (run-at-time nil (* 5 60) 'recentf-save-list))

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
                    :height (if IS-MAC 140 120))

(setq enable-recursive-minibuffers t)

(when IS-MAC
  (mac-auto-operator-composition-mode))

(use-package diminish)

;; (use-package exec-path-from-shell
;;   :when (memq window-system '(mac ns x))
;;   ;; :custom (exec-path-from-shell-arguments '("-l"))
;;   :config
;;   (setq exec-path-from-shell-variables '("PATH"))
;;   (exec-path-from-shell-initialize))

;; EVIL Mode (Can't do the emacs keybindings, hurts my pinkies
(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-respect-visual-line-mode t)
  :config
  (evil-mode 1))

(use-package undo-tree
  :custom (evil-undo-system 'undo-tree)
  :config (global-undo-tree-mode +1))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-escape
  :diminish evil-escape-mode
  :custom
  (evil-escape-delay 0.1)
  (evil-escape-key-sequence "fd")
  :hook (after-init . evil-escape-mode))

(use-package smartparens
  :hook (after-init . smartparens-global-mode))
(use-package smartparens-config
  :straight nil)

;; general keybindings
(use-package general
  :commands general-define-key general-def general-swap-key general-create-definer
  :custom
  (general-override-states '(insert
                             emacs
                             hybrid
                             normal
                             visual
                             motion
                             operator
                             replace))
  :init (general-evil-setup))

;; leader key setup
(general-create-definer +leader-def
  :prefix "SPC"
  :keymaps 'override
  :states '(normal visual))

;; local leader
(general-create-definer +local-leader-def
  :prefix ","
  :keymaps 'override
  :states '(normal visual))


(general-def :prefix-map '+file-map
  "f" #'find-file
  "s" #'save-buffer)

(general-def :prefix-map '+code-map
  "e" 'eval-buffer)

(general-def :prefix-map '+quit-restart-map
  "q" 'save-buffers-kill-emacs
  "r" 'restart-emacs)

(general-def :prefix-map '+buffer-map
  ;; "b" 'switch-to-buffer
  "p" 'previous-buffer
  "n" 'next-buffer
  "r" 'revert-buffer
  "k" 'kill-this-buffer)

(general-def :prefix-map '+vc-map)

(general-def :prefix-map '+insert-map)

(general-def :prefix-map '+open-map
  "-" 'dired-jump)

(general-def :prefix-map '+search-map)

(+leader-def
  "SPC" '(execute-extended-command :which-key "M-x")
  "w" '(:keymap evil-window-map :which-key "windows")
  "b" '(:keymap +buffer-map :which-key "buffers")
  "q" '(:keymap +quit-restart-map :which-key "quit/restart")
  "c" '(:keymap +code-map :which-key "code")
  "g" '(:keymap +vc-map :which-key "vc/git")
  "f" '(:keymap +file-map :which-key "files")
  "i" '(:keymap +insert-map :which-key "insert")
  "o" '(:keymap +open-map :which-key "open")
  "s" '(:keymap +search-map :which-key "search")
  "h" '(:keymap help-map :which-key "help"))

(use-package evil-surround
  :config
  (global-evil-surround-mode +1))
(use-package evil-embrace
  :after evil-surround
  :init (evil-embrace-enable-evil-surround-integration))

(use-package evil-nerd-commenter
  :commands evilnc-comment-operator
  :general
  (general-nvmap "gc" 'evilnc-comment-operator))

(use-package evil-easymotion
  :general
  (general-nmap
    "gs" '(:keymap evilem-map :which-key "easymotion")))

(use-package evil-lion
  :general
  (general-nvmap
    "gl" 'evil-lion-left
    "gL" 'evil-lion-right))

(use-package icomplete-vertical
  :disabled
  :demand t
  :custom
  (completion-styles '(partial-completion substring))
  (completion-category-overrides '((file (styles basic substring))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :config
  (icomplete-mode)
  (icomplete-vertical-mode)
  :general (:keymaps icomplete-minibuffer-map
		     "<down>"  #'icomplete-forward-completions
		     "C-j"     #'icomplete-forward-completions
		     "<up>"    #'icomplete-backward-completions
		     "C-k"     #'icomplete-backward-completions
		     "C-v"     #'icomplete-vertical-toggle))

;; incremental narrowing a la ivy
(use-package selectrum
  :commands selectrum-next-candidate selectrum-previous-candidate
  :init
  (selectrum-mode +1)
  :general
  (general-imap "C-k" nil)
  (:keymaps 'selectrum-minibuffer-map
	    "C-j" 'selectrum-next-candidate
	    "C-k" 'selectrum-previous-candidate))

(use-package prescient)
(use-package selectrum-prescient
  ;; :disabled
  :after selectrum prescient
  :init
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))
(use-package company-prescient
  :hook (company-mode . company-prescient-mode))

(use-package orderless
  :disabled
  :after selectrum
  :custom
  (completion-styles '(orderless))
  :config
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  ;; If you also configure `completion-styles` for orderless you might want to use the
  ;; following advice because orderless isn't well suited for initial gathering of
  ;; candidates by completion in region.
  (advice-add #'completion--category-override :filter-return
              (defun completion-in-region-style-setup+ (res)
		"Fallback to default styles for region completions with orderless."
		(or res
                    ;; Don't use orderless for initial candidate gathering.
                    (and completion-in-region-mode-predicate
			 (not (minibufferp))
			 (equal '(orderless) completion-styles)
			 '(basic partial-completion emacs22))))))

(use-package selectrum-contrib
  :straight nil
  ;; :after selectrum
  :load-path "modules/"
  ;; :config
  ;; (setq selectrum-highlight-candidates-function
  ;; 	#'+selectrum-candidate-highlight-with-icons-function)
  :general
  (:prefix-map '+file-map
	       "r" 'selectrum-recentf)
  (:prefix-map '+search-map
	       "s" 'selectrum-swiper)
  (:prefix-map '+insert-map
	       "y" '(+yank-pop :which-key "insert from kill ring")))

(use-package deadgrep
  :general
  (:prefix-map '+search-map
	       "d" #'deadgrep))

(use-package flyspell
  :straight nil
  :hook ((org-mode-hook
          markdown-mode-hook
          TeX-mode-hook
          rst-mode-hook
          mu4e-compose-mode-hook
          message-mode-hook
          git-commit-mode-hook) . flyspell-mode))
(use-package flyspell-correct
  :custom (flyspell-correct-interface #'flyspell-correct-dummy))

;; crux useful commands
(use-package crux
  :hook (after-init . crux-reopen-as-root-mode)
  :general
  (:prefix-map '+file-map
	       "p" #'crux-find-user-init-file
	       "R" #'crux-rename-file-and-buffer
	       "w" #'crux-open-with))

(use-package super-save
  :custom (super-save-auto-save-when-idle t)
  :config
  (super-save-mode +1))

(use-package +copy-file-name
  :straight nil
  :init
  (defun +copy-file-name-to-clipboard ()
    "Copy the current buffer file name to the clipboard."
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode)
			default-directory
                      (buffer-file-name))))
      (when filename
	(kill-new filename)
	(message "Copied buffer file name '%s' to the clipboard." filename))))
  :general
  (:prefix-map '+file-map
	       "C" '(+copy-file-name-to-clipboard :which-key "copy filename")))

(use-package rotate-text
  :straight (:host github :repo "debug-ito/rotate-text.el")
  :config
  (add-to-list 'rotate-text-words '("true" "false"))
  (add-to-list 'rotate-text-symbols '("+" "-"))
  :general
  (general-nmap
    "]r" #'rotate-text
    "[r" #'rotate-text-backward))

(use-package bufler
  :diminish bufler-mode
  :config (bufler-mode)
  :general
  (:keymaps 'bufler-list-mode-map
	    :states '(normal visual)
	    "RET" #'bufler-list-buffer-switch
	    "q" #'quit-window)
  (:prefix-map '+buffer-map
	       "b" #'bufler-switch-buffer
	       "B" #'bufler-list))

(use-package burly
  :straight (:host github :repo "alphapapa/burly.el"))

;; code formatting
(use-package apheleia
  :disabled
  :straight (:host github :repo "raxod502/apheleia")
  :general
  (:prefix-map '+code-map
	       "f" 'apheleia-format-buffer))

(use-package format-all)
;; :general
;; (:prefix-map '+code-map
;; 	       "f" 'format-all-buffer))

;; buffers
(defalias 'list-buffers 'ibuffer-other-window)

;; what the hell do i press next?
(put 'narrow-to-region 'disabled nil)

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

;; ui cruft
(unless (assq 'menu-bar-lines default-frame-alist)
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars)))

;;window dividers
(setq window-divider-default-places 'bottom-only
      window-divider-default-bottom-width 1)
(window-divider-mode +1)

(set-fringe-style 1)

(setq menu-bar-mode   nil
      tool-bar-mode   nil
      scroll-bar-mode nil)

(use-package winner
  :straight nil
  :hook (after-init . winner-mode)
  :general
  (:prefix-map 'evil-window-map
	       "u" 'winner-undo
	       "r" 'winner-redo))

(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xreg-activate))

;; smart modeline
(use-package smart-mode-line
  :disabled
  :custom (sml/theme 'respectful)
  :init (sml/setup))

(use-package telephone-line
  :config (telephone-line-mode +1))

;; (show-paren-mode 1)
(use-package highlight-parentheses
  :hook ((prog-mode LaTex-mode) . highlight-parentheses-mode))

(use-package hl-line
  :disabled
  :straight nil
  :hook ((prog-mode text-mode conf-mode special-mode) . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

(use-package all-the-icons)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package anzu
  :hook (after-init . global-anzu-mode))
(use-package evil-anzu)

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 3.0)
  :general (:prefix-map 'evil-window-map
			"w" #'ace-window
			"W" #'ace-swap-window))

;; dashboard
(use-package dashboard
  :custom
  (dashboard-set-footer nil)
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook))

;; themes
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-wilmersdorf)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package fira-code-mode
  :diminish fira-code-mode
  :unless IS-MAC
  :custom
  (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x" "www" ":" "+" ">=" "*"))
  (fira-code-mode-enable-hex-literal nil)
  :hook prog-mode)

(use-package evil-goggles
  :hook (after-init . evil-goggles-mode)
  :config (evil-goggles-use-diff-faces))

(use-package evil-exchange
  :config (evil-exchange-install))

(use-package display-line-numbers
  :disabled
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
  :commands visual-fill-column-mode)

;;autocomplete
(use-package company
  :custom
  (company-idle-delay 0.0)
  :hook (after-init . global-company-mode)
  :general (general-imap "C-SPC" 'company-complete))
(use-package company-box
  :hook (company-mode . company-box-mode))
(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

;; syntax highlighting
(use-package flycheck
  :diminish flycheck-mode
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook (after-init . global-flycheck-mode)
  :general
  (:prefix-map '+code-map
	       "x" '(flycheck-list-errors :which-key "show errors")))

;; lsp-mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-l")
  :hook (((python-mode TeX-mode LaTeX-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)))
(use-package lsp-ui :commands lsp-ui-mode)

(defun +format-dwim ()
  (interactive)
  (if (and (fboundp 'lsp-mode) lsp-mode)
      (if (eq evil-state "visual")
	  (lsp-format-region)
	(lsp-format-buffer))
    (format-all-buffer)))

(general-def :prefix-map '+code-map
  "f" #'+format-dwim)

;; python tweaks
(use-package python
  :straight nil
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt -i"))

(use-package pyimport
  :general
  (+local-leader-def
    :keymaps 'python-mode-map
    "i" '(nil :which-key "imports")
    "iu" 'pyimport-remove-unused
    "ii" 'pyimport-insert-missing))

;; this is great for org etc, but for existing notebooks is lacking
(use-package jupyter
  :straight (:no-native-compile t)
  :commands jupyter-connect-repl jupyter-run-repl)

(use-package emacs-ipython-notebook
  :straight ein
  :hook (ein:notebook-mode . evil-normalize-keymaps)
  :custom
  (ein:output-area-inlined-images t)
  (ein:polymode t)
  :commands ein:run ein:login
  :init
  (evil-define-minor-mode-key mode 'ein:notebook-mode
    (kbd "<C-return>") 'ein:worksheet-execute-cell-km
    (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next-km)
  :general
  (:keymaps 'ein:notebook-mode-map
	    "C-j" #'ein:worksheet-goto-next-input-km
	    "C-k" #'ein:worksheet-goto-prev-input-km))

;; utilities
(use-package restart-emacs
  :general
  (:prefix-map '+quit-map "r" 'restart-emacs))

(defun recentf-open-files+ ()
  "Use `completing-read' to open a recent file."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Find recent file: " files nil t))))


(use-package yasnippet
  :hook ((prog-mode text-mode) . yas-global-mode)
  :general (:prefix-map '+insert-map
			"s" 'yas-insert-snippet))
(use-package yasnippet-snippets
  :after yasnippet)
(use-package doom-snippets
  :straight (:host github :repo "hlissner/doom-snippets")
  :after yasnippet)

;; better help buffers
(use-package helpful
  :general
  (:prefix-map 'help-map
	       "f" #'helpful-callable
	       "v" #'helpful-variable
	       "k" #'helpful-key))

;; latex

(use-package company-auctex)
(use-package company-reftex)
(use-package company-math)
(use-package company-bibtex)

(use-package tex-site
  :after smartparens
  :straight auctex
  :hook ((TeX-mode . +latex-setup)
	 (TeX-mode . +latex-smartparens)
	 (TeX-mode . TeX-fold-mode))
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom
  (TeX-master t)
  (TeX-parse-self t) ;; parse on load
  (TeX-auto-save t)  ;; parse on save
  ;; automatically insert braces after sub/superscript in math mode
  (TeX-electric-sub-and-superscript t)
  (bibtex-dialect 'biblatex)
  (bibtex-align-at-equal-sign t)
  (bibtex-text-indentation 20)
  (TeX-auto-fold t)
  :init
  (defun +latex-setup ()
    (turn-on-visual-line-mode)
    (unless word-wrap
      (toggle-word-wrap))

    (TeX-fold-buffer)
    (setq-local visual-fill-column-center-text t
		visual-fill-column-width 100
		company-backends (append '(company-auctex-labels
					   company-auctex-bibs
					   company-auctex-macros
					   company-auctex-symbols
					   company-auctex-environments
					   company-reftex-citations
					   company-reftex-labels
					   company-math-symbols-latex
					   company-math-symbols-unicode
					   company-latex-commands)
					 company-backends)))
  (defun +latex-smartparens ()
    (setq-local  TeX-electric-math (cons "\\(" "\\)")
		 ;; Smartparens for whatever reason treats the
		 ;; insertion of dollar signs and quotes as single characters.
		 sp--special-self-insert-commands (delete `TeX-insert-dollar sp--special-self-insert-commands)
		 sp--special-self-insert-commands (delete `TeX-insert-quote sp--special-self-insert-commands)
		 ;; After selecting a region, we can wrap it in parenthesis or quotes.
		 sp-autowrap-region t)))

(use-package bibtex
  :straight nil
  :hook (bibtex-mode . +bibtex-setup)
  :init
  (defun +bibtex-setup ()
    (turn-on-visual-line-mode)
    (setq-local visual-fill-column-center-text t
		visual-fill-column-width 100)))

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

(use-package pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook (pdf-view-mode . auto-revert-mode)
  :config
  (pdf-tools-install :no-query)
  (setq-default pdf-view-display-size 'fit-page)
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  :general
  (+local-leader-def :keymaps 'pdf-view-mode-map
    "s" 'pdf-view-auto-slice-minor-mode)
  (:keymaps 'pdf-view-mode-map
	    "q" #'kill-current-buffer))

;; vc-mode tweaks
(setq vc-follow-symlinks t)

(use-package magit
  :custom
  (magit-diff-refine-hunk 'all)
  :general
  (:prefix-map '+vc-map
	       "g" 'magit-status)
  (+local-leader-def :keymaps 'with-editor-mode-map
    "," 'with-editor-finish
    "k" 'with-editor-cancel))

(use-package evil-magit
  :after magit)

(use-package magit-todos
  :after magit
  :config (magit-todos-mode))

(use-package git-gutter
  :config (global-git-gutter-mode +1))

;; TODO: needs evil keybindings
(use-package git-timemachine
  :commands git-timemachine)

;; projectile
(use-package projectile
  :custom
  (projectile-completion-system 'default)
  (projectile-auto-discovery t)
  :hook (after-init . projectile-mode)
  :general
  (+leader-def
    "p" '(:keymap projectile-command-map :package projectile :which-key "projects")))

;; Org Mode
(use-package org)
(use-package org-plus-contrib)
(use-package org-superstar
  :custom (org-superstar-special-todo-items t)
  :hook (org-mode . org-superstar-mode))


;; languages + highlighting
(use-package tree-sitter
  :diminish tree-sitter-mode
  :init (global-tree-sitter-mode))
(use-package tree-sitter-langs)
(use-package tree-sitter-hl
  :straight nil
  :after tree-sitter tree-sitter-langs
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package ssh-config-mode
  :config
  (add-to-list 'auto-mode-alist '("~/.ssh/config\\'" . ssh-config-mode)))

;; vterm
(use-package vterm)
(use-package vterm-toggle
  :after vterm
  :general
  (+leader-def
    "'" #'vterm-toggle)
  (:prefix-map '+open-map
	       "t" #'vterm-toggle
	       "T" #'vterm-other-window)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
	       '((lambda (bufname _)
		   (with-current-buffer bufname (equal major-mode 'vterm-mode)))
		 (display-buffer-reuse-window display-buffer-in-direction)
		 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
		 (direction . bottom)
		 (dedicated . t) ;dedicated is supported in emacs27
		 (reusable-frames . visible)
		 (window-height . 0.3))))

;; direnv support
(use-package envrc
  :diminish envrc-mode
  :init (envrc-global-mode +1))

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
      straight-check-for-modifications '(watch-files find-when-checking)

      use-package-alway-defer t)

;; macos needs a few different tweaks
(defvar IS-MAC (eq system-type 'darwin))

;; garbage collection hacks
(use-package gcmh
  :diminish gcmh-mode
  :init (gcmh-mode 1))

(use-package no-littering
  :demand t
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

;; EVIL Mode (Can't do the emacs keybindings, hurts my pinkies)
(use-package undo-fu)
(use-package evil
  :after undo-fu
  :demand t
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-respect-visual-line-mode t)
  (evil-want-Y-yank-to-eol t)
  (evil-cross-lines t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

(use-package undo-tree
  :disabled
  :custom (evil-undo-system 'undo-tree)
  :config (global-undo-tree-mode +1))

(use-package evil-collection
  :after evil
  :demand t
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-escape
  :diminish evil-escape-mode
  :demand t
  :custom
  (evil-escape-delay 0.1)
  (evil-escape-key-sequence "fd")
  :config (evil-escape-mode +1))

(use-package smartparens
  :hook (after-init . smartparens-global-mode))
(use-package smartparens-config
  :straight nil)

;; general keybindings
(use-package general
  :demand t
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
  "e" #'eval-buffer
  "c" #'compile
  "m" #'recompile)

(general-def :prefix-map '+quit-restart-map
  "q" 'save-buffers-kill-emacs
  "r" 'restart-emacs)

(general-def :prefix-map '+buffer-map
  :wk-full-keys nil
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

(general-def :prefix-map '+bookmark-map
  :wk-full-keys nil)

(+leader-def
  "SPC" '(execute-extended-command :which-key "M-x")
  "w" '(:keymap evil-window-map :which-key "windows")
  "b" '(:keymap +buffer-map :which-key "buffers")
  "B" '(:keymap +bookmark-map :which-key "bookmarks")
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

(use-package evil-goggles
  :after evil
  :demand t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-exchange
  :config (evil-exchange-install))

;; code folding
(use-package origami
  :disabled
  :straight (:host github :repo "jcs-elpa/origami.el")
  :hook (after-init . global-origami-mode))

(use-package vimish-fold
  :after evil)

(use-package evil-vimish-fold
  :after vimish-fold
  :custom
  (evil-vimish-fold-mode-lighter " ⮒")
  (evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
  :init (global-evil-vimish-fold-mode +1))

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

(use-package flimenu
  :config (flimenu-global-mode))

(use-package amx
  :after selectrum
  :custom (amx-backend 'selectrum)
  :config (amx-mode))

(use-package deadgrep
  :general
  (:prefix-map '+search-map
	       "d" #'deadgrep))

(use-package flyspell
  :straight nil
  :defer t
  :custom
  (flyspell-issue-welcome-flag nil)
  ;; Significantly speeds up flyspell, which would otherwise print
  ;; messages for every word when checking the entire buffer
  (flyspell-issue-message-flag nil)
  (ispell-program-name "enchant-2") ;; new spellcheck engine
  (ispell-dictionary "en_CA")
  :ghook
  ('(org-mode-hook
     markdown-mode-hook
     TeX-mode-hook
     rst-mode-hook
     mu4e-compose-mode-hook
     message-mode-hook
     git-commit-mode-hook) #'flyspell-mode)
  ('prog-mode-hook #'flyspell-prog-mode))

(use-package flyspell-correct
  :after flyspell
  :commands flyspell-correct-previous
  :general
  ([remap ispell-word] #'flyspell-correct-wrapper)
  (general-nvmap
    "zg" #'+spell/add-word)
  :config
  (defun +spell/add-word (word &optional scope)
    "Add WORD to your personal dictionary, within SCOPE.
SCOPE can be `buffer' or `session' to exclude words only from the current buffer
or session. Otherwise, the addition is permanent."
    (interactive
     (list (progn (require 'flyspell)
                  (car (flyspell-get-word)))
           (cond ((equal current-prefix-arg '(16))
                  'session)
		 ((equal current-prefix-arg '(4))
                  'buffer))))
    (require 'flyspell)
    (cond
     ((null scope)
      (ispell-send-string (concat "*" word "\n"))
      (ispell-send-string "#\n")
      (flyspell-unhighlight-at (point))
      (setq ispell-pdict-modified-p '(t)))
     ((memq scope '(buffer session))
      (ispell-send-string (concat "@" word "\n"))
      (add-to-list 'ispell-buffer-session-localwords word)
      (or ispell-buffer-local-name ; session localwords might conflict
          (setq ispell-buffer-local-name (buffer-name)))
      (flyspell-unhighlight-at (point))
      (if (null ispell-pdict-modified-p)
          (setq ispell-pdict-modified-p
		(list ispell-pdict-modified-p)))
      (if (eq replace 'buffer)
          (ispell-add-per-file-word-list word))))
    (ispell-pdict-save t))
  (require 'flyspell-correct-popup nil t)
  (define-key 'popup-menu-keymap [escape] #'keyboard-quit))

(use-package flyspell-correct-popup
  :after flyspell-correct
  :custom
  (flyspell-correct-interface #'flyspell-correct-popup)
  :general (:keymaps 'popup-menu-keymap [escape] #'keyboard-quit))

(use-package flyspell-lazy
  ;; :disabled
  :after flyspell
  :config
  (setq flyspell-lazy-idle-seconds 1
	flyspell-lazy-window-idle-seconds 3)
  (flyspell-lazy-mode +1))

;; crux useful commands
(use-package crux
  ;; :hook (after-init . crux-reopen-as-root-mode)
  :general
  (:prefix-map '+file-map
	       "E" #'crux-sudo-edit
	       "p" #'crux-find-user-init-file
	       "R" #'crux-rename-file-and-buffer)
  (:prefix-map '+open-map
	       "w" #'crux-open-with))

(use-package super-save
  :custom (super-save-auto-save-when-idle t)
  :hook (after-init . super-save-mode))

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
  :hook (after-init . bufler-mode)
  :general
  (:keymaps 'bufler-list-mode-map
	    :states '(normal visual)
	    "RET" #'bufler-list-buffer-switch
	    "q" #'quit-window)
  (:prefix-map '+buffer-map
	       "b" '(bufler-switch-buffer :which-key "switch buffer")
	       "B" '(bufler-list :which-key "buffer list")))

(use-package burly
  :straight (:host github :repo "alphapapa/burly.el")
  :general
  (:prefix-map '+bookmark-map
	       "l" 'list-bookmarks
	       "w" 'burly-bookmark-windows
	       "F" 'burly-bookmark-frames))

;; code formatting
(use-package apheleia
  :disabled
  :straight (:host github :repo "raxod502/apheleia")
  :general
  (:prefix-map '+code-map
	       "f" 'apheleia-format-buffer))

(use-package format-all
  :general
  (:prefix-map '+code-map
	       "f" 'format-all-buffer))

;; buffers
(defalias 'list-buffers 'ibuffer-other-window)

(put 'narrow-to-region 'disabled nil)

;; what the hell do i press next?
(use-package which-key
  :diminish which-key-mode
  :demand t
  :custom
  (which-key-enable-extended-define-key t)
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

(when (fboundp 'set-fringe-style)
  (set-fringe-style 1))

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

(use-package evil-terminal-cursor-changer
  :straight (:host github :repo "kisaragi-hiu/evil-terminal-cursor-changer")
  :hook (tty-setup . evil-terminal-cursor-changer-activate))

(use-package xterm-mouse-mode
  :straight nil
  :hook (tty-setup . xterm-mouse-mode))

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 3.0)
  :general (:prefix-map 'evil-window-map
			"w" #'ace-window
			"W" #'ace-swap-window))

(use-package switch-to-buffer
  :straight nil
  :init
  (defun +switch-to-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))
  :general
  (:prefix-map '+buffer-map
	       "s" #'+switch-to-scratch))

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
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; (use-package load-theme
;;   :straight nil
;;   :after (doom-themes circadian)
;;   :init
;;   (defvar +active-theme (if (display-graphic-p)
;; 			    'doom-wilmersdorf
;; 			  'doom-old-hope))
;;   (load-theme +active-theme t))

(use-package circadian
  :custom
  (calendar-latitude 43.6)
  (calendar-longitude -79.4)
  (circadian-themes '((:sunrise . doom-acario-light)
                      (:sunset  . doom-wilmersdorf)))
  :hook
  (after-init . circadian-setup))

(use-package fira-code-mode
  :diminish fira-code-mode
  :when (memq window-system '(pgtk x))
  :custom
  (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x" "www" ":" "+" ">=" "*"))
  (fira-code-mode-enable-hex-literal nil)
  :hook prog-mode)

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
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook (after-init . global-flycheck-mode)
  :general
  (:prefix-map '+code-map
	       "x" '(flycheck-list-errors :which-key "show errors")))

;; lsp-mode
;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :custom
;;   (lsp-keymap-prefix "C-l")
;;   :hook (((python-mode TeX-mode LaTeX-mode) . lsp-deferred)
;;          (lsp-mode . lsp-enable-which-key-integration)))
;; (use-package lsp-ui :commands lsp-ui-mode)

(use-package eglot
  :after (company yasnippet)
  :ghook
  ;; ('(python-mode-hook TeX-mode-hook) #'eglot-ensure)
  ('python-mode-hook  #'eglot-ensure)
  :general
  (general-def
    :prefix-map '+code-map
    :predicate '(eglot-managed-p)
    "r" #'eglot-rename)
  ;; "f" #'eglot-format))
  (:keymaps 'eglot-mode-map
	    [remap format-all-buffer] #'eglot-format))

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
  (evil-define-minor-mode-key '(normal visual) 'ein:notebook-mode
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
	       "k" #'helpful-key
	       "h" #'helpful-at-point))

;; latex

(use-package company-auctex)
(use-package company-reftex)
(use-package company-math)
(use-package company-bibtex)

(use-package tex-site
  :after smartparens
  :straight auctex
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
  ;; :gfhook
  ;; #'+latex-setup
  ;; #'+latex-smartparens
  ;; :ghook
  ;; #'Tex-fold-mode
  :hook ((TeX-mode . +latex-setup)
	 (TeX-mode . +latex-smartparens)
	 (TeX-mode . TeX-fold-mode))
  :mode ("\\.tex\\'" . LaTeX-mode)
  :general (:keymaps 'TeX-mode-map
		     [remap compile] #'TeX-command-master
		     [remap recompile] (lambda () (TeX-command-master +1)))
  :init
  (defun +latex-setup ()
    (turn-on-visual-line-mode)
    (unless word-wrap
      (toggle-word-wrap))

    (TeX-fold-buffer)
    (setq-local visual-fill-column-center-text t
		visual-fill-column-width 100

		;; important that reftex comes before auctex otherwise
		;; citation autocomplete doesn't work
		company-backends (append '(company-reftex-citations
					   company-reftex-labels
					   company-auctex-labels
					   company-auctex-bibs
					   company-auctex-macros
					   company-auctex-symbols
					   company-auctex-environments
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
  :gfhook #'+bibtex-setup
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
  (:keymaps 'transient-map
	    "q" #'transient-quit-one)
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
;; (use-package org-plus-contrib)
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

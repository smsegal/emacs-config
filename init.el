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
      straight-check-for-modifications '(watch-files find-when-checking))

;; macos needs a few different tweaks
(defvar IS-MAC (eq system-type 'darwin))

;; garbage collection hacks
(use-package gcmh
  :hook (after-init . gcmh-mode))

(use-package no-littering
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package recentf-mode
  :straight (:type built-in)
  :after no-littering
  :hook (after-init . recentf-mode)
  :custom
  (recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (run-at-time nil (* 5 60) 'recentf-save-list))

;; Personal Information
(setq user-full-name "Shane Segal"
      user-mail-address "shane@smsegal.ca")
(setq auth-sources '("~/.authinfo.gpg"))

;; load custom settings from a seperate file instead of polluting this file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

(setq enable-recursive-minibuffers t)

(when IS-MAC
  (mac-auto-operator-composition-mode))

(use-package emacs
  :straight (:type built-in)
  :config
  (server-start))

;; gnome handles this fine
(use-package exec-path-from-shell
  :disabled
  :when (memq window-system '(mac ns x))
  ;; :custom (exec-path-from-shell-arguments '("-l"))
  :config
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))

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
  (evil-cross-lines nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-undo-system 'undo-fu)
  (evil-regexp-search t)
  (evil-move-cursor-back t)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
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
  :demand t
  :custom
  (evil-escape-delay 0.1)
  (evil-escape-key-sequence "fd")
  :init
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  (add-to-list 'evil-escape-excluded-major-modes 'vterm-mode)
  (evil-escape-mode +1))

;; (use-package smartparens
;;   :hook (after-init . smartparens-global-mode))
;; (use-package smartparens-config :straight nil)
(use-package electric-pair
  :straight (:type built-in)
  :hook (emacs-startup . electric-pair-mode))

;; general keybindings
(use-package general
  :demand t
  :custom
  (general-override-states
   '(insert emacs hybrid normal visual motion operator replace))
  :config
  (general-evil-setup)

  ;; (general-add-advice #'evil-force-normal-state :after #'evil-escape)
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
    "p" 'previous-buffer
    "n" 'next-buffer
    "r" 'revert-buffer
    "k" 'kill-this-buffer)

  (general-def :prefix-map '+vc-map)
  (general-def :prefix-map '+insert-map)
  (general-def :prefix-map '+open-map
    "-" 'dired-jump
    "f" 'make-frame)
  (general-def :prefix-map '+toggle-map)
  (general-def :prefix-map '+search-map)
  (general-def :prefix-map '+bookmark-map
    :wk-full-keys nil)
  (general-def :prefix-map '+narrow/notes-map)

  (+leader-def
    "SPC" '(execute-extended-command :which-key "M-x")
    "u" 'universal-argument
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
    "n" '(:keymap +narrow/notes-map :which-key "narrow/notes")
    "t" '(:keymap +toggle-map :which-key "toggle")
    "h" '(:keymap help-map :which-key "help")))

(use-package +evil-contrib
  :straight nil
  :load-path "modules/"
  :preface
  (defun +evil-clear-search-hl ()
    (evil-ex-nohighlight))
  :config
  (evil-ex-define-cmd "@" #'+evil:apply-macro)
  (general-add-advice 'evil-ret :after '+evil-clear-search-hl)
  :general
  (general-vmap "@" #'+evil:apply-macro)
  (general-mmap "g@" #'+evil:apply-macro)
  (general-nvmap "gD" #'xref-find-references))

(use-package evil-surround
  :config
  (global-evil-surround-mode +1))
(use-package evil-embrace
  :after evil-surround
  :init (evil-embrace-enable-evil-surround-integration))

(use-package evil-snipe
  :after evil
  :custom (evil-snipe-use-vim-sneak-bindings t)
  :config
  (push 'magit-mode evil-snipe-disabled-modes)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-visualstar
  :config (global-evil-visualstar-mode))

(use-package evil-nerd-commenter
  :commands evilnc-comment-operator
  :general
  (general-nvmap "gc" 'evilnc-comment-operator))

(use-package evil-easymotion
  :general
  (general-nmap
    "gs" '(:keymap evilem-map
           :which-key "easymotion")))

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
(use-package vimish-fold :after evil)
(use-package evil-vimish-fold
  :after vimish-fold
  :custom
  (evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
  :hook (after-init . global-evil-vimish-fold-mode))

;; text indentation stuff
(with-eval-after-load 'general
  (general-add-hook (list 'prog-mode-hook 'text-mode-hook)
                    (lambda () (setq-local indent-tabs-mode nil))))

;; incremental narrowing a la ivy
(use-package selectrum
  :commands selectrum-next-candidate selectrum-previous-candidate
  :hook
  (after-init . selectrum-mode)
  :general
  (general-imap "C-k" nil)
  (:keymaps 'selectrum-minibuffer-map
            "C-j" 'selectrum-next-candidate
            "C-k" 'selectrum-previous-candidate))
(use-package prescient
  :hook (selectrum-mode . prescient-persist-mode))
(use-package selectrum-prescient
  :hook (selectrum-mode . selectrum-prescient-mode))
(use-package company-prescient
  :hook (company-mode . company-prescient-mode))
(use-package +selectrum-contrib
  :straight nil
  :load-path "modules/"
  :commands (selectrum-swiper selectrum-recentf)
  :preface
  (defun +recenter-advice ()
    "unclear why this has be in its own function but ::shrug::"
    (recenter))
  :config
  (general-add-advice 'selectrum-swiper :after '+recenter-advice)
  :general
  (:keymaps 'selectrum-minibuffer-map
            "C-s" #'selectrum-restrict-to-matches)
  (:prefix-map '+file-map
               "r" (general-predicate-dispatch 'crux-recentf-find-file
                     (not IS-MAC) 'selectrum-recentf
                     :docstring "find recent file"))
  (:prefix-map '+search-map
               "s" #'selectrum-swiper
               "o" #'selectrum-outline
               "i" #'+selectrum-imenu)
  (:prefix-map '+insert-map
               "y" '(+yank-pop :which-key "insert from kill ring")))

;; narrow-to-region etc is defined in builtin package page
(use-package page
  :straight (:type built-in)
  :general
  (:prefix-map '+narrow/notes-map
               "n" #'narrow-to-region
               "p" #'narrow-to-page
               "d" #'narrow-to-defun
               "w" #'widen))

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
  :preface
  (defun +spell/add-word (word &optional scope)
    "Add WORD to your personal dictionary, within SCOPE.  SCOPE can be
`buffer' or `session' to exclude words only from the current buffer or
session. Otherwise, the addition is permanent."
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
  :general
  ([remap ispell-word] #'flyspell-correct-wrapper)
  (general-nvmap "zg" #'+spell/add-word))

(use-package flyspell-correct-popup
  :disabled
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
  :general
  (:prefix-map '+file-map
               "E" #'crux-sudo-edit
               "D" #'crux-delete-file-and-buffer
               "p" #'crux-find-user-init-file
               "R" #'crux-rename-file-and-buffer)
  (:prefix-map '+open-map
               "w" #'crux-open-with))

(defun +find-init-file-here ()
  (interactive)
  (find-file user-init-file))

(general-def :prefix-map '+file-map
  "P" #'+find-init-file-here)

(use-package super-save
  :custom (super-save-auto-save-when-idle t)
  :hook (after-init . super-save-mode))

(use-package +copy-file-name
  :straight nil
  :preface
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

(use-package subword
  :hook (prog-mode . subword-mode))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;;; vc-mode and Magit
(use-package vc
  :straight nil
  :custom
  (vc-command-messages t)
  (vc-follow-symlinks t)
  ;; don't make an extra frame for the ediff control panel (doesn't
  ;; work well in tiling wms)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package magit
  :custom
  (magit-diff-refine-hunk t)
  :preface
  (defun +magit/fix-submodule-binding ()
    ;; evil-magit seems to be overriding or setting this wrong
    ;; somehow, so fix it here
    (transient-append-suffix 'magit-dispatch "\""
      '("'" "Submodules" magit-submodule)))
  :gfhook ('magit-mode-hook #'+magit/fix-submodule-binding)
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
               "g" 'magit-status)
  (general-nmap
    :keymaps 'magit-section-mode-map
    "TAB" #'magit-section-toggle
    "j" #'magit-section-forward
    "k" #'magit-section-backward)
  (+local-leader-def
    :keymaps 'with-editor-mode-map
    "," 'with-editor-finish
    "k" 'with-editor-cancel))

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

;;; Buffers

(use-package bufler
  ;; :hook (after-init . bufler-mode)
  :commands bufler-ex
  :general
  (general-nvmap
    :keymaps 'bufler-list-mode-map
    "RET" #'bufler-list-buffer-switch
    ;; "TAB" #'bufler-ex
    (kbd "<escape>") #'quit-window
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
(put 'narrow-to-region 'disabled nil)

;;; UI Tweaks

;; what the hell do i press next?
(use-package which-key
  :demand t
  :custom
  (which-key-popup-type 'side-window)
  (which-key-enable-extended-define-key t)
  :hook (after-init . which-key-mode)
  :general
  (:keymaps 'help-map
            "b" #'which-key-show-major-mode
            "B" #'which-key-show-top-level))

;; set this for all prompts
(defalias 'yes-or-no-p 'y-or-n-p)

(setq confirm-nonexistent-file-or-buffer nil
      mouse-yank-at-point t

      ;; make underlines look a little better
      x-underline-at-descent-line t

      ;; window resizing
      window-resize-pixelwise t
      frame-resize-pixelwise t)

;; ui cruft
(unless (assq 'menu-bar-lines default-frame-alist)
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars)))

;; pulse current line on window switch
(use-package beacon
  :disabled
  :hook (after-init . beacon-mode)
  :config
  (add-to-list 'beacon-dont-blink-commands 'vterm-send-return)
  (add-to-list 'beacon-dont-blink-commands 'mwheel-scroll))

(use-package avoid
  :straight (:type built-in)
  :config
  ;; doesn't seem to do any animating, at least on wayland should
  ;; check it out on X (but I never use X soooo)
  (mouse-avoidance-mode 'animate))

;;window dividers
(use-package window-divider
  :straight (:type built-in)
  :custom
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only)
  :hook (after-init . window-divider-mode))

(use-package fringe
  :straight nil
  :init (set-fringe-style 0)
  :custom
  ;; fringes
  (indicate-buffer-boundaries   nil)
  (indicate-empty-lines         nil)
  (fringes-outside-margins      nil)
  (indicate-buffer-boundaries   nil)
  (indicate-empty-lines         nil)
  (overflow-newline-into-fringe t))

(setq menu-bar-mode   nil
      tool-bar-mode   nil
      scroll-bar-mode nil)

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package auto-dim-other-buffers
  :hook (after-init . auto-dim-other-buffers-mode)
  :custom
  (auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
  (auto-dim-other-buffers-dim-on-focus-out nil))

(use-package winner
  :straight (:type built-in)
  :hook (after-init . winner-mode)
  :general
  (:prefix-map 'evil-window-map
               "u" 'winner-undo
               "r" 'winner-redo))

(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xreg-activate))

;;; modeline

;; used for buffer identification in moody modeline
(use-package smart-mode-line)
(use-package minions
  :config (minions-mode 1))
(use-package moody
  :after smart-mode-line
  ;; :preface
  ;; (defvar moody-evil-state-indicator
  ;;   '(:eval (moody-tab (format-mode-line (cond ((eq evil-state 'normal)
  ;;                                            "Normal")
  ;;                                           ((eq evil-state 'insert)
  ;;                                            "Vis")))
  ;;                   20 'up)))
  ;; ;; (put 'moody-evil-state-indicator 'risky-local-variable t)
  ;; (make-variable-buffer-local 'moody-evil-state-indicator)
  ;; (defun +moody-replace-mode-line-evil-state (&optional reverse)
  ;;   (interactive "P")
  ;;   (moody-replace-element 'evil-mode-line-tag
  ;;                       '+moody-replace-mode-line-evil-state
  ;;                       reverse))
  :config
  ;; (+moody-replace-mode-line-evil-state)
  (moody-replace-sml/mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package highlight-parentheses
  :hook ((prog-mode LaTex-mode) . highlight-parentheses-mode))

(use-package hl-line
  :straight (:type built-in)
  :preface
  (defun +highlight-visual-line ()
    (save-excursion
      (cons (progn (beginning-of-visual-line) (+ 1 (point)))
            (progn (beginning-of-visual-line 2) (point)))))
  :hook ((prog-mode text-mode conf-mode special-mode) . hl-line-mode)
  :custom
  (hl-line-range-function '+highlight-visual-line)
  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil))

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
  :straight (:type built-in)
  :hook (tty-setup . xterm-mouse-mode))



(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 3.0)
  :general (:prefix-map 'evil-window-map
                        "w" #'ace-window
                        "W" #'ace-swap-window))

;; window enlargement
(use-package zoom
  :custom
  (zoom-size '(0.7 . 0.7))
  (zoom-ignored-major-modes '(dired-mode vterm-mode help-mode helpful-mode rxt-help-mode help-mode-menu org-mode))
  (zoom-ignored-buffer-names '("*scratch*" "*info*" "*helpful variable: argv*"))
  (zoom-ignored-buffer-name-regexps '("^\\*calc" "\\*helpful variable: .*\\*"))
  (zoom-ignore-predicates (list (lambda () (< (count-lines (point-min) (point-max)) 20))))
  ;; (zoom-size '(0.618 . 0.618))
  ;; (zoom-ignored-major-modes '(vterm-mode
  ;;                             help-mode
  ;;                             helpful-mode
  ;;                             rxt-help-mode
  ;;                             help-mode-menu))
  ;; (zoom-ignored-buffer-names '("*info*"
  ;;                              "*helpful variable: argv*"
  ;;                              "*compilation*"))
  ;; (zoom-ignored-buffer-name-regexps '("^\\*calc" "\\*helpful variable: .*\\*"))
  :general
  (:prefix-map '+toggle-map
               "z" #'zoom-mode))

(use-package +enlarge-window
  :straight nil
  :load-path "modules/"
  :general (:prefix-map 'evil-window-map
                        "o" #'+window-enlargen
                        "O" #'delete-other-windows))

(use-package switch-to-buffer
  :straight (:type built-in)
  :preface
  (defun +switch-to-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))
  :general
  (:keymaps 'global-map
            (kbd "<mouse-9>") 'next-buffer
            (kbd "<mouse-8>") 'previous-buffer)
  (:prefix-map '+buffer-map
               "s" #'+switch-to-scratch))

;; dashboard
(use-package dashboard
  :custom
  (dashboard-set-footer nil)
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-center-content t)
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-set-init-info t)
  (dashboard-startup-banner (concat user-emacs-directory "emacs-bigsur_small.png"))
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

(use-package modus-operandi-theme
  :custom
  (modus-operandi-theme-bold-constructs t)
  (modus-operandi-theme-slanted-constructs t)
  (modus-operandi-theme-syntax 'alt-syntax)
  (modus-operandi-theme-mode-line 'moody))

(use-package modus-vivendi-theme
  :custom
  (modus-vivendi-theme-bold-constructs t)
  (modus-vivendi-theme-slanted-constructs t)
  (modus-vivendi-theme-mode-line 'moody)
  (modus-vivendi-theme-syntax 'faint))

(use-package load-theme
  :disabled
  :straight (:type built-in)
  :after
  (doom-themes modus-operandi-theme modus-vivendi-theme)
  :init
  (defvar +active-theme (if (display-graphic-p)
                            'modus-operandi
                          'doom-old-hope))
  (load-theme +active-theme t))

(use-package circadian
  ;; :disabled
  :after (doom-themes modus-operandi-theme modus-vivendi-theme)
  :custom
  (calendar-latitude 43.6)
  (calendar-longitude -79.4)
  (circadian-themes '((:sunrise . modus-operandi)
                      (:sunset  . modus-vivendi)))
  :hook
  (after-init . circadian-setup))

;;; font
(use-package emacs
  :straight (:type built-in)
  :config
  ;; macos needs a larger font due to hidpi
  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :height (if IS-MAC 140 105))
  (add-to-list 'default-frame-alist '(line-spacing . 0.2))
  ;; italic comments
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic))

;; Note: Doesn't work on emacs28+
(use-package ligature
  :unless IS-MAC
  :straight (:host github :repo "mickeynp/ligature.el")
  :ghook ('after-init-hook #'global-ligature-mode)
  :init
  ;; JetBrains Mono Ligatures
  (cond ((string= (face-attribute 'default :family) "JetBrains Mono")
         (ligature-set-ligatures
          't '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/="
                       "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                       "??" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<>" "<<<"
                       ">>>" "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||="
                       "##" "###" "####" "#{" "#[" "]#" "#(" "#?"  "#_" "#_("
                       "#:" "#!"  "#=" "^=" "<$>" "<$" "$>" "<+>" "<+" "+>"
                       "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--" "-->"
                       "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>"
                       "<==>" "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" ">--"
                       "-<" "-<<" ">->" "<-<" "<-|" "<=|" "|=>" "|->" "<->"
                       "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~" "~@" "[||]"
                       "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                       "|||>" "<|||" "<|>" "..." ".." ".=" ".-" "..<" ".?"
                       "::" ":::" ":=" "::=" ":?"  ":?>" "//" "///" "/*" "*/"
                       "/=" "//=" "/==" "@_" "__")))))

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

      mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((meta)) ((control) . text-scale))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

;; visual fill column
(use-package visual-fill-column
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  :commands visual-fill-column-mode
  :preface
  (defun +toggle-visual-fill-and-line-mode ()
    (interactive)
    (if (and visual-fill-column-mode visual-line-mode)
        (progn
          (visual-fill-column-mode -1)
          (visual-line-mode -1))
      (progn
        (visual-fill-column-mode +1)
        (visual-line-mode +1))))
  :general
  (:prefix-map '+toggle-map
               "v" #'+toggle-visual-fill-and-line-mode))

;;autocomplete
(use-package company
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :hook (emacs-startup . global-company-mode)
  :general
  (general-imap "C-SPC" 'company-complete)
  (:keymaps 'company-search-map
            "C-s" #'company-filter-candidates))
(use-package company-box
  :hook (company-mode . company-box-mode))
(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

;; syntax highlighting
(use-package flycheck
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook (after-init . global-flycheck-mode)
  :general
  (:prefix-map '+code-map
               "x" '(flycheck-list-errors :which-key "show errors")))

;; lsp-mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (read-process-output-max (* 1024 1024)) ;; 1mb
  (lsp-completion-provider :capf)
  (lsp-enable-folding nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-snippet t)
  (lsp-eldoc-enable-hover nil)
  (lsp-headerline-breadcrumb-enable t)
  :ghook
  ('(TeX-mode-hook
     yaml-mode-hook
     sh-mode-hook
     js2-mode-hook)
   #'lsp-deferred)
  ('lsp-mode-hook '(lsp-headerline-breadcrumb-mode
                    lsp-modeline-diagnostics-mode
                    lsp-enable-which-key-integration))
  :general
  (general-nvmap :keymaps 'lsp-mode-map
    "," '(:keymap lsp-command-map))
  (general-def
    :prefix-map '+code-map
    :predicate 'lsp-mode
    "r" #'lsp-rename
    "a" #'lsp-execute-code-action)
  (:keymaps 'lsp-mode-map
            [remap format-all-buffer] #'lsp-format-buffer
            [remap evil-goto-definition] #'lsp-find-definition))
(use-package lsp-ui
  :commands lsp-ui-mode
  :general
  (:keymaps 'lsp-mode-map
            [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
            [remap xref-find-references] #'lsp-ui-peek-find-references)
  (:keymaps 'lsp-ui-peek-mode-map
            "j"   #'lsp-ui-peek--select-next
            "k"   #'lsp-ui-peek--select-prev
            "C-j" #'lsp-ui-peek--select-next
            "C-k" #'lsp-ui-peek--select-prev))

(use-package lsp-pyright
  :preface
  (defun +pyright__enable-lsp ()
    (require 'lsp-pyright)
    (lsp-deferred))
  :hook (python-mode . +pyright__enable-lsp))

(use-package eglot
  :disabled
  :after (company yasnippet)
  :ghook
  ('(python-mode-hook js-mode-hook)  #'eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((js-mode typescript-mode) . ("typescript-language-server" "--stdio")))
  :general
  (general-def
    :prefix-map '+code-map
    :predicate '(eglot-managed-p)
    "r" #'eglot-rename))
;; "f" #'eglot-format))
;; (:keymaps 'eglot-mode-map
;;           [remap format-all-buffer] #'eglot-format))

(use-package +flycheck-eglot
  :disabled
  :straight nil
  :after (eglot flycheck)
  :commands +lsp-eglot-prefer-flycheck-h +add-flycheck-eglot-checker
  :load-path "modules/"
  :init
  (+add-flycheck-eglot-checker)
  :ghook
  ('eglot--managed-mode-hook #'+lsp-eglot-prefer-flycheck-h))

;; python tweaks
(use-package python
  :straight (:type built-in)
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt -i"))

(use-package pyimport
  :general
  (general-nvmap
    :keymaps 'python-mode-map
    :prefix ","
    "i" '(nil :which-key "imports")
    "iu" 'pyimport-remove-unused
    "ii" 'pyimport-insert-missing))

;; this is great for org etc, but for existing notebooks is lacking
(use-package jupyter
  ;; :straight (:no-native-compile t)
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
            [remap save-buffer] #'ein:notebook-save-notebook-command-km
            "C-j" #'ein:worksheet-goto-next-input-km
            "C-k" #'ein:worksheet-goto-prev-input-km))

(use-package js2-mode
  :interpreter "node"
  :commands js2-line-break
  :hook (js-mode . js2-minor-mode)
  :custom
  (js-chain-indent t)
  ;; Don't mishighlight shebang lines
  (js2-skip-preprocessor-directives t)
  ;; let flycheck handle this
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil)
  ;; Flycheck provides these features, so disable them: conflicting with
  ;; the eslint settings.
  (js2-strict-trailing-comma-warning nil)
  (js2-strict-missing-semi-warning nil)
  ;; maximum fontification
  (js2-highlight-level 3)
  (js2-highlight-external-variables t)
  (js2-idle-timer-delay 0.1))

(use-package js2-refactor
  :hook (js2-minor-mode . js2-refactor-mode)
  :general
  (general-nvmap
    :keymaps 'js2-mode
    "," '(:keymap js2-refactor-mode-map)))

(use-package rjsx-mode
  :mode "/.*\\.js\\'")

(use-package json-mode)
(use-package yaml-mode)
(use-package typescript-mode)
(use-package tide
  :disabled
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))

;;; utilities
(use-package restart-emacs
  :general
  (:prefix-map '+quit-map "r" 'restart-emacs))

(defun recentf-open-files+ ()
  "Use `completing-read' to open a recent file."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Find recent file: " files nil t))))

;;; Snippets
(use-package yasnippet
  :hook ((prog-mode text-mode) . yas-global-mode)
  :general (:prefix-map '+insert-map
                        "s" 'yas-insert-snippet))
(use-package yasnippet-snippets
  :after yasnippet)
(use-package doom-snippets
  :straight (:host github :repo "hlissner/doom-snippets")
  :after yasnippet)

(use-package auto-activating-snippets
  :straight (:host github :repo "ymarco/auto-activating-snippets")
  :ghook ('LaTeX-mode-hook #'auto-activating-snippets-mode)
  :config
  (aas-set-snippets 'latex-mode
                    "On" "O(n)"
                    "$" (lambda () (interactive)
                          (yas-expand-snippet "\\($0\\)"))))

;; better help buffers
(use-package helpful
  :general
  (:prefix-map 'help-map
               "f" #'helpful-callable
               "v" #'helpful-variable
               "k" #'helpful-key
               "h" #'helpful-at-point))

(use-package adaptive-wrap
  :general
  (:prefix-map '+toggle-map
               "w" #'adaptive-wrap-prefix-mode))

;;; latex
(use-package company-auctex)
(use-package company-reftex)
(use-package company-math)
(use-package company-bibtex)

(use-package auctex
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
  (TeX-electric-math (cons "\\(" "\\)"))
  :hook ((TeX-mode . +latex-setup)
         ;; (TeX-mode . +latex-smartparens)
         (TeX-mode . TeX-fold-mode))
  :mode ("\\.tex\\'" . LaTeX-mode)
  :general
  (:keymaps 'TeX-mode-map
            [remap compile] #'TeX-command-master
            [remap recompile] (lambda () (TeX-command-master +1)))
  :preface
  (defun +latex-setup ()
    (turn-on-visual-line-mode)
    (visual-fill-column-mode +1)
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

(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode))

(use-package bibtex
  :straight (:type built-in)
  :gfhook #'+bibtex-setup
  :preface
  (defun +bibtex-setup ()
    (turn-on-visual-line-mode)
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width 100)))

(use-package auctex-latexmk
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :hook
  (TeX-mode . auctex-latexmk-setup))

(use-package reftex
  :straight (:type built-in)
  :hook ((TeX-mode . reftex-mode)
         (LaTeX-mode . reftex-mode))
  :custom
  (reftex-cite-format
   '((?a . "\\autocite[]{%l}")
     (?b . "\\blockcquote[]{%l}{}")
     (?c . "\\cite[]{%l}")
     (?f . "\\footcite[]{%l}")
     (?n . "\\nocite{%l}")
     (?p . "\\parencite[]{%l}")
     (?s . "\\smartcite[]{%l}")
     (?t . "\\textcite[]{%l}"))
   (reftex-plug-into-AUCTeX t)
   (reftex-toc-split-windows-fraction 0.3)))

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

;; projectile
(use-package projectile
  :custom
  (projectile-completion-system 'default)
  (projectile-auto-discovery t)
  :hook (after-init . projectile-mode)
  :general
  (+leader-def
    "p" '(:keymap projectile-command-map
          :package projectile
          :which-key "projects")))

;; Org Mode
(use-package org
  :custom
  (org-startup-indented t)
  (org-directory "~/Documents/org")
  (org-default-notes-file (concat org-directory "/notes.org"))
  :ghook
  ('org-mode-hook '(visual-line-mode visual-fill-column-mode org-superstar-mode))
  :general
  (:prefix-map '+open-map
               "c" #'org-capture)
  (+local-leader-def :keymaps 'org-mode-map
    "," #'org-ctrl-c-ctrl-c
    "t" #'org-todo
    "o" #'org-open-at-point))

(use-package org-superstar
  :custom (org-superstar-special-todo-items t))

;; languages + highlighting

(use-package editorconfig
  :custom (editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :hook (after-init . editorconfig-mode))

(use-package julia-mode
  :mode "\.*\.jl")

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :custom (markdown-commnd "multimarkdown")
  :ghook
  ('(markdown-mode-hook gfm-mode-hook)
   #'(visual-line-mode visual-fill-column-mode))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package systemd)

(use-package tree-sitter
  :init (global-tree-sitter-mode))
(use-package tree-sitter-langs)
(use-package tree-sitter-hl
  :straight nil
  :after tree-sitter tree-sitter-langs
  :ghook (#'tree-sitter-after-on-hook  #'tree-sitter-hl-mode))

(use-package emacs-lisp
  :straight (:type built-in)
  :general
  (+local-leader-def :keymaps 'emacs-lisp-mode-map
    "e" #'eval-last-sexp))

(use-package +lisp-indent
  :straight nil
  :load-path "modules/"
  :init
  (general-add-advice
   #'calculate-lisp-indent
   :override #'void~calculate-lisp-indent))

(use-package calc
  :straight (:type built-in)
  :hook (calc-mode . calc-algebraic-mode)
  :general
  (:prefix-map '+open-map
               "c" #'calc-dispatch))

(use-package compile
  :straight (:type built-in)
  :preface
  (defun +compile/apply-ansi-color-to-compilation-buffer-h ()
    "Applies ansi codes to the compilation buffers. Meant for
     `compilation-filter-hook'."
    (with-silent-modifications
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (defun +compile/fix-compilation-size ()
    (with-selected-window (get-buffer-window "*compilation*")
      (setq window-size-fixed t)
      (window-resize (selected-window) (- 30 (window-total-width)) t t)))
  :custom
  (compilation-scroll-output 'first-error)
  :ghook
  ('compilation-filter-hook #'(+compile/apply-ansi-color-to-compilation-buffer-h
                               +compile/fix-compilation-size)))

;; vterm
(use-package vterm
  :preface
  (defun +vterm/evil-collection-vterm-escape-stay ()
    "Go back to normal state but don't move cursor backwards. Moving
    cursor backwards is the default vim behaviour but it is not appropriate
    in some cases like terminals."
    (setq-local evil-move-cursor-back nil))
  (defun +vterm/set-cursor-shape ()
    (setq-local cursor-type 'bar))
  :custom
  (vterm-buffer-name-string "vterm: %s")
  :ghook
  ('vterm-mode-hook #'(+vterm/evil-collection-vterm-escape-stay
                       +vterm/set-cursor-shape))
  :general
  (general-imap :keymaps 'vterm-mode-map
    "C-i" #'vterm-send-escape))
(use-package vterm-toggle
  :commands (vterm-toggle)
  :general
  (+leader-def
    "'" #'vterm-toggle)
  (:prefix-map '+open-map
               "t" #'vterm-toggle
               "T" #'vterm)
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
;; This should be at/near the bottom since you want this hook to be
;; run before others. Hooks are apparently a stack
(use-package envrc
  :hook (after-init . envrc-global-mode))

;; arch PKGBUILDS
(use-package pkgbuild-mode
  :mode ("PKGBUILD" . pkgbuild-mode))

;; different git file modes
(use-package git-modes
  :mode ("/.dockerignore\\'" . gitignore-mode))

;; eshell fish completion
(use-package eshell
  :straight (:type built-in)
  :ghook 'visual-line-mode
  :general
  (:prefix-map '+open-map
               "e" 'eshell))

(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode)
  :custom (fish-completion-fallback-on-bash-p t))

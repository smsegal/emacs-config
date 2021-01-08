;;; init.el -*- lexical-binding: t; -*-

;; load straight so we can use the correct version of org to load the literate config
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

;; Personal Information

(setq user-full-name "Shane Segal"
      user-mail-address "shane@smsegal.ca")
(setq auth-sources '("~/.authinfo.gpg"))

;; Package Management and Use-Package Setup
;; Using straight.el (loaded in ~init.el~) to load use-package and setup integration.

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)
;; loading the additional functions from straight-x
(use-package straight-x
  :straight nil)

;; Custom File
;; Load custom settings from a separate file instead of polluting the init file.

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

;; Variables for later use.
;; Macos needs a few separate tweaks so we set this up here.

(defvar IS-MAC (eq system-type 'darwin))

;; Garbage Collection
;; This package changes the behaviour of the garbage collector to act during idle time.

(use-package gcmh
  :hook (after-init . gcmh-mode))
;; Set up the path properly on GUI Emacs.

(use-package exec-path-from-shell
  :when (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

;; Keep ~user-emacs-directory~ clean
;; We also set up ~recentf-mode~ since it relies on no-littering being loaded right before.
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

;; Performance and Security
;; These tweaks (mainly taken from doom emacs) enhance performance and provide (the appearance of?) security.

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  ;; Credit: Doom Emacs
  ;; Contrary to what many Emacs users have in their configs, you really don't
  ;; need more than this to make UTF-8 the default coding system:
  (locale-coding-system 'utf-8)
  ;; Disable bidirectional text rendering for a modest performance boost. I've set
  ;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
  ;; is an undefined state and suggest this to be just as good:
  (bidi-display-reordering 'left-to-right)
  (bidi-paragraph-direction 'left-to-right)
  ;; Disabling the BPA makes redisplay faster, but might produce incorrect display
  ;; reordering of bidirectional text with embedded parentheses and other bracket
  ;; characters whose 'paired-bracket' Unicode property is non-nil.
  (bidi-inhibit-bpa t)  ; Emacs 27 only
  ;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
  ;; in non-focused windows.
  (cursor-in-non-selected-windows nil)
  (highlight-nonselected-windows nil)
  ;; More performant rapid scrolling over unfontified regions. May cause brief
  ;; spells of inaccurate syntax highlighting right after scrolling, which should
  ;; quickly self-correct.
  (fast-but-imprecise-scrolling t)
  ;; Don't ping things that look like domain names.
  (ffap-machine-p-known 'reject)
  ;; security (tls) tweaks
  (gnutls-verify-error (not (getenv-internal "INSECURE"))
                       gnutls-algorithm-priority
                       (when (boundp 'libgnutls-version)
                         (concat "SECURE128:+SECURE192:-VERS-ALL"
                                 (if (and (not (version< emacs-version "26.3"))
                                          (>= libgnutls-version 30605))
                                     ":+VERS-TLS1.3")
                                 ":+VERS-TLS1.2"))
                       ;; `gnutls-min-prime-bits' is set based on recommendations from
                       ;; https://www.keylength.com/en/4/
                       gnutls-min-prime-bits 3072
                       tls-checktrust gnutls-verify-error
                       ;; Emacs is built with `gnutls' by default, so `tls-program' would not be
                       ;; used in that case. Otherwise, people have reasons to not go with
                       ;; `gnutls', we use `openssl' instead. For more details, see
                       ;; https://redd.it/8sykl1
                       tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                                     "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
  --strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                                     ;; compatibility fallbacks
                                     "gnutls-cli -p %p %h"))
  :config
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode)))
(prefer-coding-system 'utf-8)

;; Server
;; Start the server unless it's already running.

(use-package server
  :straight (:type built-in)
  :config
  (unless (server-running-p)
    (server-start)))

;; Using ~general.el~ for easily setting up keybindings
;; ~general.el~ lets us pretty easily set up keybindings and keymaps, and
;; is used extensively throughout the rest of this configuration.

;; Here, I'm setting up the top-level keybindings and leader keys. I'll
;; bind to these keymaps in the relevant packages later.  I'm also
;; setting it up before ~evil-mode~ in the actual org file, but saying it
;; should load after ~evil~ in the ~use-package~ block for organizational purposes.

(use-package general
  :after evil
  :custom
  (general-override-states
   '(insert emacs hybrid normal visual motion operator replace))
  :config
  (general-evil-setup)

  ;; text indentation stuff
  (general-add-hook (list 'prog-mode-hook 'text-mode-hook)
                    (lambda () (setq-local indent-tabs-mode nil)))

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
    "e" #'eval-buffer)

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

;; Evil Mode et. al
;; ~evil-mode~ itself. I tried the default keybindings, but my pinkies hurt within the week

(use-package undo-fu)
(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-ex-substitute-global t)
  (evil-respect-visual-line-mode t)
  (evil-want-Y-yank-to-eol t)
  (evil-cross-lines nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-undo-system 'undo-fu)
  (evil-regexp-search t)
  (evil-move-cursor-back t)
  (evil-undo-system 'undo-fu)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode +1))
;; Apparently ~undo-tree~ has had it's performance improved, will try it again sometime

(use-package undo-tree
  :disabled
  :custom (evil-undo-system 'undo-tree)
  :config (global-undo-tree-mode +1))

;; ~evil-collection~ provides evil keybindings for almost every package.

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
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
  (add-to-list 'evil-escape-excluded-major-modes 'vterm-mode)
  (evil-escape-mode +1))

;; This is the dumping ground for some random elisp relating to ~evil~.

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

;; Surround text objects with different delimiters

(use-package evil-surround
  :config
  (global-evil-surround-mode +1))
(use-package evil-embrace
  :after evil-surround
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

;; Bufler and Burly
;; These are both packages written by alphapapa, a prolific emacs package
;; dev. Bufler organizes buffers by mode and project and provides a good
;; UI for switching among them.

;; Burly is a lightweight workspace manager that builds on top of emacs
;; bookmark system.

(use-package bufler
  :disabled
  :hook (after-init . bufler-mode)
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
  (:prefix-map '+buffer-map
               "o" 'burly-open-bookmark
               "w" 'burly-bookmark-windows
               "F" 'burly-bookmark-frames))

;; Switch to the scratch buffer

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

;; Window Management
;; Undo and redo window configurations.
(use-package winner
  :straight (:type built-in)
  :hook (after-init . winner-mode)
  :general
  (:prefix-map 'evil-window-map
               "u" 'winner-undo
               "r" 'winner-redo))

;; Switch to a window with the keyboard like avy.

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 3.0)
  :general (:prefix-map 'evil-window-map
                        "w" #'ace-window
                        "W" #'ace-swap-window))

;; Window Enlargement

(use-package zoom
  :custom
  (zoom-size '(0.7 . 0.7))
  (zoom-ignored-major-modes '(dired-mode vterm-mode
                              help-mode helpful-mode
                              rxt-help-mode help-mode-menu
                              org-mode))
  (zoom-ignored-buffer-names '("*scratch*" "*info*" "*helpful variable: argv*"))
  (zoom-ignored-buffer-name-regexps '("^\\*calc" "\\*helpful variable: .*\\*"))
  (zoom-ignore-predicates (list (lambda () (< (count-lines (point-min) (point-max)) 20))))
  :general
  (:prefix-map '+toggle-map
               "z" #'zoom-mode))

(use-package +enlarge-window
  :straight nil
  :load-path "modules/"
  :general (:prefix-map 'evil-window-map
                        "o" #'+window-enlargen
                        "O" #'delete-other-windows))

;; Modeline
;; I use the moody modeline (by the author of magit). I also use minions
;; to hide all the other modes active instead of diminishing them.
;; I also depend on smart-mode-line for the buffer-naming.

;; used for buffer identification in moody modeline
(use-package smart-mode-line)
(use-package minions
  :config (minions-mode 1))
(use-package moody
  :after smart-mode-line
  :config
  (moody-replace-sml/mode-line-buffer-identification)
  (moody-replace-vc-mode))
;; Anzu highlights current search results in the modeline.

(use-package anzu
  :hook (after-init . global-anzu-mode))
(use-package evil-anzu)

;; Dashboard
;; A nice start page for emacs. I set a custom logo for the buffer, and
;; enable ~all-the-icons~ support.

;; dashboard
(use-package dashboard
  :init
  (setq dashboard-set-footer nil)
  (setq dashboard-center-content t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-init-info t)
  (setq dashboard-startup-banner (concat user-emacs-directory "emacs-bigsur_small.png"))
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (bookmarks . 5)))
  :config
  (dashboard-setup-startup-hook))

;; Themes
;; I use the great ~doom-themes~ package from Doom. It provides a whole
;; ton of great light and dark themes.

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package modus-themes
  :disabled
  :straight
  (:host gitlab :repo "protesilaos/modus-themes" :branch "main")
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-slanted-constructs t)
  (modus-themes-syntax 'faint)
  (modus-themes-completions 'opinionated)
  (modus-themes-paren-match 'intense-bold)
  (modus-themes-org-blocks 'rainbow)
  (modus-themes-mode-line 'moody))

(use-package circadian
  :custom
  (calendar-latitude 43.6)
  (calendar-longitude -79.4)
  (circadian-themes '((:sunrise . doom-acario-light)
                      (:sunset  . doom-gruvbox)))
  :hook
  (after-init . circadian-setup))

(use-package all-the-icons)

;; Font
;; I like a font with ligatures and I like my comments italic.

(defvar FONT-NAME "Victor Mono")
;; macos needs a larger font due to hidpi
(set-face-attribute 'default nil
                    :family FONT-NAME
                    :height (if IS-MAC 180 110))

(set-frame-font FONT-NAME nil t)
;; this is a fix for doom-acario-theme setting a weird font
(set-face-attribute 'fixed-pitch-serif nil :family FONT-NAME)
;; italic comments
(set-face-attribute 'font-lock-comment-face nil :family FONT-NAME :slant 'italic)

;; Here is where we set up the ligatures. There's configuration for the
;; fonts I use most often: "Victor Mono" and "JetBrains Mono".

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :ghook ('after-init-hook #'global-ligature-mode)
  :init
  (cond
   ;; JetBrains Mono Ligatures
   ((string= (face-attribute 'default :family) "JetBrains Mono")
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
          "/=" "//=" "/==" "@_" "__")))
   ;; Victor Mono Ligatures
   ((string= (face-attribute 'default :family) "Victor Mono")
    (ligature-set-ligatures
     't '("</" "</>" "/>" "~-" "-~" "~@" "<~" "<~>" "<~~" "~>" "~~"
          "~~>" ">=" "<=" "<!--" "##" "###" "####" "|-" "-|" "|->"
          "<-|" ">-|" "|-<" "|=" "|=>" ">-" "<-" "<--" "-->" "->" "-<"
          ">->" ">>-" "<<-" "<->" "->>" "-<<" "<-<" "==>" "=>" "=/="
          "!==" "!=" "<==" ">>=" "=>>" ">=>" "<=>" "<=<" "<<=" "=<<"
          ".-" ".=" "=:=" "=!=" "==" "===" "::" ":=" ":>" ":<" ">:"
          ";;" "<|" "<|>" "|>" "<>" "<$" "<$>" "$>" "<+" "<+>" "+>"
          "?=" "/=" "/==" "/\\" "\\/" "__" "&&" "++" "+++")))))

;; UI Tweaks
;; What the hell do I press next? Which-key answers that question.

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

;; A lot of the built-in UI needs some tweaks. We disable menu bars, the toolbar and the scrollbar.
;; We also want to confirm things with a single y/n instead of the whole word.

(setq confirm-nonexistent-file-or-buffer nil)
(setq mouse-yank-at-point t)

;; make underlines look a little better
(setq x-underline-at-descent-line t)

;; window resizing
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; bars
(setq menu-bar-mode   nil)
(setq tool-bar-mode   nil)
(setq scroll-bar-mode nil)

;; set this for all prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; ui cruft
(unless (assq 'menu-bar-lines default-frame-alist)
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars)))

;; Visual Fill Column
;; Sometimes we want text to wrap before the window border.

(use-package visual-fill-column
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  ;; (setq-default split-window-preferred-function 'visual-fill-column-split-window-sensibly)
  :ghook
  ('visual-fill-column-mode-hook #'(visual-line-mode
                                    (lambda ()
                                      (setq-local split-window-preferred-function
                                       'visual-fill-column-split-window-sensibly)))))

;; Misc Tweaks
;; Pulse current line on window switch

(use-package beacon
  :disabled
  :hook (after-init . beacon-mode)
  :config
  (add-to-list 'beacon-dont-blink-commands 'vterm-send-return)
  (add-to-list 'beacon-dont-blink-commands 'mwheel-scroll))

;; Hide the mouse when we type near it.

;; (use-package avoid
;;   :straight (:type built-in)
;;   :config
;;   ;; doesn't seem to do any animating, at least on wayland should
;;   ;; check it out on X (but I never use X soooo)
;;   (mouse-avoidance-mode 'exile))

;; Highlight TODO keywords when they appear in comments

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; This is a great one. Dim the background colour of the buffers you're not currently editing in.

(use-package auto-dim-other-buffers
  :hook (after-init . auto-dim-other-buffers-mode)
  :custom
  (auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
  (auto-dim-other-buffers-dim-on-focus-out nil))

;; Highlight different things. The parentheses surround the point get
;; highlighted which is great.
(use-package highlight-parentheses
  :hook ((prog-mode org-mode LaTeX-mode) . highlight-parentheses-mode))

(use-package hl-line
  :straight (:type built-in)
  :hook ((prog-mode conf-mode) . hl-line-mode)
  :custom
  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil))

;; Change the shape of the cursor when running in the tty. Also enable the mouse.
(use-package evil-terminal-cursor-changer
  :straight (:host github :repo "kisaragi-hiu/evil-terminal-cursor-changer")
  :hook (tty-setup . evil-terminal-cursor-changer-activate))

(use-package xterm-mouse-mode
  :straight (:type built-in)
  :hook (tty-setup . xterm-mouse-mode))

;; Scrolling

(use-package emacs
  :custom
  (hscroll-margin 2)
  (hscroll-step 1)
  ;; Emacs spends too much effort recentering the screen if you scroll the
  ;; cursor more than N lines past window edges (where N is the settings of
  ;; `scroll-conservatively'). This is especially slow in larger files
  ;; during large-scale scrolling commands. If kept over 100, the window is
  ;; never automatically recentered.
  (scroll-conservatively 101)
  (scroll-margin 0)
  (scroll-preserve-screen-position t)
  ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
  ;; for tall lines.
  (auto-window-vscroll nil)
  ;; mouse
  (mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((meta)) ((control) . text-scale)))
  (mouse-wheel-progressive-speed nil))  ; don't accelerate scrolling

(use-package scroll-on-jump
  :disabled
  :after (evil goto-chg)
  :straight (:host gitlab :repo "ideasman42/emacs-scroll-on-jump")
  :custom
  (scroll-on-jump-duration 0.4)
  (scroll-on-jump-use-curve t)
  :config
  (scroll-on-jump-advice-add evil-undo)
  (scroll-on-jump-advice-add evil-redo)
  (scroll-on-jump-advice-add evil-jump-item)
  (scroll-on-jump-advice-add evil-jump-forward)
  (scroll-on-jump-advice-add evil-jump-backward)
  (scroll-on-jump-advice-add evil-ex-search-next)
  (scroll-on-jump-advice-add evil-ex-search-previous)
  (scroll-on-jump-advice-add evil-forward-paragraph)
  (scroll-on-jump-advice-add evil-backward-paragraph)

  (scroll-on-jump-advice-add goto-last-change)
  (scroll-on-jump-advice-add goto-last-change-reverse))

;; Writeroom Mode
;; Enable a nicer writing environment

(use-package writeroom-mode
  :custom
  (writeroom-global-effects '(writeroom-set-bottom-divider-width))
  :config
  (general-add-advice 'text-scale-adjust :after
                      #'visual-fill-column-adjust)
  :general
  (:prefix-map '+toggle-map "z" 'writeroom-mode))

;; Incremental Completion
;; We're using Selectrum and the associated ecosystem. They have a bigger
;; focus on creating and extending basic APIs vs alternative like Ivy.

(use-package selectrum
  :commands selectrum-next-candidate selectrum-previous-candidate
  :hook
  (emacs-startup . selectrum-mode)
  :general
  (general-imap "C-k" nil)
  (:keymaps 'selectrum-minibuffer-map
            "C-j" 'selectrum-next-candidate
            "C-k" 'selectrum-previous-candidate))

;; Prescient is a sorting/filtering package that orders results by "frecency".

(use-package prescient
  :hook (after-init . prescient-persist-mode))
(use-package selectrum-prescient
  :hook (selectrum-mode . selectrum-prescient-mode))
(use-package company-prescient
  :hook (company-mode . company-prescient-mode))

;; Consult is to selectrum as counsel is to Ivy.
;; Marginalia is a bit of extra eye-candy on top of Consult.

(use-package consult
  :straight (:host github :repo "minad/consult")
  ;; :after project
  ;; :preface
  ;; ;; Hack from pr 33 on consult. remove this once it's merged upstream
  ;; (defun consult--outline-show-branch-maybe ()
  ;;   "Reveal the current outline branch.
  ;;       Show all of the current headine's parents and their children. This includes this
  ;;       headline."
  ;;   (when (outline-invisible-p (line-end-position))
  ;;     (let (points)
  ;;       (save-excursion
  ;;         (outline-back-to-heading :invisible-ok)
  ;;         (push (point) points)
  ;;         (while (ignore-errors (outline-up-heading 1 :invisible-ok))
  ;;           (push (point) points))
  ;;         (dolist (point points)
  ;;           (goto-char point)
  ;;           (outline-show-children)
  ;;           (outline-show-entry))))))
  :init
  ;; (general-add-advice 'consult-line :after #'consult--outline-show-branch-maybe)
  ;; (general-add-advice 'consult-outline :after #'consult--outline-show-branch-maybe)

  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur)
  ;; :config
  ;; (setq consult-project-root-function #'+get-project-root)
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
               "S" #'consult-line-symbol-at-point
               "o" #'consult-outline)
  :general
  (:prefix-map '+code-map
               "x" #'consult-error))
(use-package consult-selectrum)
(use-package consult-flycheck)
(use-package marginalia
  :straight (:host github :repo "minad/marginalia" :branch "main")
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
  :init
  (marginalia-mode)

  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))

;; Searching / Narrowing
;; deadgrep lets us search the specified director with ripgrep. Provides a good UI

(use-package deadgrep
  :general
  (:prefix-map '+search-map
               "d" #'deadgrep))

;; narrow-to-region etc is defined in builtin package page
(use-package page
  :straight (:type built-in)
  :init
  (put 'narrow-to-page 'disabled nil)
  :general
  (:prefix-map '+narrow/notes-map
               "n" #'narrow-to-region
               "p" #'narrow-to-page
               "d" #'narrow-to-defun
               "w" #'widen))

;; Spellcheck
;; Spell checking with flyspell and enchant.

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

;; Correct the word at the point with ~z=~.

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

;; Crux
;; Crux is a selection of useful functions.

(use-package crux
  :general
  (:prefix-map '+file-map
               "E" #'crux-sudo-edit
               "D" #'crux-delete-file-and-buffer
               ;; "p" #'crux-find-user-init-file
               "R" #'crux-rename-file-and-buffer)
  (:prefix-map '+open-map
               "w" #'crux-open-with))

;; Dired and File Management

  ;;; File Management with Dired
(use-package dired
  :straight (:type built-in)
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-dwim-target t)
  (dired-delete-by-moving-to-trash t)
  :ghook
  ('dired-mode-hook #'(dired-async-mode))
  :general
  (:prefix-map '+open-map
               "-" #'dired-jump)
  (general-nmap :keymaps 'dired-mode-map
    "h" #'dired-up-directory
    "l" #'dired-find-file))
(use-package diredfl
  :hook (dired-mode . diredfl-mode))
(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package ranger :disabled)

(use-package +find-init-file-here
  :straight nil
  :preface
  (defun +find-init-file-here ()
    (interactive)
    (find-file (expand-file-name "config.org" user-emacs-directory)))
  :general
  (:prefix-map '+file-map
               "p" #'+find-init-file-here))

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

;; Text Manipulations
;; Rotating text lets you toggle things under point where that makes sense.
;; Subword mode lets you navigate camelCase words etc.
;; We also want to clean up whitespace in prog-mode.

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
  :hook (prog-mode . subword-mode)
  :general
  (:prefix-map '+toggle-map
               "s" #'subword-mode))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; Set up automatic pairing of {(<>)}

(use-package electric-pair
  :straight (:type built-in)
  :hook (emacs-startup . electric-pair-mode))

;; Match the indentation of wrapped lines

(use-package adaptive-wrap
  :general
  (:prefix-map '+toggle-map
               "w" #'adaptive-wrap-prefix-mode))

;; Autocomplete and Syntax Checking
;; Using company for auto completion and flycheck.

;;; autocomplete
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
(use-package company-posframe
  :unless IS-MAC
  :hook (company-box-mode . company-posframe-mode))

;; syntax checking
(use-package flycheck
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook (after-init . global-flycheck-mode)
  :general
  (:prefix-map '+code-map
               "x" '(flycheck-list-errors :which-key "show errors")))

;; VC / Git
;; Magit is probably the single best emacs package.
;; We also use the build-int VC mode for some things like ediff.

  ;;; vc-mode and Magit
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

;; Tree-sitter
;; Now, apparently this package is useful for a ton of different
;; things. I use it for the nicer syntax highlighting in supported
;; languages.

(use-package tree-sitter
  :ghook ('(python-mode-hook
            js2-mode-hook
            typescript-mode-hook
            css-mode-hook) #'tree-sitter-hl-mode))
(use-package tree-sitter-langs)

;; Formatting
;; Format all code with one keybinding.

(use-package format-all
  :general
  (:prefix-map '+code-map
               "f" 'format-all-buffer))

;; Use aphelia when the mode is supported, and fallback to format-all otherwise.

(use-package apheleia
  :straight (:host github :repo "raxod502/apheleia")
  :general
  (:keymaps '(python-mode-map js-mode-map)
            [remap format-all-buffer] #'apheleia-format-buffer))

;; Code Search
;; Automatically jump to definitions in different languages.

(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xreg-activate))

;; Editorconfig
;; Per directory spaces/tabs indentation.

(use-package editorconfig
  :custom (editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :hook (after-init . editorconfig-mode))

;; Compilation
;; Make compilation buffers process escape codes for colours etc.

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
  :general
  (:prefix-map '+code-map
               "c" #'compile
               "m" #'recompile)
  :ghook
  ('compilation-filter-hook #'+compile/apply-ansi-color-to-compilation-buffer-h))
;; +compile/fix-compilation-size)))

;; Makefiles
;; I disabled indent-tabs-mode above, I need this enabled for makefiles.

(general-add-hook
 'makefile-mode-hook (lambda ()
                       (setq-local indent-tabs-mode +1)))

;; Org Mode
;; We don't do much to customize Org. Set the notes directory for
;; ~org-capture~, enable some languages to be evaled in ~src~ blocks.
;; I also set up fancy heading symbols with org bullets.

;; I'm aiming to translate a lot of keys to vim-like equivalents, using
;; leader keys to replace the special ~C-c~ bindings.

(use-package org
  ;; :straight (:host github
  ;;                  :repo "emacs-straight/org-mode"
  ;;                  :files ("*.el" "lisp/*.el" "contrib/lisp/*.el"))
  :custom
  (org-startup-indented t)
  (org-src-fontify-natively t)
  (org-directory "~/Documents/org")
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-export-backends '(beamer html md man latex))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
  :hook (org-mode . visual-line-mode)
  :general
  (:prefix-map '+open-map
               "c" #'org-capture)
  (+local-leader-def :keymaps 'org-mode-map
    "," #'org-ctrl-c-ctrl-c
    "'" #'org-edit-special
    "t" #'org-todo
    "o" #'org-open-at-point)
  (+local-leader-def :keymaps 'org-src-mode-map
    "," #'org-edit-src-exit
    "k" #'org-edit-src-abort))

(use-package org-superstar
  :ghook ('org-mode-hook #'org-superstar-mode)
  :custom (org-superstar-special-todo-items t))

;; Table of Contents
;; Have an auto-updated TOC, primarly for github readme support

(use-package toc-org
  :hook (org-mode . toc-org-mode))

;; LSP-Mode
;; We use LSP mode for pretty much everything we use a lot.
;; We set some of the more intrusive UI elements to nil.
;; Enable support for pyright language server.

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (read-process-output-max (* 1024 1024)) ;; 1mb
  (lsp-completion-provider :capf)
  (lsp-enable-folding t)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-snippet t)
  (lsp-eldoc-enable-hover nil)
  (lsp-headerline-breadcrumb-enable t)
  :ghook
  ('(TeX-mode-hook
     yaml-mode-hook
     sh-mode-hook
     js2-mode-hook) #'lsp-deferred)
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
            ;; [remap format-all-buffer] #'lsp-format-buffer
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

;; Python
;; The builtin package needs some simple tweaks to use ipython as the REPL.

;; python tweaks
(use-package python
  :straight (:type built-in)
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt -i"))

;; Pyimport
;; We can sort and remove imports from files with this.

(use-package pyimport
  :general
  (general-nvmap
    :keymaps 'python-mode-map
    :prefix ","
    "i" '(nil :which-key "imports")
    "iu" 'pyimport-remove-unused
    "ii" 'pyimport-insert-missing))

;; Jupyter Kernal and Notebook support
;; The ein package has really improved lately. In addition, the jupyter
;; kernel provides a pretty good experience for using it inside org-mode.

;; (use-package jupyter
;;   :straight (:no-native-compile t)
;;   :commands jupyter-connect-repl jupyter-run-repl)

(use-package emacs-ipython-notebook
  :straight ein
  :hook (ein:notebook-mode . evil-normalize-keymaps)
  :custom
  (ein:output-area-inlined-images t)
  (ein:polymode t)
  :commands (ein:run ein:login)
  :preface
  (general-add-advice 'ein:worksheet-execute-cell-and-goto-next-km
                      :after (lambda () (interactive)
                               (evil-scroll-line-to-center)))
  :init
  (evil-define-minor-mode-key '(normal visual) 'ein:notebook-mode
    (kbd "<C-return>") #'ein:worksheet-execute-cell-km
    (kbd "<S-return>") #'ein:worksheet-execute-cell-and-goto-next-km)
  :general
  (:keymaps 'ein:notebook-mode-map
            [remap save-buffer] #'ein:notebook-save-notebook-command-km
            "C-j" #'ein:worksheet-goto-next-input-km
            "C-k" #'ein:worksheet-goto-prev-input-km))

;; LaTeX
;; Set up company mode for autocompletion of references, citations, etc.
;; We also setup inline pdf viewing.

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
  (TeX-fold-type-list '(env math))
  ;; insert \(\) instead of $$
  (TeX-electric-math (cons "\\(" "\\)"))
  :hook ((TeX-mode . +latex-setup)
         (TeX-mode . TeX-fold-mode))
  :mode ("\\.tex\\'" . LaTeX-mode)
  :general
  ;; (:keymaps 'TeX-mode-map
  ;;           ;; [remap compile] #'TeX-command-master)
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
                                         company-backends))))
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
  :mode ("\\.pdf\\'" . pdf-view-mode)
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

;; {Java,Type}Script
;; The different web languages. I've also enabled LSP-mode for them above.

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

;; Haskell
;; Basic syntax highlighting.

(use-package haskell-mode)
(use-package hlint-refactor
  :commands (hlint-refactor-refactor-buffer hlint-refactor-refactor-at-point)
  :general
  (+local-leader-def :keymaps 'haskell-mode-map
    "r" '(nil :which-key "refactor")
    "rr" #'hlint-refactor-refactor-at-point
    "rR" #'hlint-refactor-refactor-buffer))

(use-package flycheck-haskell
  :hook (flycheck-mode . flycheck-haskell-setup))

(use-package dante
  :disabled
  :after haskell-mode
  :commands 'dante-mode
  :ghook ('haskell-mode-hook '(flycheck-mode-hook 'dante-mode))
  :config
  (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint)))

;; Set up language server support.

(use-package lsp-haskell
  :custom (lsp-haskell-server-path "~/.ghcup/bin/haskell-language-server-wrapper")
  :hook (haskell-mode . lsp-deferred))

;; Emacs-Lisp
;; Custom indentation for lisp code. Small keybindings for evaling sexps.

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
   #'calculate-lisp-indent :override #'void~calculate-lisp-indent))

;; Small Language modes
;; These packages are pretty much just for the syntax highlighting. Don't
;; use these enough for any more sophisticated configuration.

(use-package julia-mode
  :mode "\.*\.jl")

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :custom (markdown-command "multimarkdown")
  :ghook
  ('(markdown-mode-hook gfm-mode-hook)
   #'visual-fill-column-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package systemd)
;; arch PKGBUILDS
(use-package pkgbuild-mode
  :mode ("PKGBUILD" . pkgbuild-mode))

;; different git file modes
(use-package git-modes
  :mode ("/.dockerignore\\'" . gitignore-mode))

;; Integrated Terminal
;; Vterm is by far the best terminal emulator. It now has support for
;; evil-mode motions and such.

(use-package vterm
  :preface
  ;; Add evil specific bindings that work with vterm mode
  (defun vterm-evil-insert ()
    (interactive)
    (vterm-goto-char (point))
    (call-interactively #'evil-insert))
  (defun vterm-evil-append ()
    (interactive)
    (vterm-goto-char (1+ (point)))
    (call-interactively #'evil-append))
  (defun vterm-evil-delete ()
    "Provide similar behavior as `evil-delete'."
    (interactive)
    (let ((inhibit-read-only t))
      (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
        (call-interactively 'evil-delete))))
  (defun vterm-evil-change ()
    "Provide similar behavior as `evil-change'."
    (interactive)
    (let ((inhibit-read-only t))
      (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
        (call-interactively 'evil-change))))
  ;; (defun +evil-vterm-hook ()
  ;;   (evil-local-mode 1)
  ;;   (evil-define-key 'normal 'local "a" 'vterm-evil-append)
  ;;   (evil-define-key 'normal 'local "x" 'vterm-evil-delete)
  ;;   (evil-define-key 'normal 'local "i" 'vterm-evil-insert)
  ;;   (evil-define-key 'normal 'local "c" 'vterm-evil-change))
  ;; :ghook ('vterm-mode-hook #'+evil-vterm-hook)
  :custom
  (vterm-buffer-name-string "vterm: %s")
  :general
  (+leader-def "'" #'vterm-other-window)
  (:prefix-map '+open-map
               "t" #'vterm-other-window
               "T" #'vterm)
  (general-nmap :keymaps 'vterm-mode-map
    "a" 'vterm-evil-append
    "d" 'vterm-evil-delete
    "i" 'vterm-evil-insert
    "c" 'vterm-evil-change)
  (general-imap :keymaps 'vterm-mode-map
    "C-i" #'vterm-send-escape))

(use-package vterm-toggle
  :disabled
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

;; TRAMP Support
;; Remote file support.

(use-package tramp
  :straight (:type built-in)
  :custom
  (tramp-default-method "sshx"))

;; Restart emacs

(use-package restart-emacs
  :general
  (:prefix-map '+quit-restart-map "r" 'restart-emacs))

;; Calc mode
;; set calc mode to start in algebraic (ie normal) mode

(use-package calc
  :straight (:type built-in)
  :hook (calc-mode . calc-algebraic-mode)
  :general
  (:prefix-map '+open-map
               "c" #'calc-dispatch))

;; Elcord
;; Discord Integration cause why not

(use-package elcord)

;; Snippets
;; We use yasnippet, as well as the snippets from doom.

;; We also have auto activating snippets, which will insert the specified
;; string /b/ when the string /a/ is written in the enabled mode.

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
                    "On" "O(n)"))

;; A Better Help Buffer
;; We also set apropos to search as much as possible.

(use-package helpful
  :general
  (:prefix-map 'help-map
               "f" #'helpful-callable
               "v" #'helpful-variable
               "k" #'helpful-key
               "h" #'helpful-at-point))

(use-package help
  :straight (:type built-in)
  :config
  (require 'apropos)
  ;; this is taken from
  ;; https://github.com/Wilfred/helpful/issues/25#issuecomment-738347468
  ;; to enable helpful buffers from apropos
  (let ((do-function (lambda (button)
                       (helpful-function (button-get button 'apropos-symbol))))
        (do-variable (lambda (button)
                       (helpful-variable (button-get button 'apropos-symbol)))))
    ;; :supertype only takes effect statically, at the time of
    ;; definition, so we can in fact redefine a button with itself
    ;; as its supertype
    (define-button-type 'apropos-function :supertype 'apropos-function 'action do-function)
    (define-button-type 'apropos-macro :supertype 'apropos-macro 'action do-function)
    (define-button-type 'apropos-command :supertype 'apropos-command 'action do-function)
    (define-button-type 'apropos-variable :supertype 'apropos-variable 'action do-variable)
    (define-button-type 'apropos-user-option :supertype 'apropos-user-option 'action do-variable))
  :custom
  (apropos-do-all t))

;; Project.el
;; This is built in and simpler than projectile.

(use-package project
  :preface
  ;; (cl-defmethod project-root ((project (head local)))
  ;;   (cdr project))

  ;; (defun +_project-files-in-directory (dir)
  ;;   "Use `fd' to list files in DIR."
  ;;   (let* ((default-directory dir)
  ;;          (localdir (file-local-name (expand-file-name dir)))
  ;;          (command (format "fd -t f -0 . %s" localdir)))
  ;;     (project--remote-file-names
  ;;      (sort (split-string (shell-command-to-string command) "\0" t)
  ;;            #'string<))))

  ;; (cl-defmethod project-files ((project (head local)) &optional dirs)
  ;;   "Override `project-files' to use `fd' in local projects."
  ;;   (mapcan #'+_project-files-in-directory
  ;;           (or dirs (list (project-root project)))))
  (defun +get-project-root ()
    (interactive)
    (cdr (project-current)))
  :custom
  (project-switch-commands
   '((?f "File" project-find-file)
     (?g "Grep" project-find-regexp)
     (?d "Dired" project-dired)
     (?b "Buffer" project-switch-to-buffer)
     (?q "Query replace" project-query-replace-regexp)
     (?m "Magit" magit-status)
     (?v "VC dir" project-vc-dir)))
  :general
  (+leader-def
    "p" '(:keymap project-prefix-map
          :package project
          :which-key "projects")))

;; Direnv
;; Direnv automatically adjusts the environment for you when entering a
;; directory with a ~.envrc~ file that contains the appropriate commands.
;; This should be at/near the bottom since you want this hook to be run
;; before others. Hooks are apparently a stack.

;; I'm hooking it to ~emacs-startup-hook~ instead of ~after-init-hook~, as
;; ~emacs-startup-hook~ runs after ~after-init-hook~
(use-package envrc
  :hook (emacs-startup . envrc-global-mode))

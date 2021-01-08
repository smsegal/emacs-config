;; -*- lexical-binding: t; -*-

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
  (dashboard-setup-startup-hook))


;; Themes
;; I use the great ~doom-themes~ package from Doom. It provides a whole
;; ton of great light and dark themes.
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
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


;; What the hell do I press next? Which-key answers that question.
(use-package which-key
  :demand t
  :custom
  (which-key-popup-type 'side-window)
  :config
  (setq which-key-enable-extended-define-key t)
  :hook (after-init . which-key-mode)
  :general
  (:keymaps 'help-map
            "b" #'which-key-show-major-mode
            "B" #'which-key-show-top-level))


;; Pulse current line on window switch
(use-package beacon
  :disabled
  :hook (after-init . beacon-mode)
  :config
  (add-to-list 'beacon-dont-blink-commands 'vterm-send-return)
  (add-to-list 'beacon-dont-blink-commands 'mwheel-scroll))

;; Highlight TODO keywords when they appear in comments
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; This is a great one. Dim the background colour of the buffers you're not currently editing in.
(use-package auto-dim-other-buffers
  :hook (after-init . auto-dim-other-buffers-mode)
  :config
  (setq auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
  (setq auto-dim-other-buffers-dim-on-focus-out nil))

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

;; smooth scrolling when jumping around
(use-package scroll-on-jump
  :disabled
  :after (evil goto-chg)
  :straight (:host gitlab :repo "ideasman42/emacs-scroll-on-jump")
  :config
  (setq scroll-on-jump-duration 0.4)
  (setq scroll-on-jump-use-curve t)
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


;; Visual Fill Column
;; Sometimes we want text to wrap before the window border.
(use-package visual-fill-column
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  :ghook
  ('visual-fill-column-mode-hook #'(visual-line-mode
                                    (lambda ()
                                      (setq-local split-window-preferred-function
                                       'visual-fill-column-split-window-sensibly)))))



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

;; scrolling
(setq hscroll-margin 2)
(setq hscroll-step 1)
;; Emacs spends too much effort recentering the screen if you scroll the
;; cursor more than N lines past window edges (setq where N is the settings of
;; `scroll-conservatively'). This is especially slow in larger files
;; during large-scale scrolling commands. If kept over 100, the window is
;; never automatically recentered.
(setq scroll-conservatively 101)
(setq scroll-margin 0)
(setq scroll-preserve-screen-position t)
;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
;; for tall lines.
(setq auto-window-vscroll nil)
;; mouse
(setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((meta)) ((control) . text-scale)))
(setq mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

;; set this for all prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; ui cruft
(unless (assq 'menu-bar-lines default-frame-alist)
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars)))

(provide '+ui)

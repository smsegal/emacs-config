;; -*- lexical-binding: t; -*-

;; Dashboard
;; A nice start page for emacs. I set a custom logo for the buffer, and
;; enable ~all-the-icons~ support.
(use-package dashboard
  :commands (dashboard-setup-startup-hook)
  :init
  (setq dashboard-set-footer nil)
  (setq dashboard-center-content t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-path-style 'truncate-middle)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t)
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-items '((recents   . 5)
                          (projects  . 5)
                          (bookmarks . 5)))
  (dashboard-setup-startup-hook)
  :config
  (set-face-attribute 'dashboard-items-face nil :inherit 'widget-button :underline nil))

;; Themes
;; I use the great ~doom-themes~ package from Doom. It provides a whole
;; ton of great light and dark themes.
(use-package doom-themes
  :commands (doom-themes-visual-bell-config)
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Theme switcher if we're on linux, otherwise we'll depend on the macos system appearance

(cond ((boundp 'ns-system-appearance-change-functions)
       (defun my/apply-theme (appearance)
         "Load theme, taking current system APPEARANCE into consideration."
         (mapc #'disable-theme custom-enabled-themes)
         (pcase appearance
           ('light (load-theme 'doom-nord-light t))
           ('dark (load-theme 'doom-oceanic-next t))))
       (add-hook 'ns-system-appearance-change-functions #'my/apply-theme))
      ((boundp 'mac-effective-appearance-change-hook)
       (defun my/apply-theme ()
         "Load theme, taking current system APPEARANCE into consideration."
         (mapc #'disable-theme custom-enabled-themes)
         (pcase (plist-get (mac-application-state) ':appearance)
           ("NSAppearanceNameAqua"
            (load-theme 'doom-opera-light t))
           ("NSAppearanceNameDarkAqua"
            (load-theme 'doom-oceanic-next t))))
       (add-hook 'mac-effective-appearance-change-hook #'my/apply-theme)
       (add-hook 'after-init-hook #'my/apply-theme))
      (IS-LINUX
       (use-package circadian
         :config
         (setq calendar-latitude 43.6)
         (setq calendar-longitude -79.4)
         (setq circadian-themes '((:sunrise . doom-nord-light)
                                  (:sunset  . doom-oceanic-next)))
         :hook (after-init . circadian-setup))))

(use-package all-the-icons)

;; Here is where we set up the ligatures. There's configuration for the
;; fonts I use most often: "RecMono Duotone", "Victor Mono" and "JetBrains Mono".
(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :ghook ('(prog-mode-hook org-mode-hook) #'ligature-mode)
  :init
  (ligature-set-ligatures
   't '("- [x]" "- [x]" "####" "<!--" "'''" "---" "=<<" "-->" ">>=" "+++"
        "<*>" "===" "|||" "=/=" "://" "<<<" "-<<" "\"\"\"" "<<-" ">>>"
        "<$>" "<|>" "***" "<=>" "&&&" "!==" "///" "###" "<+>" "<<=" "<<~"
        ">>-" "=>" "==" "<>" "++" "<<" "*/" "*=" ">-" "&&" "?." "|>" ">>"
        "<|" "||" "|=" "##" "<*" "//" "${" "−=" "??" "=~" "\\b" "!~" "__"
        "-<" "<-" "\\r" "?:" "/*" "<=" "!=" "**" "!!" "->" "\\t" "/=" "\\v"
        "\\n" "+=" ">=" "::" "--" "```")))

(defun +_set-font ()
  (let ((font-name "Rec Mono Duotone")
        (font-size (if IS-MAC "14" "10")))
    (set-face-attribute 'fixed-pitch-serif nil :family font-name)
    (set-face-attribute 'font-lock-comment-face nil :family font-name :slant 'italic)
    (set-frame-font (concat font-name "-" font-size) t t)))

(general-after-gui (+_set-font))

(general-after-init (doom-themes-visual-bell-config))

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

;; Highlight TODO keywords when they appear in comments
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; This is a great one. Dim the background colour of the buffers you're not currently editing in.
(use-package auto-dim-other-buffers
  :disabled
  :hook (after-init . auto-dim-other-buffers-mode)
  :config
  (setq auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
  (setq auto-dim-other-buffers-dim-on-focus-out nil))

(use-package solaire-mode
  :init (solaire-global-mode +1))

;; Highlight different things. The parentheses surround the point get
;; highlighted which is great.
(use-package highlight-parentheses
  :init
  (setq show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t)
  :hook
  (((prog-mode org-mode LaTeX-mode) . highlight-parentheses-mode)
   (highlight-parentheses-mode      . show-paren-mode)))

(use-package hl-line
  :straight (:type built-in)
  :hook ((prog-mode conf-mode) . hl-line-mode)
  :custom
  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil))

;; Visual Fill Column
;; Sometimes we want text to wrap before the window border.
(use-package visual-fill-column
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  :ghook
  ('visual-fill-column-mode-hook
   #'(visual-line-mode
      (lambda () (setq-local split-window-preferred-function
                             'visual-fill-column-split-window-sensibly)))))

;; Line Numbers
(use-package display-line-numbers
  :config
  (setq display-line-numbers-type 'visual)
  (setq-default display-line-numbers-width 3)
  (setq-default display-line-numbers-grow-only t)
  :hook (prog-mode  . display-line-numbers-mode))

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

;; frame title
(setq frame-title-format '("%b - Emacs")
      icon-title-format frame-title-format)

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'after-init-hook #'window-divider-mode)

;; always avoid GUI
(setq use-dialog-box nil)
;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
;; ...especially on linux
(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))

;; Favor vertical splits over horizontal ones. Screens are usually wide.
(setq split-width-threshold 160
      split-height-threshold nil)

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

(setq-default word-wrap t) ;; wrap at word boundaries

(provide '+ui)

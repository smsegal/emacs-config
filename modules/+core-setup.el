;;; +core-setup.el -*- lexical-binding: t; -*-

;;; Custom File
;; Load custom settings from a separate file instead of polluting the init file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

;; What OS?
(defvar IS-MAC (eq system-type 'darwin))
(defvar IS-LINUX (eq system-type 'gnu/linux))

;;; Garbage Collection
;; This package changes the behaviour of the garbage collector to act during idle time.
(use-package gcmh
  :hook (after-init . gcmh-mode))

;; Keep ~user-emacs-directory~ clean
;; We also set up ~recentf-mode~ since it relies on no-littering being loaded right before.
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Set up the path properly on GUI Emacs in environments that don't handle that for us
(use-package exec-path-from-shell
  :disabled
  :when (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (defun +init-tty-h ()
    (advice-remove #'tty-run-terminal-initialization #'ignore)
    (tty-run-terminal-initialization (selected-frame) nil t))
  (add-hook 'window-setup-hook #'+init-tty-h))

;; don't need to warn about legacy advices since its up to upstream
(setq ad-redefinition-action 'accept)

(setq auto-mode-case-fold nil);; don't bother with a second case-insensitive pass

;;; Performance and Security
;; These tweaks (mainly taken from doom emacs) enhance performance and provide (the appearance of?) security.
(setq enable-recursive-minibuffers t)

;; Credit: Doom Emacs
;; Contrary to what many Emacs users have in their configs, you really don't
;; need more than this to make UTF-8 the default coding system:
(setq locale-coding-system 'utf-8)

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq bidi-display-reordering 'left-to-right)
(setq bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; security (tls) tweaks
(setq gnutls-verify-error (not (getenv-internal "INSECURE"))
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

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)

;;; Server
;; Start the server unless it's already running.
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; Direnv
;; Direnv automatically adjusts the environment for you when entering a
;; directory with a ~.envrc~ file that contains the appropriate commands.
;; This should be at/near the bottom since you want this hook to be run
;; before others. Hooks are apparently a stack.

;; I'm hooking it to ~emacs-startup-hook~ instead of ~after-init-hook~, as
;; ~emacs-startup-hook~ runs after ~after-init-hook~
(use-package envrc
  :hook (emacs-startup . envrc-global-mode))

(provide '+core-setup)

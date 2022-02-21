;; -*- lexical-binding: t; -*-

;; Tree-sitter
;; Now, apparently this package is useful for a ton of different
;; things. I use it for the nicer syntax highlighting in supported languages.
(use-package tree-sitter)
(use-package tree-sitter-langs
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

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
   :predicate '(not (file-remote-p buffer-file-name))
   [remap format-all-buffer] #'apheleia-format-buffer))

;; Code Search
;; Automatically jump to definitions in different languages.
(use-package dumb-jump
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  :hook (xref-backend-functions . dumb-jump-xref-activate))

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
  (defun +compile-autoclose (buffer string)
    (require 'winner)
    (if (string-match "finished" string)
        (progn (bury-buffer "*compilation*")
               (winner-undo)
               (message "Build successful."))
      (message "Compilation exited abnormally: %s" string)))
  :init
  (setq compilation-scroll-output 'first-error)
  (setq compilation-finish-functions '+compile-autoclose)
  :general
  (:prefix-map '+code-map
   "c" #'compile
   "m" #'recompile)
  :ghook
  ('compilation-filter-hook #'+compile/apply-ansi-color-to-compilation-buffer-h))

;; Makefiles
;; I disabled indent-tabs-mode above, I need this enabled for makefiles.
(use-package make-mode
  :straight (:type built-in)
  :config
  (general-add-hook
   'makefile-mode-hook (lambda ()
                         (setq-local indent-tabs-mode +1))))

;; Emacs-Lisp
;; Small keybindings for evaling sexps.
(use-package emacs-lisp
  :straight (:type built-in)
  :general
  (+local-leader-def :keymaps 'emacs-lisp-mode-map
    "e" #'eval-last-sexp))

;; Custom indentation for lisp code.
(use-package +lisp-indent
  :straight nil
  :after lisp-mode)

;;; Small language modes not big enough for their own modules

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :custom (markdown-command "multimarkdown")
  :ghook
  ('(markdown-mode-hook gfm-mode-hook)
   #'visual-fill-column-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")

(use-package systemd)

;; arch PKGBUILDS
(use-package pkgbuild-mode
  :mode ("PKGBUILD" . pkgbuild-mode))

;; different git file modes
(use-package git-modes
  :mode ("/.dockerignore\\'" . gitignore-mode))

(use-package lua-mode)

(use-package fish-mode)

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode)
  :config
  (define-format-all-formatter dockfmt
    (:executable "dockfmt")
    (:install "go get github.com/jessfraz/dockfmt")
    (:languages "Dockerfile")
    (:format (format-all--buffer-easy executable "fmt" (buffer-file-name)))))

(provide '+langs)

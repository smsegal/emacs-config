;; -*- lexical-binding: t; -*-

;; LSP-Mode
;; We use LSP mode for pretty much everything with good support.
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
  ;; move to language specific areas where packages exist
  ('(sh-mode-hook) #'lsp-deferred)
  ('lsp-mode-hook '(lsp-headerline-breadcrumb-mode
                    lsp-modeline-diagnostics-mode
                    lsp-enable-which-key-integration))
  :init
  ;; use this for adding additional dirs to the ignore list from .dir-locals.el
  (defvar +lsp-ignore-additional-dirs nil)
  :general
  (general-nvmap
    :keymaps 'lsp-mode-map
    "," '(:keymap lsp-command-map))
  (general-def
    :prefix-map '+code-map
    :predicate 'lsp-mode
    "r" #'lsp-rename
    "a" #'lsp-execute-code-action)
  (:keymaps 'lsp-mode-map
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

(provide '+lsp)

;; -*- lexical-binding: t; -*-

;; LSP-Mode
;; We use LSP mode for pretty much everything with good support.
;; We set some of the more intrusive UI elements to nil.
;; Enable support for pyright language server.

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :ghook
  ;; move to language specific areas where packages exist
  ('(python-mode-hook sh-mode-hook) #'lsp-deferred)
  ('lsp-mode-hook '(lsp-modeline-diagnostics-mode
                    lsp-modeline-code-actions-mode
                    lsp-enable-which-key-integration))
  :init
  :config
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-completion-provider :capf)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-text-document-color nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-snippet t)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-diagnostics-provider :flycheck)
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
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-peek-enable t
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-ignore-duplicate t
        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-show-hover nil
        ;; REVIEW Temporarily disabled, due to immense slowness on every
        ;;        keypress. See emacs-lsp/lsp-ui#613
        lsp-ui-doc-enable nil)
  :general
  (:keymaps 'lsp-mode-map
   [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
   [remap xref-find-references] #'lsp-ui-peek-find-references)
  (:keymaps 'lsp-ui-peek-mode-map
   "j"   #'lsp-ui-peek--select-next
   "k"   #'lsp-ui-peek--select-prev
   "C-j" #'lsp-ui-peek--select-next-file
   "C-k" #'lsp-ui-peek--select-prev-file))

(use-package consult-lsp
  :general
  (:keymaps 'lsp-mode-map
   [remap xref-find-apropos] #'consult-lsp-symbols))

(provide '+lsp)

;; -*- lexical-binding: t; -*-

(require '+lsp)

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
(use-package jupyter
  :disabled
  :straight (:no-native-compile t)
  :commands jupyter-connect-repl jupyter-run-repl)

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

(provide '+python)

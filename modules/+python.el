;; -*- lexical-binding: t; -*-

(use-package lsp-pyright
  :after lsp
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; Python
;; The builtin package needs some simple tweaks to use ipython as the REPL.

;; python tweaks
(use-package python
  :straight (:type built-in)
  :init
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt -i"))

(use-package pip-requirements)

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

(use-package py-isort
  :general
  (general-nvmap
    :keymaps 'python-mode-map
    :prefix ","
    "io" #'py-isort-buffer))

(require 'crux)
(use-package python-black
  :general
  (:keymaps 'python-mode-map
   [remap format-all-buffer] (crux-with-region-or-buffer python-black-region)))

;; Jupyter Kernal and Notebook support
;; The ein package has really improved lately. In addition, the jupyter
;; kernel provides a pretty good experience for using it inside org-mode.
(use-package jupyter
  :commands (jupyter-connect-repl jupyter-run-repl jupyter-eval-region)
  :general
  (:keymaps 'jupyter-repl-mode-map
   :states '(insert normal)
   "C-j" #'jupyter-repl-history-next
   "C-k" #'jupyter-repl-history-previous)
  (:keymaps 'jupyter-repl-mode-map
   :states 'normal
   "j" #'jupyter-repl-history-next
   "k" #'jupyter-repl-history-previous))

;; trying lighter-weight alternative to ein
(use-package code-cells
  :commands (code-cells-command)
  :preface
  (defun +insert-code-cell ()
    (interactive)
    (code-cells-forward-cell 2)
    (insert "# %%\n\n# %%\n\n")
    (code-cells-backward-cell 2)
    (forward-line))
  :general
  (:keymaps 'python-mode-map
   "C-j" #'code-cells-forward-cell
   "C-k" #'code-cells-backward-cell
   "<C-return>" (code-cells-command 'python-shell-send-region :use-region :pulse)
   "<S-return>" '(code-cells-do
                  (pulse-momentary-highlight-region start end)
                  (python-shell-send-region start end)
                  (code-cells-forward-cell))))

(use-package ein
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


(use-package python-pytest
  :general
  (+local-leader-def :keymaps 'python-mode-map
    "t" #'python-pytest-dispatch))

(provide '+python)

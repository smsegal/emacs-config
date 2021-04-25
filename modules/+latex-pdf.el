;; -*- lexical-binding: t; -*-

(require '+lsp)

;; LaTeX
;; Set up company mode for autocompletion of references, citations, etc.
;; We also setup inline pdf viewing.

;; (use-package company-auctex)
;; (use-package company-reftex)
;; (use-package company-math)
;; (use-package company-bibtex)

(use-package lsp-latex
  :disabled
  :hook ((TeX-mode bibtex-mode) . lsp-deferred)
  :commands (lsp-latex-build)
  :config
  (setq lsp-latex-build-executable "tectonic")
  (setq lsp-latex-build-args '( "%f"
                                "--synctex"
                                "--keep-logs"
                                "--keep-intermediates"))
  (setq lsp-latex-forward-search-executable "emacsclient")
  (setq lsp-latex-forward-search-args
        '("--eval" "(lsp-latex-forward-search-with-pdf-tools \"%f\" \"%p\" \"%l\")")))

(use-package auctex
  :custom
  (setq TeX-master t)
  (setq TeX-parse-self t) ;; parse on load
  (setq TeX-auto-save t)  ;; parse on save
  ;; automatically insert braces after sub/superscript in math mode
  (setq TeX-electric-sub-and-superscript t)
  (setq bibtex-dialect 'biblatex)
  (setq bibtex-align-at-equal-sign t)
  (setq bibtex-text-indentation 20)
  (setq TeX-fold-type-list '(env math))
  :hook (TeX-mode . +latex-setup)
         ;; (TeX-mode . TeX-fold-mode))
  :mode ("\\.tex\\'" . LaTeX-mode)
  :preface
  (defun +latex-setup ()
    (turn-on-visual-line-mode)
    (visual-fill-column-mode +1)
    (unless word-wrap
      (toggle-word-wrap))
    ;; (TeX-fold-buffer)
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

;; insert \(\) instead of $$, plus same support for display-mode math
(use-package math-delimiters
  :straight (:host github :repo "oantolin/math-delimiters")
  :general
  (general-imap
    :keymaps 'LaTeX-mode-map
    "$" #'math-delimiters-insert))

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
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :hook
  (TeX-mode . auctex-latexmk-setup))

(use-package reftex
  :straight (:type built-in)
  :hook ((TeX-mode . reftex-mode)
         (LaTeX-mode . reftex-mode))
  :custom
  (reftex-plug-into-AUCTeX t)
  :config
  (setq reftex-cite-format
        '((?a . "\\autocite[]{%l}")
          (?b . "\\blockcquote[]{%l}{}")
          (?c . "\\cite[]{%l}")
          (?f . "\\footcite[]{%l}")
          (?n . "\\nocite{%l}")
          (?p . "\\parencite[]{%l}")
          (?s . "\\smartcite[]{%l}")
          (?t . "\\textcite[]{%l}")))
  (setq reftex-toc-split-windows-fraction 0.3))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  ;; :hook (pdf-view-mode . auto-revert-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  :general
  (+local-leader-def :keymaps 'pdf-view-mode-map
    "s" 'pdf-view-auto-slice-minor-mode)
  (:keymaps 'pdf-view-mode-map
   "q" #'kill-current-buffer))

(provide '+latex-pdf)

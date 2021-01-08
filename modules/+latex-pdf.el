;; -*- lexical-binding: t; -*-

(require '+lsp)

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
  :hook ((TeX-mode . lsp-deferred)
         (TeX-mode . +latex-setup)
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

(provide '+latex-pdf)

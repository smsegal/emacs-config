;; -*- lexical-binding: t; -*-

;; Snippets
;; We use yasnippet, as well as the snippets from doom.

;; We also have auto activating snippets, which will insert the specified
;; string /b/ when the string /a/ is written in the enabled mode.

(use-package yasnippet
  :hook ((prog-mode text-mode) . yas-global-mode)
  :general (:prefix-map '+insert-map
                        "s" 'yas-insert-snippet))
(use-package yasnippet-snippets
  :after yasnippet)
;; (use-package doom-snippets
;;   ;; :straight (:host github :repo "hlissner/doom-snippets")
;;   :after yasnippet)

(use-package auto-activating-snippets
  ;; :straight (:host github :repo "ymarco/auto-activating-snippets")
  :ghook ('TeX-mode-hook #'auto-activating-snippets-mode)
  :config
  (aas-set-snippets 'latex-mode
                    "On" "O(n)"
                    "ev0" "\\(\\text{EV}_{0}\\)"))

(provide '+snippets)

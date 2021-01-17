;; -*- lexical-binding: t; -*-

;; Spellcheck
;; Spell checking with flyspell and enchant.

(use-package flyspell
  :straight nil
  :defer t
  :init
  (setq flyspell-issue-welcome-flag nil)
  ;; Significantly speeds up flyspell, which would otherwise print
  ;; messages for every word when checking the entire buffer
  (setq flyspell-issue-message-flag nil)
  (setq ispell-program-name "enchant-2") ;; new spellcheck engine
  (setq ispell-dictionary "en_CA")
  :config
  (setq flyspell-mode-map nil);; I bind my own keys, and this interferes rudely with other packages.
  :ghook
  ('(org-mode-hook
     markdown-mode-hook
     TeX-mode-hook
     rst-mode-hook
     mu4e-compose-mode-hook
     message-mode-hook
     git-commit-mode-hook) #'flyspell-mode)
  ('prog-mode-hook #'flyspell-prog-mode))

;; Correct the word at the point with ~z=~.

(use-package flyspell-correct
  :after flyspell
  :commands flyspell-correct-previous
  :preface
  (defun +spell/add-word (word &optional scope)
    "Add WORD to your personal dictionary, within SCOPE.  SCOPE can be
  `buffer' or `session' to exclude words only from the current buffer or
  session. Otherwise, the addition is permanent."
    (interactive
     (list (progn (require 'flyspell)
                  (car (flyspell-get-word)))
           (cond ((equal current-prefix-arg '(16))
                  'session)
                 ((equal current-prefix-arg '(4))
                  'buffer))))
    (require 'flyspell)
    (cond
     ((null scope)
      (ispell-send-string (concat "*" word "\n"))
      (ispell-send-string "#\n")
      (flyspell-unhighlight-at (point))
      (setq ispell-pdict-modified-p '(t)))
     ((memq scope '(buffer session))
      (ispell-send-string (concat "@" word "\n"))
      (add-to-list 'ispell-buffer-session-localwords word)
      (or ispell-buffer-local-name ; session localwords might conflict
          (setq ispell-buffer-local-name (buffer-name)))
      (flyspell-unhighlight-at (point))
      (if (null ispell-pdict-modified-p)
          (setq ispell-pdict-modified-p
                (list ispell-pdict-modified-p)))
      (if (eq replace 'buffer)
          (ispell-add-per-file-word-list word))))
    (ispell-pdict-save t))
  :general
  ([remap ispell-word] #'flyspell-correct-wrapper)
  (general-nvmap "zg" #'+spell/add-word))

(use-package flyspell-correct-popup
  :disabled
  :after flyspell-correct
  :custom
  (flyspell-correct-interface #'flyspell-correct-popup)
  :general (:keymaps 'popup-menu-keymap [escape] #'keyboard-quit))

(provide '+spellcheck)

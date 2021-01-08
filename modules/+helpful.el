;; -*- lexical-binding: t; -*-

;; A Better Help Buffer
;; We also set apropos to search as much as possible.
(use-package helpful
  :general
  (:prefix-map 'help-map
               "f" #'helpful-callable
               "v" #'helpful-variable
               "k" #'helpful-key
               "h" #'helpful-at-point))

(use-package help
  :straight (:type built-in)
  :config
  (require 'apropos)
  ;; this is taken from
  ;; https://github.com/Wilfred/helpful/issues/25#issuecomment-738347468
  ;; to enable helpful buffers from apropos
  (let ((do-function (lambda (button)
                       (helpful-function (button-get button 'apropos-symbol))))
        (do-variable (lambda (button)
                       (helpful-variable (button-get button 'apropos-symbol)))))
    ;; :supertype only takes effect statically, at the time of
    ;; definition, so we can in fact redefine a button with itself
    ;; as its supertype
    (define-button-type 'apropos-function :supertype 'apropos-function 'action do-function)
    (define-button-type 'apropos-macro :supertype 'apropos-macro 'action do-function)
    (define-button-type 'apropos-command :supertype 'apropos-command 'action do-function)
    (define-button-type 'apropos-variable :supertype 'apropos-variable 'action do-variable)
    (define-button-type 'apropos-user-option :supertype 'apropos-user-option 'action do-variable))
  :custom
  (apropos-do-all t))

(provide '+helpful)

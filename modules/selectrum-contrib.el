(require 'selectrum)
(require 'cl-lib)

(defvar selectrum-swiper-history nil "Submission history for `selectrum-swiper'.")
(autoload 'selectrum-read "selectrum")
(defun selectrum-swiper ()
  "Search for a matching line and jump to the beginning of its text.  Obeys narrowing."
  (interactive)
  (let* ((selectrum-should-sort-p nil)
         ;; Get the current line number for determining the travel distance.
         (current-line-number (line-number-at-pos (point) t))

         (default-cand-and-line-choices
           (cl-loop
            with minimum-line-number = (line-number-at-pos (point-min) t)
            with buffer-text-lines = (split-string (buffer-string) "\n")
            with number-format = (concat
                                  "L%0"
                                  (number-to-string
                                   (length (number-to-string
                                            (length buffer-text-lines))))
                                  "d: ")

            with closest-candidate = nil
            with distance-to-current-line = nil
            with smallest-distance-to-current-line = most-positive-fixnum

            with formatted-line = nil
            with formatted-lines = nil

            for txt in buffer-text-lines
            for num = minimum-line-number then (1+ num)
            unless (string-empty-p txt) ; Just skip empty lines.
            do
            (setq formatted-line (propertize
                                  txt
                                  'selectrum-candidate-display-prefix
                                  (propertize
                                   (format number-format num)
                                   'face 'completions-annotations)
                                  'line-num num)
                  distance-to-current-line (abs (- current-line-number num)))
            (push formatted-line formatted-lines)
            (when (< distance-to-current-line
                     smallest-distance-to-current-line)
              (setq smallest-distance-to-current-line distance-to-current-line
                    closest-candidate formatted-line))
            finally return (cons closest-candidate
                                 (nreverse formatted-lines))))
         (default-cand (car default-cand-and-line-choices))
         (line-choices (cdr default-cand-and-line-choices))

         ;; Get the matching line.
         (chosen-line (selectrum-read "Jump to matching line: "
                                      line-choices
                                      :default-candidate default-cand
                                      :history 'selectrum-swiper-history
                                      :require-match t
                                      :no-move-default-candidate t))

         (chosen-line-number (get-text-property 0 'line-num chosen-line)))
    
    (push-mark (point) t)
    (forward-line (- chosen-line-number current-line-number))
    (beginning-of-line-text 1)
    (view-recenter)))

(defun org:show-subtree-headlines ()
  "Show headlines surrounding point."
  (save-excursion
    (let ((points nil) (count 0))
      (unless (org-at-heading-p) (org-back-to-heading t))
      (push (point) points)
      (while (org-up-heading-safe)
        (push (point) points))
      (dolist (point points)
        (goto-char point)
        (when (org:heading-folded-p)
          (outline-toggle-children))))))

(defun selectrum:reveal-if-in-org-folds (orig-fn &rest args)
  (prog1 (apply orig-fn args)
    (when (eq major-mode 'org-mode)
      (org:show-subtree-headlines))))

(advice-add #'selectrum-swiper :around #'selectrum:reveal-if-in-org-folds)

(defun +yank-pop (&optional arg)
  "Call `yank-pop' with ARG when appropriate, or offer completion."
  (interactive "*P")
 (if arg (yank-pop arg)
   (let* ((old-last-command last-command)
          (selectrum-should-sort-p nil)
          (enable-recursive-minibuffers t)
          (text (completing-read
                 "Yank: "
                 (cl-remove-duplicates
                  kill-ring :test #'string= :from-end t)
                 nil t nil nil))
          ;; Find `text' in `kill-ring'.
          (pos (cl-position text kill-ring :test #'string=))
          ;; Translate relative to `kill-ring-yank-pointer'.
          (n (+ pos (length kill-ring-yank-pointer))))
     (unless (string= text (current-kill n t))
       (error "Could not setup for `current-kill'"))
     ;; Restore `last-command' over Selectrum commands.
     (setq last-command old-last-command)
     ;; Delegate to `yank-pop' if appropriate or just insert.
     (if (eq last-command 'yank)
         (yank-pop n) (insert-for-yank text)))))



(provide 'selectrum-contrib)


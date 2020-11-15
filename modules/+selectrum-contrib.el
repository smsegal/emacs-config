;;; +selectrum-contrib.el -*- lexical-binding: t; -*-

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
    (recenter)))

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
  "Paste a previously killed string.
With just \\[universal-argument] as ARG, put point at beginning,
and mark at end.  Otherwise, put point at the end, and mark at
the beginning without activating it.

This is like `yank-pop'.  The differences are:

- This let you manually choose a candidate to paste.

- This doesn't delete the text just pasted if the previous
  command is `yank'."
  (interactive "P")
  (let* ((selectrum-should-sort-p nil)
         (text nil))
    (setq text
          (completing-read "Yank: "
                           (cl-remove-duplicates
                            kill-ring :test #'equal :from-end t)
                           nil 'require-match))
    (unless (eq last-command 'yank)
      (push-mark))
    (setq last-command 'yank)
    (setq yank-window-start (window-start))
    (when (and delete-selection-mode (use-region-p))
      (delete-region (region-beginning) (region-end)))
    (insert-for-yank text)
    (if (consp arg)
        (goto-char (prog1 (mark t)
                     (set-marker (mark-marker) (point) (current-buffer)))))))



(require 'all-the-icons)

;;;###autoload
(defun +selectrum-candidate-highlight-with-icons-function (input candidates)
  "Default value of `selectrum-highlight-candidates-function'.
Highlight the substring match with
`selectrum-primary-highlight'. INPUT is a string, CANDIDATES is a
list of strings."
  (let ((regexp (regexp-quote input)))
    (save-match-data
      (mapcar
       (lambda (candidate)
         (when (string-match regexp candidate)
           (setq candidate (copy-sequence candidate))
           (put-text-property
            (match-beginning 0) (match-end 0)
            'face 'selectrum-primary-highlight
            candidate))
         (concat (all-the-icons-icon-for-file candidate) candidate))
       candidates))))


(eval-and-compile
  (require 'dom)
  (require 'xdg))

(declare-function dom-attr "dom")
(declare-function dom-by-tag "dom")
(defvar recentf-list)

(defun selectrum--recentf-get-xdg ()
  (let ((file-of-recent-files
         (expand-file-name "recently-used.xbel" (xdg-data-home))))
    (if (not (file-readable-p file-of-recent-files))
        (user-error "List of XDG recent files not found.")
      (delq
       nil
       (mapcar
        (lambda (bookmark-node)
          (let ((local-path
                 (string-remove-prefix "file://"
                                       (dom-attr bookmark-node
                                                 'href))))
            (when local-path
              (let ((full-file-name
                     (decode-coding-string
                      (url-unhex-string local-path)
                      'utf-8)))
                (when (file-exists-p full-file-name)
                  full-file-name)))))
        (nreverse (dom-by-tag (with-temp-buffer
                                (insert-file-contents file-of-recent-files)
                                (libxml-parse-xml-region (point-min)
                                                         (point-max)))
                              'bookmark)))))))

(defun selectrum-recentf ()
  "Open a recently used file (including XDG)."
  (interactive)
  (let* ((selectrum-should-sort-p nil)
         (all-recent-files
          (append (mapcar #'substring-no-properties
                          recentf-list)
                  (seq-filter #'recentf-include-p
                              (selectrum--recentf-get-xdg))))
         (files-with-times
          (mapcar (lambda (file)
                    (cons file
                          ;; Use modification time, since getting file access time
                          ;; seems to count as accessing the file, ruining future uses.
                          (file-attribute-modification-time (file-attributes file))))
                  all-recent-files))
         (sorted-files
          (delete-dups (sort files-with-times
                             (lambda (file1 file2)
                               ;; Want existing most recent local files first.
                               (cond ((or (not (file-exists-p (car file1)))
                                          (file-remote-p (car file1)))
                                      nil)
                                     ((or (not (file-exists-p (car file2)))
                                          (file-remote-p (car file2)))
                                      t)
                                     (t (time-less-p (cdr file2)
                                                     (cdr file1))))))))
         (propertized-files (mapcar (lambda (f)
                                      (propertize (abbreviate-file-name (car f))
                                                  'selectrum-candidate-display-right-margin
                                                  (propertize (current-time-string (cdr f))
                                                              'face 'completions-annotations)))
                                    sorted-files)))
    (find-file (completing-read "Select recent file: " propertized-files
                                nil t nil 'file-name-history
                                (car propertized-files)))))


(defvar selectrum-imenu+ nil)
(defun +selectrum-imenu ()
  "Choose from `imenu' just like `counsel-imenu'."
  (interactive)
  (require 'imenu)
  (let* ((selectrum-should-sort-p nil)
         (candidates (let* ((imenu-auto-rescan t)
                            (items (imenu--make-index-alist t)))
                       ;; remove *Rescan*
                       (setq items (delete (assoc "*Rescan*" items) items))
                       ;; special mode
                       (when (eq major-mode 'emacs-lisp-mode)
                         (let ((fns (cl-remove-if #'listp items :key #'cdr)))
                           (if fns
                               (setq items (nconc (cl-remove-if #'nlistp items :key #'cdr)
                                                  `(("Functions" ,@fns)))))))
                       ;; refine
                       (cl-labels
                           ((get-candidates (alist &optional prefix)
                                            (cl-mapcan (lambda (elm)
                                                         (if (imenu--subalist-p elm)
                                                             (get-candidates
                                                              (cl-loop for (e . v) in (cdr elm)
                                                                       collect (cons e (if (integerp v) (copy-marker v) v)))
                                                              (concat prefix (if prefix ".") (car elm)))
                                                           (let ((key (concat
                                                                       (if prefix
                                                                           (concat (propertize prefix 'face 'font-lock-keyword-face) ": "))
                                                                       (car elm))))
                                                             (list
                                                              (cons key (cons key (if (overlayp (cdr elm))
                                                                                      (overlay-start (cdr elm))
                                                                                    (cdr elm))))))))
                                                       alist)))
                         (setq items (get-candidates items)))
                       ;; sort
                       (cl-sort items #'string< :key #'car)))
         (cand (completing-read "Imenu: " (mapcar #'car candidates) nil t nil selectrum-imenu+)))
    (imenu (cdr (cl-find cand candidates :test #'string= :key #'car)))))

(provide '+selectrum-contrib)

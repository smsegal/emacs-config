;;; +selectrum-contrib.el -*- lexical-binding: t; -*-

(require 'selectrum)
(require 'cl-lib)

(autoload 'selectrum-read "selectrum")

(defvar selectrum-swiper-history nil "Submission history for `selectrum-swiper'.")
(defun selectrum-swiper ()
  "Search for a matching line and jump to the beginning of its text.
The default candidate is a non-empty line closest to point.
This command obeys narrowing."
  (interactive)
  (let ((selectrum-should-sort-p nil)
        ;; Get the current line number for determining the travel distance.
        (current-line-number (line-number-at-pos (point) t)))
    (cl-destructuring-bind (default-candidate formatted-candidates)
        (cl-loop
         with buffer-lines = (split-string (buffer-string) "\n")
         with number-format = (concat "L%0"
                                      (number-to-string
                                       (length (number-to-string
                                                (length buffer-lines))))
                                      "d: ")

         with formatted-candidates = nil
         for line-text in buffer-lines
         for line-num = (line-number-at-pos (point-min) t) then (1+ line-num)

         with default-candidate = nil
         with prev-distance-to-default-cand = 1.0e+INF ; This updated later.
         for distance-to-default-cand = (abs (- current-line-number line-num))

         unless (string-empty-p line-text) ; Just skip empty lines.
         do
         ;; Find if we’ve started to move away from the current line.
         (when (null default-candidate)
           (when (> distance-to-default-cand
                    prev-distance-to-default-cand)
             (setq default-candidate (cl-first formatted-candidates)))
           (setq prev-distance-to-default-cand distance-to-default-cand))

         ;; Format current line and collect candidate.
         (push (propertize line-text
                           'selectrum-candidate-display-prefix
                           (propertize (format number-format line-num)
                                       'face 'completions-annotations)
                           'line-num line-num)
               formatted-candidates)

         finally return (list default-candidate
                              (nreverse formatted-candidates)))
      (let ((chosen-line-number
             (get-text-property
              0 'line-num
              (selectrum-read "Jump to matching line: "
                              formatted-candidates
                              :default-candidate default-candidate
                              :history 'selectrum-swiper-history
                              :require-match t
                              :no-move-default-candidate t))))
        (push-mark (point) t)
        (forward-line (- chosen-line-number current-line-number))
        (beginning-of-line-text 1)
        (recenter)))))

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

(defvar selectrum-outline-history nil
  "History of chosen headings for `selectrum-outline'.")

(defcustom selectrum-outline-formats
  ;; Groups: (1) level determinant, (2) heading text.
  ;; The top level is 0, for a zero-length determinant.
  '((emacs-lisp-mode
     . "^;;;\\(?1:;*\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
    (markdown-mode
     . "^#\\(?1:#*\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
    (outline-mode
     . "^\\*\\(?1:\\**\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
    ;; For Org, see also `org-goto'.
    (org-mode
     . "^\\*\\(?1:\\**\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
    (python-mode
     . "^##\\(?1:\\**\\|#*\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'"))
  "An alist of regexps to use for identifying outline headings, one for each major mode.

The `car' of an item in the list should be a symbol of the major mode.
The `cdr' should be a regular expression with two required match groups:
1. Match group 1, whose length determines the outline level of that heading.
   For best formatting, the top level should be level 0 for zero length.
2. Match group 2, which is the actual heading text.

A heading is assumed to be on only one line. "
  :group 'selectrum
  :type '(alist
          :key-type (symbol :tag "Major mode symbol")
          :value-type (string :tag "Regexp")))

;;;###autoload
(defun selectrum-outline ()
  "Jump to a heading.  Regexps are pre-defined.  Obeys narrowing."
  (interactive)
  ;; Signal a `user-error' if we don't have a regexp for this major mode.
  (if-let ((heading-regexp (alist-get major-mode selectrum-outline-formats)))
      (let ((selectrum-should-sort-p nil) ; Headings should stay in order of appearance.
            ;; Get the basic information of each heading in the accessible
            ;; portion of the buffer.
            (buffer-lines (split-string (buffer-string) "\n"))
            (line-number 0)
            (line-number-format)

            ;; Finding the default heading
            (default-heading)
            (current-line-number (line-number-at-pos (point)))

            ;; Keeping track of the tree.
            (backwards-prefix-list)
            (prev-heading-text)
            (prev-heading-level)

            ;; Backwards result of the `dolist'. Will `nreverse'.
            (formatted-headings))

        (setq line-number-format
              (concat "L%0"
                      (number-to-string
                       (length (number-to-string (length buffer-lines))))
                      "d: "))

        (save-match-data
          (dolist (text-line buffer-lines)
            ;; Increment line number when moving to next.
            (cl-incf line-number)
            (when (string-match heading-regexp text-line)
              (let ((heading-text (match-string-no-properties 2 text-line))
                    (heading-level
                     (length (match-string-no-properties 1 text-line)))
                    (formatted-heading))

                ;; Want to make sure this has a correct value.
                (when (null prev-heading-level)
                  (setq prev-heading-level heading-level))

                ;; Decide whether to update the prefix list and the previous
                ;; heading level.
                (cond
                 ;; If we've moved to a greater level (further down the tree),
                 ;; add the previous heading to the heading prefix list so
                 ;; that we can prepend it to the current heading when
                 ;; formatting.
                 ((> heading-level prev-heading-level)
                  (setq backwards-prefix-list (cons prev-heading-text
                                                    backwards-prefix-list)
                        prev-heading-level heading-level))
                 ;; Otherwise, if we've moved to a lower level (higher up the
                 ;; tree), and need to remove the most recently added prefix
                 ;; from the list (i.e., go from '(c b a) back to '(b a)).
                 ((< heading-level prev-heading-level)
                  (setq backwards-prefix-list (last backwards-prefix-list
                                                    heading-level)
                        prev-heading-level heading-level))
                 ;; Otherwise, do nothing.
                 (t nil))

                ;; Regardless of what happens, update the previous heading text.
                (setq prev-heading-text heading-text)

                ;; Decide whether the previous formatted heading was the
                ;; default.
                (when (and (null default-heading)
                           (> line-number current-line-number))
                  (setq default-heading (car formatted-headings)))

                ;; Finally, add to list of formatted headings.
                ;; Create heading of form "L#: a/b/c" as:
                ;; - having a text property holding the line number
                ;; - prepended with a formatted line number,
                ;;   with the face `completions-annotations'.
                (push (propertize
                       (concat (string-join (reverse backwards-prefix-list) "/")
                               (and backwards-prefix-list "/")
                               heading-text)
                       'line-number line-number
                       'selectrum-candidate-display-prefix
                       (propertize
                        (format line-number-format line-number)
                        'face 'completions-annotations))
                      formatted-headings)))))

        ;; Now that candidates formatted, select from candidates.
        (let ((chosen-heading
               (selectrum-read "Jump to heading: "
                               (nreverse formatted-headings)
                               :default-candidate default-heading
                               :history 'selectrum-outline-history
                               :require-match t
                               :no-move-default-candidate t)))
          ;; Push mark, in case we want to return to current location.  This
          ;; needs to happen /after/ the user has made it clear that they want
          ;; to go somewhere.
          (push-mark (point) t)
          ;; Move to beginning of chosen line.
          (forward-line (- (get-text-property 0 'line-number chosen-heading)
                           current-line-number))
          (beginning-of-line-text 1)))
    (user-error "selectrum-outline: No headings defined for %s." major-mode)))


;;;###autoload
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


(autoload 'ffap-file-at-point "ffap")

(add-hook 'completion-at-point-functions
          (defun complete-path-at-point+ ()
            (let ((fn (ffap-file-at-point))
                  (fap (thing-at-point 'filename)))
              (when (and (or fn
                             (equal "/" fap))
                         (save-excursion
                           (search-backward fap (line-beginning-position) t)))
                (list (match-beginning 0)
                      (match-end 0)
                      #'completion-file-name-table)))) 'append)

(defun selectrum-restrict-to-matches ()
  (interactive)
  (setq selectrum--preprocessed-candidates
        selectrum--refined-candidates)
  (delete-minibuffer-contents))

(provide '+selectrum-contrib)

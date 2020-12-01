;;; +selectrum-contrib.el -*- lexical-binding: t; -*-

(require 'selectrum)
(require 'cl-lib)
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

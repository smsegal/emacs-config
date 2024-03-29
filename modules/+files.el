;; -*- lexical-binding: t; -*-

;; recent files, taking into consideration the no-littering
(use-package recentf-mode
  :straight (:type built-in)
  :after no-littering
  :hook (after-init . recentf-mode)
  :custom
  (recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (run-at-time nil (* 5 60) 'recentf-save-list))

;; Dired and File Management
(use-package dired
  :straight (:type built-in)
  :commands (dired dired-jump)
  :init
  (when IS-MAC
    (setq insert-directory-program "/usr/local/bin/gls"))
  ;; (setq dired-listing-switches "-agho --group-directories-first")
  (setq dired-dwim-target t)
  (setq dired-delete-by-moving-to-trash t)
  :general
  (:prefix-map '+open-map
   "-" #'dired-jump)
  (general-nmap :keymaps 'dired-mode-map
    "h" #'dired-up-directory
    "l" #'dired-find-file))

;; This provides library async commands, but also dired-async mode
;; (use-package dired-async
;;   :straight (async)
;;   :commands (dired-async-mode)
;;   :hook (dired-mode . dired-async-mode))

;; fancy font-locking in dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; collapses folders that only contain one child
(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(use-package dired+
  :commands (diredp-do-apply-to-marked))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package super-save
  :custom (super-save-auto-save-when-idle t)
  :hook (after-init . super-save-mode))

(use-package +copy-file-name
  :straight nil
  :preface
  (defun +copy-file-name-to-clipboard ()
    "Copy the current buffer file name to the clipboard."
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
      (when filename
        (kill-new filename)
        (message "Copied buffer file name '%s' to the clipboard." filename))))
  :general
  (:prefix-map '+file-map
   "c" '(+copy-file-name-to-clipboard :which-key "copy filename")))

;; Crux
;; Crux is a selection of useful functions.
(use-package crux
  :general
  (:prefix-map '+file-map
   "E" #'crux-sudo-edit
   "D" #'crux-delete-file-and-buffer
   "p" #'crux-find-user-init-file
   "C" #'crux-copy-file-preserve-attributes
   "R" #'crux-rename-file-and-buffer)
  (:prefix-map '+open-map
   "w" #'crux-open-with))

(defun +find-init-file-here ()
  (interactive)
  (find-file user-init-file))

(general-def :prefix-map '+file-map
  "P" #'+find-init-file-here)

(setq delete-old-versions t ;;remove old backup files
      version-control t
      tramp-backup-directory-alist backup-directory-alist)

(defun +guess-file-mode ()
  "inspired by doom again, when saving a file with no mode, try and guess it afterwards"
  (when (eq major-mode 'fundamental-mode)
    (let ((buffer (or (buffer-base-buffer) (current-buffer))))
      (and (buffer-file-name buffer)
           (eq buffer (window-buffer (selected-window))) ;;buffer must be visible
           (set-auto-mode)))))
(add-hook 'after-save-hook #'+guess-file-mode)

(provide '+files)

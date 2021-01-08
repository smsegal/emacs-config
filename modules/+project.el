;; -*- lexical-binding: t; -*-


;; Project.el
;; This is built in and simpler than projectile.

(use-package project
  :preface
  ;; (cl-defmethod project-root ((project (head local)))
  ;;   (cdr project))

  ;; (defun +_project-files-in-directory (dir)
  ;;   "Use `fd' to list files in DIR."
  ;;   (let* ((default-directory dir)
  ;;          (localdir (file-local-name (expand-file-name dir)))
  ;;          (command (format "fd -t f -0 . %s" localdir)))
  ;;     (project--remote-file-names
  ;;      (sort (split-string (shell-command-to-string command) "\0" t)
  ;;            #'string<))))

  ;; (cl-defmethod project-files ((project (head local)) &optional dirs)
  ;;   "Override `project-files' to use `fd' in local projects."
  ;;   (mapcan #'+_project-files-in-directory
  ;;           (or dirs (list (project-root project)))))
  :config
  (setq project-switch-commands
   '((?f "File" project-find-file)
     (?g "Grep" project-find-regexp)
     (?d "Dired" project-dired)
     (?b "Buffer" project-switch-to-buffer)
     (?q "Query replace" project-query-replace-regexp)
     (?m "Magit" magit-status)
     (?v "VC dir" project-vc-dir)))
  :general
  (+leader-def
    "p" '(:keymap project-prefix-map
          :package project
          :which-key "projects")))

;;;###autoload
(defun +get-project-root ()
  "Get current project root directory."
  (interactive)
  (cdr (project-current)))

(provide '+project)

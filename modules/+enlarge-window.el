(defvar +window-enlargened nil)

;;;###autoload
(defun +window-enlargen ()
  "Enlargen the current window to focus on this one. Does not close other
windows. Activate again to undo."
  (interactive)
  (setq +window-enlargened
        (if (and +window-enlargened
                 (assq ?_ register-alist))
            (ignore (ignore-errors (jump-to-register ?_)))
          (window-configuration-to-register ?_)
          (let* ((window (selected-window))
                 (dedicated-p (window-dedicated-p window))
                 (preserved-p (window-parameter window 'window-preserved-size))
                 (ignore-window-parameters t))
            (unwind-protect
                (progn
                  (when dedicated-p
                    (set-window-dedicated-p window nil))
                  (when preserved-p
                    (set-window-parameter window 'window-preserved-size nil))
                  (maximize-window window))
              (set-window-dedicated-p window dedicated-p)
              (when preserved-p
                (set-window-parameter window 'window-preserved-size preserved-p)))
            t))))

(provide '+enlarge-window)

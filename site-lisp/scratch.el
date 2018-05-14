(defun foo ()
  (interactive)
  (message "fooooooo"))

(defun bar ()
  (interactive)
  (message "barrrrrr"))

(defun dwim-at-point ()
  "If there's an active selection, return that. Otherwise, get
the symbol at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (if (symbol-at-point)
        (symbol-name (symbol-at-point)))))

(defun gbk-encoding ()
  (interactive)
  (revert-buffer-with-coding-system 'gbk))

(defun glen-ecs ()
  (interactive)
  (setq flow-control-c-s-replacement ?\035)
  (enable-flow-control))

(defun get-svn-root ()
  "Return svn root directory if in SVN project or NIL if not"
  (interactive)
  (let ((svn-dir ""))
    (catch 'ret
      (when (not (file-directory-p (concat default-directory "/.svn")))
        (message "Not in a SVN project, use C-u to force force pick files by marking")
        (throw 'ret nil))
      (setq svn-dir (expand-file-name default-directory))
      ;; root svn
      (while (file-directory-p (expand-file-name (concat svn-dir "/../.svn")))
        (setq svn-dir (expand-file-name (concat svn-dir "/.."))))

      ;; delete trainling "/"
      (when (and (> (length svn-dir) 2)
                 (string-equal "/" (substring svn-dir -1))
                 (setq svn-dir (substring svn-dir 0 -1)))))
    svn-dir))

(defun svn-diff ()
  (interactive)
  (shell-command "svn diff" "*svn-diff*")
  (save-excursion
    (pop-to-buffer (get-buffer "*svn-diff*"))
    (diff-mode)))

(provide 'scratch)

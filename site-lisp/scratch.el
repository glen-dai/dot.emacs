(defun foo ()
  (interactive)
  (message "fooooooo"))

(defun dwim-at-point ()
  "If there's an active selection, return that. Otherwise, get
the symbol at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (if (symbol-at-point)
        (symbol-name (symbol-at-point)))))

(defun qq-cmd (arg)
  (interactive "p")
  ;; (message arg)
  (let ((sym nil))
    (if (eq 4 arg)
        (setq sym (read-from-minibuffer "Cmd to query:" "0x"))
      (setq sym (dwim-at-point)))
    (shell-command
     (format "grep -i %s %s" sym "/home/glendai/conn/doc/CS协议命令号列表.txt"))))

(defun gbk-encoding ()
  (interactive)
  (revert-buffer-with-coding-system 'gbk))

(defun glen-mark ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (when (search-forward "printf(\"glen glen glen\\n\");\n" nil t)
      (return))
    (when (and (search-forward "main(" nil t)
               (search-forward "DaemonInit(" nil t))
      (back-to-indentation)
      (c-context-line-break)
      (previous-line)
      (c-indent-line-or-region)
      (insert "\n")
      (let ((n 4))
        (while (> n 0)
          (setq n (- n 1))
          (c-indent-line-or-region)
          (insert "printf(\"glen glen glen\\n\");\n"))))))

(defun glen-conn-guess-coding ()
  (interactive)
  (cond
   ((string-equal "icq_finger.c" (buffer-name))
    (revert-buffer-with-coding-system 'chinese-hz))
   (t (message "No coding system"))))

(defun glen-ecs ()
  (interactive)
  (setq flow-control-c-s-replacement ?\035)
  (enable-flow-control))

(defun show-attr ()
  (interactive)
  (let ((symbol nil))
    (if (use-region-p)
        (setq symbol (buffer-substring-no-properties (region-beginning) (region-end)))
      (setq symbol (thing-at-point 'symbol)))
    (call-interactively
     #'(lambda ()
         (interactive)
         (shell-command (format "cli.py %s" symbol))))))

(defalias 'a 'show-attr)

(setq rsync-def-cmd "")
(setq rsync-svr-addr "c2_udpconn_dev_x64")

(require 'counsel)
;;;###autoload
(defun rsync-to-remote (&optional initial-input)
  "Rsync file with file/dir choose by counsel`'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
  (interactive)
  (ivy-read "[rsync to remote] Local file/dir: " 'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :initial-input initial-input
            :action
            (lambda (x)
              (with-ivy-window
                (let ((find-file-hook (if (and
                                           counsel-find-file-speedup-remote
                                           (file-remote-p ivy--directory))
                                          nil
                                        find-file-hook))
                      (remote-dir-or-file 
                       (read-from-minibuffer "[rsync to remote] Remote dir(default /home/glendai): ")))
                  (async-shell-command
                   (format
                    "RSYNC_PASSWORD=\"conn2.0\" rsync -vzcCrLptgoI --port=28000 %s glendai@%s::glendai/%s "
                    (expand-file-name x ivy--directory) rsync-svr-addr remote-dir-or-file) "*run-rsync*")
                  ;; (find-file (expand-file-name x ivy--directory))
                  )))
            :preselect (when counsel-find-file-at-point
                         (require 'ffap)
                         (let ((f (ffap-guesser)))
                           (when f (expand-file-name f))))
            :require-match 'confirm-after-completion
            :history 'file-name-history
            :keymap counsel-find-file-map
            :caller 'counsel-find-file))

;;;###autoload
(defun rsync-from-remote (&optional initial-input)
  "Rsync file with file/dir choose by counsel`'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
  (interactive)
  (let ((remote-dir-or-file (read-from-minibuffer "[rsync from remote] Remote file/dir: ")))
    (ivy-read "[rsync from remote] Local dir/file: " 'read-file-name-internal
              :matcher #'counsel--find-file-matcher
              :initial-input initial-input
              :action
              (lambda (x)
                (with-ivy-window
                  (let ((find-file-hook (if (and
                                             counsel-find-file-speedup-remote
                                             (file-remote-p ivy--directory))
                                            nil
                                          find-file-hook)))
                    (async-shell-command
                     (format
                      "RSYNC_PASSWORD=\"conn2.0\" rsync -vzcCrLptgoI --port=28000 glendai@%s::glendai/%s %s"
                      rsync-svr-addr remote-dir-or-file (expand-file-name x ivy--directory)) "*run-rsync*")
                    ;; (find-file (expand-file-name x ivy--directory))
                    )))
              :preselect (when counsel-find-file-at-point
                           (require 'ffap)
                           (let ((f (ffap-guesser)))
                             (when f (expand-file-name f))))
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'counsel-find-file)))

(defun rsync-cmd (&optional arg)
  (interactive "p")
  (if (eq arg 4)
      (rsync-from-remote)
    (rsync-to-remote)))

(defun save-utf8 ()
  (interactive)
  (revert-buffer-with-coding-system 'utf-8)
  (save-buffer))

(defun make-then-rsync ()
  (interactive)
  (call-interactively 'compile)
  (call-interactively 'run-rsync))

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

(defun qdiao-build ()
  (interactive)
  (let ((root-dir (get-svn-root))
        (def-dir-saved default-directory))
    (if (or (not root-dir)
            (not (file-directory-p (concat root-dir "/qualitytest"))))
        (message "Not in qdiao project dir")
      (setq default-directory (concat root-dir "/qualitytest"))
      (call-interactively 'compile)
      (setq default-directory def-dir-saved))))

(defun qdiao-rsync ()
  (interactive)
  (let ((root-dir (get-svn-root))
        (def-dir-saved default-directory))
    (if (or (not root-dir)
            (not (file-directory-p (concat root-dir "/qualitytest"))))
        (message "Not in qdiao project dir")
      (setq default-directory (concat root-dir "/qualitytest"))
      (call-interactively 'run-rsync)
      (setq default-directory def-dir-saved))))

(defun svn-diff ()
  (interactive)
  (shell-command "svn diff" "*svn-diff*")
  (save-excursion
    (pop-to-buffer (get-buffer "*svn-diff*"))
    (diff-mode)))

(defalias 'qb 'qdiao-build)
(defalias 'qr 'qdiao-rsync)
(defalias 'ms 'make-then-rsync)

(provide 'scratch)

;; * prelude
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defconst emacs-start-time (current-time))
(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed)))
          t)


(require 'use-package)

(defvar ctl-period-map)
(define-prefix-command 'ctl-z-map)
(global-set-key (kbd "C-z") 'ctl-z-map)


(defun enable-outline-for-init-el ()
  (interactive)
  (when (equal (expand-file-name "~/.emacs.d/init.el")
               (buffer-file-name))
    (setq outline-regexp ";; [*]+ ")
    (outline-minor-mode)
    (outline-hide-body)))

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(add-hook 'emacs-lisp-mode-hook 'enable-outline-for-init-el)

(defun reload-major-mode ()
  (interactive)
  (setq old-mode major-mode)
  (text-mode)
  (call-interactively old-mode)
  (message "revert major mode done ..."))

(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path "~/.emacs.d/setup")
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/defuns")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(require 'diminish)
(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  :diminish ivy-mode
  :bind
  ("C-c /" . swiper)
  ("M-x" . counsel-M-x)
  ("C-C C-r" . ivy-resume)
  ("C-x C-f" . counsel-find-file)
  )

(ivy-mode)

(use-package noutline
  :bind
  (("C-c C-c" . outline-toggle-children)
   ("C-c C-n" . outline-next-visible-heading)
   ("C-c C-p" . outline-previous-visible-heading)
   ("C-c C-b" . hide-body)
   ("C-c C-a" . show-all)
   ("C-c C-p" . outline-previous-visible-heading)))

;; * global settings
(setq-default
 blink-cursor-delay 0
 inhibit-startup-screen 1
 fill-column 80
 blink-cursor-interval 0.4
 echo-keystrokes 0.15
 scroll-preserve-screen-position 1
 scroll-conservatively 5
 scroll-step 2
 indicate-empty-lines t
 buffers-menu-max-size 30
 case-fold-search t
 compilation-scroll-output t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 grep-highlight-matches t
 grep-scroll-output t
 indent-tabs-mode nil
 make-backup-files nil
 auto-save-default nil
 mouse-yank-at-point t
 highlight-nonselected-windows t
 set-mark-command-repeat-pop t
 show-trailing-whitespace nil
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 x-stretch-cursor t
 completion-cycle-threshold t
 tab-width 4
 make-backup-files nil
 major-mode 'text-mode
 enable-recursive-minibuffers t
 visible-bell nil)

(cond
 ((eq window-system 'ns)
  (progn
    (global-set-key (kbd "C-x C-m") 'toggle-frame-maximized)
    (when (display-graphic-p)
      (set-face-attribute 'default nil :family "Menlo" :height 140 :weight 'bold))
    (setq mac-option-modifier 'super)
    (setq mac-function-modifier 'control)
    (setq mac-command-modifier 'meta)))
 (t nil))

(setq linum-format "%d ")
;; (global-linum-mode -1)

(transient-mark-mode t)
(global-visual-line-mode)               ; word wrap
(blink-cursor-mode -1)
(delete-selection-mode t)

(put 'downcase-region  'disabled nil)   ; Let downcasing work
(put 'erase-buffer     'disabled nil)
(put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page   'disabled nil)   ; Let narrowing work
(put 'narrow-to-region 'disabled nil)   ; Let narrowing work
(put 'set-goal-column  'disabled nil)
(put 'upcase-region    'disabled nil)   ; Let upcasing work

(fset 'yes-or-no-p 'y-or-n-p)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(show-paren-mode)
(column-number-mode)
(which-function-mode)
(global-hi-lock-mode)                 ; highlight regexp

(bind-key "C-z t" 'toggle-show-trainling-whitespace)
(bind-key* "C-h C-k" 'describe-personal-keybindings)

(bind-key* "M-?" 'imenu)

(defun reformat-buffer ()
  " Reformat buffer using emacs indent - region "
  (interactive)
  (delete-trailing-whitespace)
  (save-excursion
    (mark-whole-buffer)
    (indent-region (region-beginning) (region-end))))

(bind-key "M-o" 'reformat-buffer)

(defun show-compilation ()
  (interactive)
  (call-interactively 'compile))
(bind-key* "M-O" 'show-compilation)

(set-register ?. '(file . "~/.emacs.d/init.el")) ; use C-x r j e to open init.el
(set-register ?, '(file . "~/.emacs.d/"))
(set-register ?s '(file . "~/.emacs.d/site-lisp/scratch.el"))
(set-register ?o '(file . "~/org/index.org"))

(bind-key "M-N" 'next-error)
(bind-key "M-P" 'previous-error)

(use-package scroll-lock
  :bind
  ("M-n" . scroll-lock-next-line)
  ("M-p" . scroll-lock-previous-line))

(defun zap-up-to-char (arg char)
  "Kill up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current
buffer.  Goes backward if ARG is negative; error if CHAR not
found."
  (interactive "p\ncZap to char: ")
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for char) char))))
  (kill-region (point)
               (progn
                 (search-forward (char-to-string char) nil nil arg)
                 (goto-char
                  (if (> arg 0) (1- (point))
                    (1+ (point))))
                 (point))))

(bind-key "M-z" 'zap-up-to-char)
(cua-selection-mode t)
;; (bind-key "C-<cr>" 'cua-rectangle-mark-mode)


(use-package align
  :bind
  ("C-z C-r" . align-regexp))

(use-package windmove
  :bind*
  ("M-L" . windmove-right)
  ("M-H" . windmove-left)
  ("M-K" . windmove-up)
  ("M-J" . windmove-down))

(require 'cl)
(defun split-window-func-with-other-buffer (split-function)
  (lexical-let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer)))))

(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer
              'split-window-horizontally))))

(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer
              'split-window-vertically))))

(defun toggle-win-td-lr ()
  "Toggle top/down layout to left/right layout when only 2
windows available"
  (interactive)
  (when (eq 2 (length (window-list)))
    (cond
     ((window-in-direction 'right)
      (windmove-right)
      (delete-window)
      (split-window-vertically-instead)
      (windmove-up))
     ((window-in-direction 'left)
      (delete-window)
      (split-window-vertically-instead)
      (windmove-down))
     ((window-in-direction 'below)
      (windmove-down)
      (delete-window)
      (split-window-horizontally-instead))
     (t
      (delete-window)
      (split-window-horizontally-instead)
      (windmove-right)))))

(bind-key* "M-E" 'toggle-win-td-lr)

(defun split-follow-buffer ()
  " Split current buffer into 2 and enter follow mode to make use
of modern wide display"
  (interactive)
  (keyboard-escape-quit)
  (split-window-right)
  (follow-mode)
  (message "split-follow-file"))

(bind-key* "C-z C-f" 'split-follow-buffer)

(bind-key* "C-h u" 'woman)

;; (require 'scratch)

(use-package highlight-global
  :bind
  ("M-\"" . highlight-frame-toggle)
  ("M-+" . clear-highlight-frame)
  ("C-z r" . revert-buffer))

(use-package expand-region
  :bind
  ("M-M" . er/expand-region))

(use-package magit
  :bind
  ("C-c g" . magit-status)
  ("C-c C-g" . magit-status))

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(projectile-mode)

(bind-key "M-R" 'rsync-cmd)

(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))


(hook-into-modes (lambda () (linum-mode -1)) #'compilation-mode)


;; * ag
(use-package ag
  :config
  (defun ag--get-func-name (fname line)
    (require 'which-func)
    (save-excursion
      (find-file fname)
      (goto-line line)
      (which-function)))

  (defun ag--get-file-name()
    (let ((line (thing-at-point 'line t)))
      (setq line (replace-regexp-in-string "File: " "" line))
      (setq line (replace-regexp-in-string "\n" "" line))
      line))

  (defun ag-save-opened-files(proc)
    (when (equal major-mode 'ag-mode)
      ;; (message "ag-save-opened-files() ........")
      (mapcar '(lambda (buf)
                 (when (buffer-file-name buf)
                   (message "found open file(%s)" (buffer-file-name buf))
                   (puthash (buffer-file-name buf) t ag-opened-file-hash)))
              (buffer-list))))

  (defun ag-close-all-tmp-files (buff-not-cared howclosed-not-cared)
    (when (equal major-mode 'ag-mode)
      ;; (message "ag-close-all-tmp-files() .....")
      (mapcar '(lambda (buf)
                 (let ((bufname (buffer-file-name buf)))
                   (when bufname
                     (when (not (gethash (buffer-file-name buf) ag-opened-file-hash nil))
                       ;; (message "file(%s) open tmply, close it" bufname)
                       (kill-buffer buf))
                     )))
              (buffer-list))
      )
    )

  (defun ag-add-fun ()
    (when (equal major-mode 'ag-mode)
      (save-window-excursion
        (forward-line 0)
        (let ((end (point))
              (fname nil)
              (cwd default-directory)
              (fname-set-by-search nil)
              (cur-point nil)
              (beg nil))

          (goto-char compilation-filter-start)
          (forward-line 0)
          (setq beg (point))

          ;; (message "ag-add-fun ........>>>>> beg(%d) end(%d), buf\n------\n%s\n-------"
          ;;          beg end (buffer-substring-no-properties beg end))
          ;; create hash of all opened file

          ;; (message "-----> add-fun start(%d) end(%d)" beg end) ag/search

          (when (not (string-match "^File: .*$" (thing-at-point 'line t)))
            ;; (message "point(%d) line(%s) not a file, search backward ... "
            ;;          (point) (thing-at-point 'line t))

            (if (re-search-backward "^File: " (point-min) 1)
                (progn (setq fname (ag--get-file-name))
                       ;; (message "search back for file(%s) success" fname)
                       (setq fname-set-by-search t)
                       (goto-char beg))
              (progn
                ;; (message "!!! BUG: search back for \"File: xxx\" faild, txt(%s), buffer(%s)"
                ;;          (buffer-substring-no-properties beg end)
                ;;          (buffer-substring-no-properties (point-min) (point-max)))
                )))

          ;; find other File
          (while (or fname-set-by-search (re-search-forward "^File: \\(.*\\)$" end 1))
            (when (not fname-set-by-search) (setq fname (match-string 1)))
            ;; tell next round search for fname
            (when fname-set-by-search (setq fname-set-by-search nil))

            (setq cur-point (point))

            (let ((file-end end))
              (when (re-search-forward "^File: \\(.*\\)$" end 1)
                ;; (message "    found a new File at(%d), line(%s)" (point) (thing-at-point 'line t))
                (goto-char (match-beginning 0))
                (setq file-end (point)))

              (goto-char cur-point)
              (while (re-search-forward "^\\([0-9]+\\):\\([0-9]+\\):" file-end 1)
                (setq line (string-to-number (match-string 1)))
                (setq column (string-to-number (match-string 2)))
                (goto-char (match-end 0))
                (setq func (format " %s()  " (ag--get-func-name fname line)))
                (setq end (+ end (length func))) ; ajust end
                ;; (message "%s:%d:%d %s ..." fname line column func)
                (insert func))
              ))

          ;; (message "     <-end end(%d)" end)
          ))))


  (defun setup-ag-mode ()
    (setq ag-highlight-search t)
    (message "setup-ag-mode ...............+++++.")
    ;; (add-hook 'compilation-filter-hook 'ag-add-fun t t)
    ;; (remove-hook 'compilation-filter-hook 'ag-add-fun t)

    (setq ag-opened-file-hash (make-hash-table :test 'equal))
    (make-variable-buffer-local 'ag-opened-file-hash)
    (add-hook 'compilation-start-hook 'ag-save-opened-files t t)
    (add-hook 'compilation-finish-functions 'ag-close-all-tmp-files t t)
    )

  (add-hook 'ag-mode-hook 'setup-ag-mode))

;; * elisp mode
(use-package lisp-mode
  :config
  (defun do-eval-buffer ()
    (interactive)
    (call-interactively 'eval-buffer)
    (message "Buffer has been evaluated"))
  (bind-key "C-c e b" 'do-eval-buffer emacs-lisp-mode-map)
  (bind-key "C-c e c" 'cancel-debug-on-entry)
  (bind-key "C-j" 'newline-and-indent)
  (bind-key "C-c e d" 'debug-on-entry))

;; * dired
(use-package dired
  :bind
  :config
  (setq dired-listing-switches "-ahl")

  (defun dired-isearch-filenames-forward ()
    "Search for a string using Isearch only in file names in the Dired buffer."
    (interactive)
    (beginning-of-buffer)
    (forward-line)
    (let ((dired-isearch-filenames t))
      (isearch-forward)))

  (defun dired-isearch-filenames-backward ()
    "Search for a string using Isearch only in file names in the Dired buffer."
    (interactive)
    (let ((dired-isearch-filenames t))
      (isearch-backward)))

  (defun sof/dired-sort ()
    "Dired sort hook to list directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
    (and (featurep 'xemacs)
         (fboundp 'dired-insert-set-properties)
         (dired-insert-set-properties (point-min) (point-max)))
    (set-buffer-modified-p nil))

  (add-hook 'dired-after-readin-hook 'sof/dired-sort)
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode 1)
              (require 'dired-x)
              (setq wdired-allow-to-change-permissions 'advanced)
              (setq dired-omit-size-limit nil)
              (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
              (dired-omit-mode)
              (bind-key "/" 'dired-isearch-filenames-forward dired-mode-map)
              (bind-key "M-o" 'dired-omit-mode dired-mode-map)
              (bind-key "?" 'dired-isearch-filenames-backward dired-mode-map)
              (bind-key "J" 'find-name-dired dired-mode-map)
              (bind-key "K" 'find-grep-dired dired-mode-map))))


;; * company mode
(use-package company
  :init
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (hook-into-modes #'company-mode
                   'emacs-lisp-mode-hook
                   'c++-mode-hook
                   'c-mode-hook
                   'go-mode-hook)

  :config
  (defun setup-company-mode ()
    ;; setup faces
    (custom-set-faces
     '(company-preview
       ((t (:foreground "darkgray" :underline t))))
     '(company-preview-common
       ((t (:inherit company-preview))))
     '(company-tooltip
       ((t (:background "lightgray" :foreground "black"))))
     '(company-tooltip-selection
       ((t (:background "steelblue" :foreground "white"))))
     '(company-tooltip-common
       ((((type x)) (:inherit company-tooltip :weight bold))
        (t (:inherit company-tooltip))))
     '(company-tooltip-common-selection
       ((((type x)) (:inherit company-tooltip-selection :weight bold))
        (t (:inherit company-tooltip-selection)))))

    (setq company-tooltip-limit 20)       ; bigger popup window
    (setq company-idle-delay .3)  ; decrease delay before autocompletion popup shows
    (setq company-echo-delay 0)   ; remove annoying

    ;; setup completion
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous))

  (add-hook 'company-mode-hook 'setup-company-mode))

;; * cc mode
(add-to-list 'auto-mode-alist '("\\.c+$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h+$" . c++-mode))

(use-package cc-mode
  :config
  (defun my-c-get-comment ()
    "/* */ [whitespace] [cursor], get comment before cursor."
    (interactive)
    (let ((comment nil)
          (begin nil)
          (end nil))
      (interactive)
      ;; skip white space
      (save-excursion
        (c-skip-ws-backward)
        (setq begin (point))
        (backward-char 2)
        (when (looking-at "\\*\/")
          ;; (message "comment end")
          (when (search-backward "/\*" nil t)
            (setq end (point))
            (setq comment
                  (buffer-substring-no-properties
                   begin end))))
        ;; (message comment)
        comment)))

  (defun my-c-defun-p (arg-list)
    "Check if function is a new function"
    ;; (message arg-list)
    (if (string-match "\)\\'" arg-list)
        t
      nil))

  (defun c-export-declarations (&optional arg)
    "Prompt for a C source file, and grab all function (except
ic ones) declaration and insert current point"
    (interactive "p")
    (let ((file (ido-read-file-name "File of the C source code:"))
          (func-declares "")
          (cur-buffer (buffer-name))
          (end-line 0)
          (new-declare "")
          (comment "")                ;intial empty comment
          (cur-line 0))
      (find-file file)
      ;; locate last function
      (end-of-buffer)
      (c-beginning-of-defun)
      (setq end-line (line-number-at-pos))
      ;; start from beginning of buffer
      (beginning-of-buffer)
      (while (not (eq end-line cur-line))
        (c-end-of-defun)
        (c-beginning-of-defun)
        (setq cur-line (line-number-at-pos))
        ;; get comments
        (setq comment (my-c-get-comment))
        (push-mark)
        (search-forward "{" nil t)
        (backward-char 1)
        (setq new-declare
              (buffer-substring-no-properties
               (region-beginning)
               (region-end)))

        ;; trim trailing whitespace
        (setq new-declare
              (replace-regexp-in-string "[ \t\n\r]+\\'" "" new-declare))
        ;; merge comment into declaration
        (when comment
          (setq new-declare
                (concat comment "\n" new-declare)))

        ;; static function is skipped
        (unless (or (not (my-c-defun-p new-declare))
                    (string-match "static " new-declare))
          (setq func-declares
                (concat func-declares
                        new-declare
                        ";\n\n")))
        ;; goto next defun
        (c-end-of-defun))
      ;; restore current buffer
      (switch-to-buffer cur-buffer)
      (insert func-declares)))

  (defun c-get-includes (&optional func)
    "Get #include for FUNC / symbol at point if func is nil"
    (interactive)
    (let ((sym nil)
          (includes nil)
          (bound nil)
          (file-list nil))
      (when (stringp func)
        (setq sym func)
        ;; get string
        (save-window-excursion
          (dolist (f (list (format "/usr/share/man/man3/%s.3.gz" sym)
                           (format "/usr/share/man/man2/%s.2.gz" sym)))
            (when (file-exists-p f)
              (message f)
              (woman-find-file f)

              ;; set bound to not include unneeded #includes in example code
              (setq bound (progn (search-forward "DESCRIPTION" nil t) (point)))
              (beginning-of-buffer)

              (while (search-forward "#include" bound t)
                (back-to-indentation)
                (add-to-list 'includes
                             (replace-regexp-in-string ;remove trailin comments
                              "[ ]*/\\*[^*]*\\*\\/" ""
                              (buffer-substring-no-properties
                               (point) (progn (end-of-line) (point))))))
              (return)
              (widen)))))
      ;; (princ includes)
      (setq includes (reverse includes)) ; reverse to get right order
      includes))

  (defun switch-to-header-or-source ()
    (interactive)
    (projectile-completing-read
     "Find file: "
     (projectile-current-project-files)
     :initial-input (concat (file-name-base (buffer-file-name))
                            (if (equal (file-name-extension (buffer-file-name)) "h")
                                " c"
                              " h"))
     :action `(lambda (file)
                (find-file (expand-file-name file ,(projectile-project-root)))
                (run-hooks 'projectile-find-file-hook))))


  (defun c-insert-includes (&optional arg)
    (interactive "p")
    (let ((sym nil)
          (incs-exists (list ))
          (new-inclds nil))
      ;; get function name
      (if (eq arg 4)
          (setq sym (read-from-minibuffer "Function to look:"))
        (setq sym (thing-at-point 'symbol)))
      ;; get new incs
      (when (stringp sym)
        (setq new-inclds (c-get-includes sym))
        (princ new-inclds))
      ;; do insert
      (when new-inclds
        (save-excursion
          ;; find all #includes already exists
          (beginning-of-buffer)
          (while (re-search-forward "^#include" nil t)
            (back-to-indentation)
            (add-to-list 'incs-exists
                         (buffer-substring-no-properties
                          (point) (progn (end-of-line) (point)))))
          (delete-dups incs-exists)

          (defun c-in-comment ()
            "Return in block comment"
            (interactive)
            (let ((in-com nil))
              (dolist (s (c-guess-basic-syntax))
                (dolist (fltr (list 'comment-intro 'c))
                  (when (eq fltr (car s))
                    (setq in-com t)
                    (return))))
              in-com))

          ;; find insert location
          (beginning-of-buffer)
          (if (re-search-forward "^#include" nil t)
              (progn
                (beginning-of-line))
            (progn
              (while (c-in-comment)
                (next-line))
              (beginning-of-line)))
          ;; real insert
          (dolist (i new-inclds)
            (when (not (member i incs-exists)) ;insert only when not exist
              (insert i)
              (newline)))))))

  (defun reformat-function ()
    "Reformat function args and return value when IN { } block."
    (interactive)
    (let ((arg-list nil)
          (ret-val-line nil)
          (back-brace-line nil))
      ;; join all to 1 line
      (progn (c-beginning-of-defun) (setq ret-val-line (line-number-at-pos)))
      (progn (re-search-forward ")[ \t\r\n]*{" nil t)
             (search-backward ")" nil t) (forward-char 1)
             (setq back-brace-line (line-number-at-pos)))
      (goto-line back-brace-line)
      (while (not (eq (line-number-at-pos) ret-val-line))
        (join-line))

      ;; trim multiple space to 1 after ,
      (replace-regexp ",[ \t]*\\([^ ]\\)" ", \\1" nil
                      (progn (beginning-of-line) (point))
                      (progn (end-of-line) (point)))
      (back-to-indentation)
      (search-forward "{" nil t)
      (search-backward ")" nil t) (forward-char 1)
      (backward-sexp 1)
      ;; (backward-sexp 1)
      (re-search-backward "[ \t\r\n]" nil t)
      (c-context-line-break)
      (search-forward "(" nil t) (backward-char 1)
      (mark-sexp)
      (narrow-to-region (region-beginning) (region-end))
      (while (search-forward "," nil t)
        (c-context-line-break)
        (align-current))
      (widen)
      (reformat-buffer)))

  (defun insert-header-guard (&optional arg)
    (interactive "p")
    (if (buffer-file-name)
        (let*
            ((fName (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name))))
             (ifDef (concat
                     "#ifndef _" fName "_H_" "\n#define _" fName "_H_" "\n"))
             (endDef (concat
                      "\n#endif    " "/* _" fName "_H_ */\n"))
             (insert-cpp-gaurd (equal 4 arg)))
          (progn
            (goto-char (point-min))
            (insert ifDef)
            (when insert-cpp-gaurd
              (insert "\n#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n"))
            (goto-char (point-max))
            (when insert-cpp-gaurd
              (insert "\n#ifdef __cplusplus\n}\n#endif\n"))
            (insert endDef)))))
  (require 'font-lock)

  ;; create a face for function calls
  (defface my-font-lock-function-call-face
    '((t (:foreground "DarkBlue")))
    "Font Lock mode face used to highlight function calls."
    :group 'font-lock-highlighting-faces)

  (defvar my-font-lock-function-call-face 'font-lock-function-name-face)
  (add-hook 'c-mode-hook
            (lambda ()
              (font-lock-add-keywords
               nil
               '(("\\<\\(\\sw+\\) ?(" 1
                  my-font-lock-function-call-face)) t)))

  (add-hook 'c++-mode-hook
            (lambda ()
              (font-lock-add-keywords nil '(("\\<\\(\\sw+\\) ?(" 1
                                             my-font-lock-function-call-face)) t)
              ))


  (add-hook 'c-initialization-hook
            (lambda ()
              (bind-key "C-j" 'c-context-line-break c-mode-base-map)
              (bind-key "C-c C-u" 'c-up-conditional-with-else c-mode-base-map)
              (bind-key "C-c C-d" 'c-down-conditional-with-else c-mode-base-map)
              (bind-key "C-c C-i" 'insert-header-guard c-mode-base-map)
              (bind-key "C-z C-r" 'align-current c-mode-base-map)
              (bind-key "C-c i" 'c-insert-includes c-mode-base-map)
              (bind-key "C-c f" 'reformat-function c-mode-base-map)
              (bind-key "C-c c" 'ir-refresh c-mode-base-map)
              (bind-key "M-D" 'c-export-declarations c-mode-base-map)
              (bind-key "C-c h" 'switch-to-header-or-source c-mode-base-map)
              (bind-key "C-c i" 'c-insert-includes c-mode-base-map)
              (setq-default c-electric-pound-behavior (quote (alignleft)))))

  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-toggle-electric-state 1)
              (c-toggle-hungry-state 1)
              (c-toggle-auto-newline 1)
              (c-toggle-syntactic-indentation 1)
              (require 'google-c-style)
              (google-set-c-style)
              (setq-default c-echo-syntactic-information-p t)
              (setq-default c-syntactic-indentation-in-macros t)))
  )

;; * go mode
(use-package go-mode
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              ;; (setq company-go-gocode-command "/home/glendai/goworkspace/bin/gocode")
              (bind-key "M-." 'godef-jump go-mode-map)
              (bind-key "C-j" 'newline-and-indent go-mode-map)
              (bind-key "M-." 'godef-jump go-mode-map)
              (bind-key "C-j" 'newline-and-indent go-mode-map)
              (bind-key "C-c C-u" 'go-remove-unused-imports go-mode-map)
              (bind-key "C-c C-i" 'go-goto-imports go-mode-map)
              (bind-key "C-c C-r" 'go-remove-unused-imports go-mode-map)
              (bind-key "C-x 4 ." 'godef-jump-other-window go-mode-map)
              (add-hook 'before-save-hook #'gofmt-before-save)
              (set (make-local-variable 'company-backends) '(company-go))
              (set (make-local-variable 'compile-command) (concat "go build " buffer-file-name))
              )))

;; * term

(use-package term
  :bind
  ("M-T" . new-term-or-switch)
  :config
  (defun spawn-ansi-term ()
    (interactive)
    (call-interactively 'ansi-term))

  (defun switch-to-term-buffer ()
    "List all ansi-term buffer with helm, jump to it when
select one"
    (interactive)
    (let ((term-buf-list
           (cl-delete-if-not
            #'(lambda (b)
                (string-match "ansi-term" (buffer-name b)))
            (buffer-list)))
          switch-fun
          buf)
      (if term-buf-list
          (progn
            (setq switch-fun
                  (if
                      'switch-to-buffer
                      'pop-to-buffer))
            (if (eq 1 (length term-buf-list))
                (switch-to-buffer (car term-buf-list))
              (progn
                (setq buf
                      (ivy-completing-read
                       "Select a terminal buffer:"
                       (mapcar
                        #'(lambda (b)   ; show default directory to help selection
                            (format "%s - <%s>" (buffer-name b)
                                    (with-current-buffer b default-directory)))
                        term-buf-list)))
                (when buf
                  ;; delete default directory first
                  (setq buf (replace-regexp-in-string " - <.*>$" "" buf))
                  (switch-to-buffer buf)))))
        (spawn-ansi-term))))
  (defun new-term-or-switch (arg)
    (interactive "p")
    (if (eq arg 4) (spawn-ansi-term)
      (switch-to-term-buffer)))
  (add-hook 'term-mode-hook (lambda () (linum-mode -1))))

;; * org
(use-package org
  :config
  ;; todo stuff
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-todo-keyword-faces
        '(("TODO" . org-warning)
          ("STARTED" . "yellow")
          ("DONE" . org-done)
          ("CANCELED" . (:foreground "blue" :weight bold))
          ("WAITED" . org-scheduled)
          ))

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "STARTED(s)"
           "WAITED(w@/!)"               ; / leaving also record timestamp
           "|"
           "DONE(d!)"                   ; ! for timestamp
           "CANCELED(c@)"               ; @ for note with timestamp
           )))

  ;; progress logging
  (setq org-log-done 'time)
  (setq org-log-into-drawer t) ; use drawer as state chagne note instead of list item

  ;; Automatically change to DONE when all children are done
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  (add-hook 'org-mode-hook
            (lambda ()
              (bind-key* "C-c l" 'org-store-link)
              (bind-key* "C-c c" 'org-capture)
              (bind-key* "C-c a" 'org-agenda)
              (bind-key* "C-c b" 'org-switchb)
              ;; (org-bullets-mode)
              )))


;; * web related
(use-package web-mode
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.css?\\'" . web-mode)
   ("\\.js\\'" . web-mode)
   )
  :config
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq tab-width 2)
    (setq web-mode-enable-current-column-highlight t)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-auto-closing t) ; auto close tag after </
    (setq web-mode-enable-css-colorization t)
    (set (make-local-variable 'company-backends) '(company-css company-web-html company-files))
    (emmet-mode)
    (company-mode)
    (setq web-mode-css-indent-offset 2))
  (add-hook 'web-mode-hook  'my-web-mode-hook))

(use-package js2-mode
  :mode
  (("\\.js\\'" . js2-mode))
  :config
  (add-hook 'js2-mode-hook  #'company-mode)
  )

;; * postload
(add-hook 'find-file-hook
          (lambda ()
            (diminish 'ivy-mode)
            (diminish 'abbrev-mode)
            (diminish 'outline-minor-mode)
            (diminish 'visual-line-mode)
            (diminish 'company-mode)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-web emmet-mode js2-mode web-mode company-go ag async cmake-mode company counsel dash diminish expand-region git-commit go-eldoc go-mode ivy magit projectile snazzy-theme swiper transient use-package with-editor))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white")))))

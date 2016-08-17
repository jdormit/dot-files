;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(require 'dired-x)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(indent-tabs-mode nil)
 '(require-final-newline t)
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.utah.edu")
 '(smtpmail-smtp-service 25)
 '(sql-postgres-program "/usr/sup/pgsql/bin/psql")
 '(sql-sybase-options (quote ("-w" "2000")))
 '(wakatime-api-key "3eec5e9e-a6a9-48fa-8098-bab315e32f49")
 '(wakatime-cli-path "/usr/local/bin/wakatime"))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Send backups to a save directory, so you don't have a bunch of ~ files in your working directories.
;; Make sure this directory exists!
(setq backup-directory-alist `(("." . "~/.saves")))

; Just some personal preferences and key bindings
(show-paren-mode)
(global-set-key "\C-x\C-b"	'electric-buffer-list)
(global-set-key "\C-cs"		'shell)
(global-set-key [f6]		'call-last-kbd-macro)
(global-set-key "\C-c;"		'next-error)
(global-set-key "\C-s"		'isearch-forward-regexp)
(global-set-key "\C-r"		'isearch-backward-regexp)
(global-set-key [(meta f12)]	'next-multiframe-window) 
(global-set-key [(meta f12)]	'next-multiframe-window)
(global-set-key "\C-cc"		'copy-region-as-kill) ;; this should be out of the box, in my opinion. The equivalent of cmd - c
(global-set-key "\C-cl"		'goto-line)
(global-set-key "\C-ct"		'toggle-truncate-lines)
(global-set-key "\C-c\\"	'comment-region)
(global-set-key "\C-xq"		'query-replace) ;; conditional replace -- it goes one by one.
(global-set-key "\C-cr"         'replace-string);; mass replace -- does it everywhere!
(global-set-key "\C-xj"		'fill-paragraph)
(global-set-key "\C-cn"		'rename-buffer)
(global-set-key (kbd"\C-c SPC") 'compile)
(global-set-key "\C-c\C-c"      'shell-command)
(global-set-key "\C-x\C-l"      'linum-mode)
(global-set-key "\C-cm"         'toggle-frame-maximized)
(global-set-key (kbd"C-<return>")
                                'newline) ;; newline without auto-indent
(global-set-key "\C-x\M-f"      'sudo-find-file)

(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)

(put 'narrow-to-region 'disabled nil)

;; Find FILE AT POINT -- this is a really cool plugin, where if your curser is on a file name (in the shell, in some script)
;; ffap will try it's hardest to find that file in your file system, so you don't have to write out the full path
(require 'ffap)
(declare (special  ffap-bindings
		   ffap-require-prefix))
(setq ffap-bindings 
      '(
	(global-set-key  "\M-M" 'ffap-menu)
	(global-set-key "\M-L" 'ffap-next)
	(global-set-key [S-mouse-3] 'ffap-at-mouse)
	(global-set-key [C-S-mouse-3] 'ffap-menu)
	(global-set-key "\C-x\C-f" 'find-file-at-point)
	(global-set-key "\C-x\C-v" 'ffap-other-window)
	(global-set-key "\C-x4f"   'ffap-other-window)
	(global-set-key "\C-x5f"   'ffap-other-frame)
	)
      )

(setq ffap-require-prefix nil)
(ffap-bindings)

(setq special-display-buffer-names
      '(
	("*SQL*" (top . 0) (left . 20) (width . 110) (height . 35))
	("*grep*" (top . 220) (left . 35) (width . 100) (height . 10))
	("*info*" (top . 200) (left . 210) (height . 55))
	("*mail*" (width . 80))
	("electrix.html" (top . 40) (left . 20) (height . 65) (auto-raise . t))
	("*Shell Command Output*")))

;;; Allow quitting ediff mode without asking "are you sure?"!
(defun my-ediff-quit (reverse-default-keep-variants)
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (let ((ctl-buf (current-buffer)))
    (message "")
    (set-buffer ctl-buf)
    (ediff-really-quit reverse-default-keep-variants)))

(add-hook 'ediff-keymap-setup-hook
	  (lambda ()
	    (define-key ediff-mode-map "q" 'my-ediff-quit)))

;; quiet, please! No dinging!
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
      (message "No non-ascii characters."))))

(defun find-next-unsafe-char (&optional coding-system)
  "Find the next character in the buffer that cannot be encoded by
coding-system. If coding-system is unspecified, default to the coding
system that would be used to save this buffer. With prefix argument,
prompt the user for a coding system."
  (interactive "Zcoding-system: ")
  (if (stringp coding-system) (setq coding-system (intern coding-system)))
  (if coding-system nil
    (setq coding-system
          (or save-buffer-coding-system buffer-file-coding-system)))
  (let ((found nil) (char nil) (csets nil) (safe nil))
    (setq safe (coding-system-get coding-system 'safe-chars))
    ;; some systems merely specify the charsets as ones they can encode:
    (setq csets (coding-system-get coding-system 'safe-charsets))
    (save-excursion
      ;;(message "zoom to <")
      (let ((end  (point-max))
            (here (point    ))
            (char  nil))
        (while (and (< here end) (not found))
          (setq char (char-after here))
          (if (or (eq safe t)
                  (< char ?\177)
                  (and safe  (aref safe char))
                  (and csets (memq (char-charset char) csets)))
              nil ;; safe char, noop
            (setq found (cons here char)))
          (setq here (1+ here))) ))
    (and found (goto-char (1+ (car found))))
    found))

;; hopefully set path to the default terminal path
(if (not (getenv "TERM_PROGRAM"))
      (let ((path (shell-command-to-string
              "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
        (setenv "PATH" path)))

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(autoload 'bash-completion-dynamic-complete 
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
  'bash-completion-dynamic-complete)

(add-hook 'comint-output-filter-functions
'comint-watch-for-password-prompt)

(global-wakatime-mode)


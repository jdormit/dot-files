;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(require 'dired-x)

(global-auto-revert-mode 1)

(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'server)
(or (server-running-p)
        (server-start))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d606ac41cdd7054841941455c0151c54f8bff7e4e050255dbd4ae4d60ab640c1" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" default)))
 '(ecb-options-version "2.40")
 '(eclim-eclipse-dirs (quote ("~/opt/eclipse")))
 '(eclim-executable "~/opt/eclipse/eclim")
 '(indent-tabs-mode nil)
 '(paradox-github-token t)
 '(require-final-newline t)
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.utah.edu")
 '(smtpmail-smtp-service 25)
 '(sql-postgres-program "/usr/sup/pgsql/bin/psql")
 '(sql-sybase-options (quote ("-w" "2000")))
 '(wakatime-api-key "3eec5e9e-a6a9-48fa-8098-bab315e32f49")
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin "/usr/bin/python")
 '(wakatime-python-path "/usr/bin/python" t))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mmm-cleanup-submode-face ((t nil)))
 '(mmm-code-submode-face ((t nil)))
 '(mmm-comment-submode-face ((t nil)))
 '(mmm-declaration-submode-face ((t nil)))
 '(mmm-default-submode-face ((t nil)))
 '(mmm-init-submode-face ((t nil)))
 '(mmm-output-submode-face ((t nil)))
 '(mmm-special-submode-face ((t nil))))

(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
            `((".*" ,user-temporary-file-directory t)))

(defun region-to-xclip (&optional b e)
  (interactive  "r")
  (shell-command-on-region b e "xclip -selection clip")
  (deactive-mark)
  )

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
(global-set-key "\C-cc"		'region-to-xclip)
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
(global-set-key (kbd "C--")     'text-scale-decrease)
(global-set-key (kbd "C-=")     'text-scale-increase)
(global-set-key "\M-/"          'completion-at-point)
(global-set-key "\C-xl"         'org-store-link)
(global-set-key (kbd "C-x g") 'magit-status)

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

(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'after-init-hook 'global-company-mode)

(xterm-mouse-mode)

(add-to-list 'load-path "~/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load "company"
   '(add-to-list 'company-backends 'company-tern))

(add-hook 'python-mode-hook 'anaconda-mode)
(eval-after-load "company"
   '(add-to-list 'company-backends 'company-anaconda))
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location "~/.envs")

(require 'eclim)
(require 'eclimd)
(add-hook 'java-mode-hook 'eclim-mode)
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-eclim))

(add-hook 'js-mode-hook 'linum-mode)
(add-hook 'web-mode-hook 'linum-mode)
(add-hook 'vue-mode-hook 'linum-mode)
(add-hook 'rust-mode-hook 'linum-mode)
(add-hook 'toml-mode-hook 'linum-mode)
(add-hook 'python-mode-hook 'linum-mode)
(add-hook 'emacs-lisp-mode-hook 'linum-mode)
(add-hook 'scala-mode-hook 'linum-mode)
(add-hook 'java-mode-hook 'linum-mode)

(unless (display-graphic-p)
  (setq linum-format "%4d \u2502 ")
  (set-default 'truncate-lines t)
  )

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))

(add-hook 'mail-mode-hook 'visual-line-mode)

(add-hook 'find-file-hooks
   (lambda ()
     (let ((file (buffer-file-name)))
       (when (and file (equal (file-name-directory file) "/home/jeremy/notes/"))
         (org-mode)))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ruby . t)
   (C . t)
   (java . t)
   (js . t)
   (sh . t)))

(add-hook 'org-mode-hook
   (lambda ()
     (local-unset-key [(meta up)])
     (local-unset-key [(meta down)])
     (local-unset-key [(meta left)])
     (local-unset-key [(meta right)])
     (local-unset-key (kbd "ESC <up>"))
     (local-unset-key (kbd "ESC <down>"))
     (local-unset-key (kbd "ESC <left>"))
     (local-unset-key (kbd "ESC <right>"))
     (local-unset-key "\M-j")
     (local-unset-key "\M-k")
     (local-unset-key "\M-h")
     (local-unset-key "\M-l")
     (local-unset-key (kbd "ESC j"))
     (local-unset-key (kbd "ESC k"))
     (local-unset-key (kbd "ESC h"))
     (local-unset-key (kbd "ESC l"))

     (local-set-key (kbd "C-c f") 'org-metaright)
     (local-set-key (kbd "C-c b") 'org-metaleft)

     (visual-line-mode)
     ))

;; Many thanks to the author of and contributors to the following posts:
;; https://gist.github.com/mislav/5189704
;; http://robots.thoughtbot.com/post/53022241323/seamlessly-navigate-vim-and-tmux-splits
;;
;; TODO: Make a script that generates tmux and emacs code without duplication
;;
;; NOTE: My keybindings are not the default emacs ones, using windmove

;; Try to move direction, which is supplied as arg
;; If cannot move that direction, send a tmux command to do appropriate move
(defun windmove-emacs-or-tmux(dir tmux-cmd)
  (interactive)
  (if (ignore-errors (funcall (intern (concat "windmove-" dir))))
      nil                       ;; Moving within emacs
    (shell-command tmux-cmd)) ;; At edges, send command to tmux
  )

;Move between windows with custom keybindings
(global-set-key [(meta up)]
                '(lambda () (interactive) (windmove-emacs-or-tmux "up"  "tmux select-pane -U")))
(global-set-key [(meta down)]
                '(lambda () (interactive) (windmove-emacs-or-tmux "down"  "tmux select-pane -D")))
(global-set-key [(meta right)]
                '(lambda () (interactive) (windmove-emacs-or-tmux "right" "tmux select-pane -R")))
(global-set-key [(meta left)]
                '(lambda () (interactive) (windmove-emacs-or-tmux "left"  "tmux select-pane -L")))
(global-set-key (kbd "ESC <up>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "up"  "tmux select-pane -U")))
(global-set-key (kbd "ESC <down>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "down"  "tmux select-pane -D")))
(global-set-key (kbd "ESC <right>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "right" "tmux select-pane -R")))
(global-set-key (kbd "ESC <left>")
                '(lambda () (interactive) (windmove-emacs-or-tmux "left"  "tmux select-pane -L")))
(global-set-key "\M-k"
                '(lambda () (interactive) (windmove-emacs-or-tmux "up"  "tmux select-pane -U")))
(global-set-key "\M-j"
                '(lambda () (interactive) (windmove-emacs-or-tmux "down"  "tmux select-pane -D")))
(global-set-key "\M-l"
                '(lambda () (interactive) (windmove-emacs-or-tmux "right" "tmux select-pane -R")))
(global-set-key "\M-h"
                '(lambda () (interactive) (windmove-emacs-or-tmux "left"  "tmux select-pane -L")))
(global-set-key (kbd "ESC k")
                '(lambda () (interactive) (windmove-emacs-or-tmux "up"  "tmux select-pane -U")))
(global-set-key (kbd "ESC j")
                '(lambda () (interactive) (windmove-emacs-or-tmux "down"  "tmux select-pane -D")))
(global-set-key (kbd "ESC l")
                '(lambda () (interactive) (windmove-emacs-or-tmux "right" "tmux select-pane -R")))
(global-set-key (kbd "ESC h")
                '(lambda () (interactive) (windmove-emacs-or-tmux "left"  "tmux select-pane -L")))


; find TODO items
(defun grep-todos-in-dir (dir &optional not-recursive)
  "Grep recursively for TODO comments in the given directory"
  (interactive "Ddirectory:")
  (let ((recur "-r"))
    (if not-recursive
        (setq recur "")
    )
    (grep (concat "grep -nH -I " recur " -E \"[\\#\\/\\-\\;\\*]\s*TODO|FIXME|XXX:?\" " dir " 2>/dev/null"))
    )
)
(global-set-key [f4] 'grep-todos-in-dir) 

(setq inhibit-startup-screen t)

(ido-mode t)
(setq ido-enable-flex-matching t) ; flexibly match names
(setq ido-everywhere t) ; use ido-mode everywhere, in buffers and for finding files
(setq ido-use-filename-at-point 'guess) ; for find-file-at-point
(setq ido-use-url-at-point t) ; support IDO url matching too
(ido-ubiquitous-mode t)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(autoload 'ido-at-point-mode "ido-at-point")
(ido-at-point-mode)

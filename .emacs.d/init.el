(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(show-paren-mode t)
(add-to-list 'load-path "~/.emacs.d/")

(if window-system
  (progn (tool-bar-mode -1)
         (menu-bar-mode -1)))

; Clip Board
(if (and (equal system-type 'gnu/linux)
         (window-system))
    (setq x-select-enable-clipboard t))

; font-color
(global-font-lock-mode t)
(setq font-lock-support-mode 'jit-lock-mode)
(set-default-font "VL Gothic-11")

; default indent
(setq default-tab-width 2)
(setq c-basic-offset 2)
(setq-default indent-tabs-mode nil)

(eval-after-load "untabify-file"
  '(progn
     (remove-hook 'write-file-hooks 'untabify-before-write)))

; UTF-8 and Japanese Setting
(set-language-environment 'Japanese)
(set-terminal-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)

; global key
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-ci"
  (lambda () (interactive) (indent-rigidly (region-beginning) (region-end)  2)))
(global-set-key "\C-cu"
  (lambda () (interactive) (indent-rigidly (region-beginning) (region-end) -2)))

; color
(setq red    "#ff6666")
(setq green  "#66ff66")
(setq cyan   "cyan")
(setq blue   "#6699ff")
(setq yellow "#ffd314")
(setq purple "#9966ff")
(setq white  "white")
(set-face-foreground 'font-lock-comment-face red)
(set-face-foreground 'font-lock-string-face  green)
(set-face-foreground 'font-lock-keyword-face cyan)
(set-face-bold-p     'font-lock-keyword-face t)
(set-face-foreground 'font-lock-function-name-face blue)
(set-face-bold-p     'font-lock-function-name-face t)
(set-face-foreground 'font-lock-variable-name-face yellow)
(set-face-foreground 'font-lock-type-face green)
(set-face-foreground 'font-lock-builtin-face purple)
(set-face-foreground 'font-lock-constant-face purple)
(set-face-foreground 'font-lock-warning-face white)
(set-face-bold-p 'font-lock-warning-face nil)
(if window-system
  (progn (set-background-color "Black")
         (set-foreground-color "LightGray")
         (set-cursor-color "Gray")
         (set-frame-parameter nil 'alpha 90)
         (setq default-frame-alist (append (list '(width . 80)
                                          '(height . 30))
                                    default-frame-alist))
         (set-fontset-font
          nil 'japanese-jisx0208
          (font-spec :family "Ricty"))
))

; close confirm
(defadvice save-buffers-kill-emacs
  (before safe-save-buffers-kill-emacs activate)
  "safe-save-buffers-kill-emacs"
  (unless (y-or-n-p "Really exit emacs? ")
    (keyboard-quit)))

; package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

; helm-mode
(require 'helm-mode)
(require 'helm-git-grep)

(defun my-helm ()
  (interactive)
  (helm :sources '(helm-c-source-buffers-list
                   helm-c-source-recentf
                   helm-c-source-buffer-not-found
                   helm-c-source-git-grep)
        :buffer "*my helm*"))
(global-set-key "\M-;" 'my-helm)
(setq helm-input-idle-delay 0.02)

; js3-mode
(require 'js3-mode)
(setq js3-mirror-mode t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))

; rinari
(require 'rinari)
(add-to-list 'load-path "~/.emacs.d/rhtml")
(require 'rhtml-mode)

(defun ruby-mode-hook ()
  (autoload 'ruby-mode "ruby-mode" nil t)
  (add-hook 'ruby-mode-hook '(lambda ()
                               (setq ruby-deep-arglist t)
                               (setq ruby-deep-indent-paren nil)
                               (setq c-tab-always-indent nil)
                               (require 'inf-ruby)
                               (require 'ruby-compilation))))
(add-to-list 'auto-mode-alist '("Capfile"        . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile"        . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile"       . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'"     . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'"       . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'"       . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec?\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.prawn?\\'"   . ruby-mode))

(defun rhtml-mode-hook ()
  (autoload 'rhtml-mode "rhtml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . rhtml-mode)))

(defun yaml-mode-hook ()
  (autoload 'yaml-mode "yaml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs" "-i" "\g"))
(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
(setq migemo-coding-system 'utf-8-unix)
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(load-library "migemo")
(migemo-init)


(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-hook 'scss-mode-hook '(lambda()
                             (setq css-indent-level 2)
                             (setq css-indent-offset 2)))

(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'"     . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))

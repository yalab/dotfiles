(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(show-paren-mode t)
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

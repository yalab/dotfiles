(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(show-paren-mode t)
(setq ring-bell-function 'ignore)
(add-to-list 'load-path "~/.emacs.d/elisp")

(if window-system
  (progn (tool-bar-mode -1)
         (menu-bar-mode -1)))

(if (string= system-type "darwin")
  (progn (add-to-list 'exec-path "/opt/local/homebrew/bin")
         (setq ns-command-modifier (quote meta))))

; Clip Board
(if (and (equal system-type 'gnu/linux)
         (window-system))
    (setq x-select-enable-clipboard t))

; font-color
(global-font-lock-mode t)
(setq font-lock-support-mode 'jit-lock-mode)
;; (set-default-font "VL Gothic-11")

; default indent
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)

(eval-after-load "untabify-file"
  '(progn
     (remove-hook 'write-file-hooks 'untabify-before-write)))

; format
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;(setq require-final-newline nil)

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
(setq black  "black")
(setq red    "#ff6666")
(setq green  "#66ff66")
(setq yellow "#ffd314")
(setq blue   "#6699ff")
(setq cyan   "cyan")
(setq white  "white")
(setq magenta "#9966ff")
(set-face-foreground 'font-lock-comment-face red)
(set-face-foreground 'font-lock-string-face  green)
(set-face-foreground 'font-lock-keyword-face cyan)
(set-face-bold-p     'font-lock-keyword-face t)
(set-face-foreground 'font-lock-function-name-face blue)
(set-face-bold-p     'font-lock-function-name-face t)
(set-face-foreground 'font-lock-variable-name-face yellow)
(set-face-foreground 'font-lock-type-face green)
(set-face-foreground 'font-lock-builtin-face magenta)
(set-face-foreground 'font-lock-constant-face magenta)
(set-face-foreground 'font-lock-warning-face white)
(set-face-bold-p 'font-lock-warning-face nil)

(setq font-family "Ricty")

(if window-system
  (progn (set-background-color "Black")
         (set-foreground-color "LightGray")
         (set-cursor-color "Gray")
         (set-frame-parameter nil 'alpha 90)
         (setq default-frame-alist (append (list '(width . 80)
                                          '(height . 30))
                                    default-frame-alist))
         (set-face-attribute 'default nil :family font-family :height 140)
         (set-fontset-font
          nil 'japanese-jisx0208
          (font-spec :family font-family))
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
             '("melpa" . "https://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)

; helm-mode
(require 'format-spec)
(require 'helm-mode)
(require 'helm-config)
(require 'helm-grep)

(defun helm-git-grep (arg)
  "Preconfigured helm for git-grepping `default-directory'.
With a prefix arg ARG git-grep the whole repository."
  (interactive "P")
  (require 'helm-files)
  (helm-grep-git-1 (file-name-as-directory
                    (replace-regexp-in-string
                     "\n" ""
                     (shell-command-to-string "git rev-parse --show-toplevel"))) arg))

(helm-mode 1)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-mini-default-sources
   (quote
    (helm-source-grep-git helm-source-buffers-list helm-source-recentf helm-source-files-in-current-dir)))
 '(js2-strict-missing-semi-warning nil)
 '(package-selected-packages
   (quote
    (fish-mode sass-mode typescript-mode yaml-mode w3m twittering-mode toml-mode srefactor slim-mode scss-mode rubocop rinari quickrun popup nyan-mode nhexl-mode nasm-mode mmm-mode migemo markdown-mode+ lua-mode less-css-mode js2-mode indent-guide handlebars-mode gradle-mode go-mode flymake-sass flymake-rust flymake-ruby flymake flycheck dockerfile-mode csharp-mode coffee-mode async))))

(define-key global-map (kbd "M-;") 'helm-mini)
(define-key global-map (kbd "M-'") 'helm-git-grep)

; js2-mode
(require 'js2-mode)
(setq js2-mirror-mode t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.es6$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . js2-jsx-mode))
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

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
(add-to-list 'auto-mode-alist '("Bowerfile"      . ruby-mode))
(add-to-list 'auto-mode-alist '("Podfile"        . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'"     . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'"       . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ruby\\'"       . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'"       . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec?\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.prawn?\\'"   . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder?\\'"   . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.xlsx.axlsx?\\'"   . ruby-mode))

(defun ruby-mode-set-encoding () nil)

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
(cond ((string= system-type "darwin")
       (setq migemo-dictionary "/opt/local/homebrew/Cellar/cmigemo/20110227/share/migemo/utf-8/migemo-dict"))
      (t
       (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")))
;(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
(setq migemo-coding-system 'utf-8-unix)
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(load-library "migemo")
(migemo-init)


(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-hook 'scss-mode-hook '(lambda()
                             (setq css-indent-level 2)
                             (setq css-indent-offset 2)
                             (setq scss-compile-at-save nil)))

(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'"     . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))

(require 'ruby-style)
(add-hook 'c-mode-hook '(lambda () (c-set-style "ruby")))
(add-hook 'yacc-mode-hook '(lambda () (c-set-style "ruby")))

; treetop-mode
(require 'treetop-mode)
(add-to-list 'auto-mode-alist '("\\.treetop$" . treetop-mode))

; coffee-mode for coffee.erb
(add-to-list 'auto-mode-alist '("\\.coffee\\.erb$" . coffee-mode))
(add-hook 'coffee-mode-hook '(lambda()
                               (setq tab-width 2)
                               (setq coffee-tab-width 2)))


; handlebars-mode
(require 'handlebars-mode)
(add-to-list 'auto-mode-alist '("\\.hbs$" . handlebars-mode))

; GNU global
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))

; nyan-mode
(require 'nyan-mode)
(nyan-mode)
(nyan-start-animation)

;;; mmm-mode
(require 'mmm-mode)
(require 'mmm-auto)
(require 'mmm-erb)
(setq mmm-global-mode 'maybe)

(define-derived-mode markdown-erb-mode markdown-mode "markdown erb")

(mmm-add-mode-ext-class 'markdown-erb-mode "\\.md\\.erb\\" 'erb)
(add-to-list 'auto-mode-alist '("\\.md\\.erb$" . markdown-erb-mode))

(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode))
(require 'quickrun)


(require 'twittering-mode)
(setq twittering-use-master-password t)


;;; go-mode
(require 'go-mode)
(add-hook 'go-mode-hook
      '(lambda()
         (setq c-basic-offset 4)
         (setq indent-tabs-mode nil) ))

;; modeline-git-branch
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/modeline-git-branch"))
(require 'modeline-git-branch)
(modeline-git-branch-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(require 'racc-mode)

;; cpp
(require 'cc-mode)
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "k&r")
            (setq indent-tabs-mode nil)
            (setq c-basic-offset 2)))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-include-path
                           (list (expand-file-name "/opt/boxen/homebrew/opt/opencv3/include")))))
(require 'srefactor)
(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)


(add-to-list 'auto-mode-alist '("\\.script\\'"       . lua-mode))


;; markdown
(setq markdown-command "/opt/local/homebrew/bin/multimarkdown")

(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(show-paren-mode t)
(setq ring-bell-function 'ignore)
(add-to-list 'load-path "~/.emacs.d/elisp")
(load "ylb-functions")
;; (require 'foreign-regexp)
;; (custom-set-variables
;;  '(foreign-regexp/regexp-type 'ruby)
;;  '(reb-re-syntax 'foreign-regexp))

(if window-system
  (progn (tool-bar-mode -1)
         (menu-bar-mode -1)))

(if (string= system-type "darwin")
  (progn (add-to-list 'exec-path "/opt/homebrew/bin")
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

;; (eval-after-load "untabify-file"
;;   '(progn
;;      (remove-hook 'write-file-hooks 'untabify-before-write)))

; format
;(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline nil)

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
         (setq default-frame-alist (append (list '(width . 95)
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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

; js2-mode
(require 'js2-mode)
(setq js2-mirror-mode t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.es6$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . js2-jsx-mode))
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

; tss
(setq typescript-indent-level 2)
(require 'ruby-env)

(defun yaml-mode-hook ()
  (autoload 'yaml-mode "yaml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs" "-i" "\g"))
(cond ((string= system-type "darwin")
       (setq migemo-dictionary "/opt/homebrew/Cellar/cmigemo/20110227/share/migemo/utf-8/migemo-dict"))
      (t
       (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")))
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


(require 'company)
(global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)

(define-derived-mode markdown-erb-mode markdown-mode "markdown erb")

(mmm-add-mode-ext-class 'markdown-erb-mode "\\.md\\.erb\\" 'erb)
(add-to-list 'auto-mode-alist '("\\.md\\.erb$" . markdown-erb-mode))

(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode))
(require 'quickrun)


(require 'twittering-mode)
(setq twittering-use-master-password t)

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
(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c-mode-hook (lambda ()
                         (setq-local flycheck-clang-language-standard "c11")))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-include-path
                           (list "./include"
                                 "./.ext/include/x86_64-darwin16"
                                 "/Users/yalab/project/ruby/include"))))
;; (require 'srefactor)
;; (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
;; (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)


(add-to-list 'auto-mode-alist '("\\.script\\'"       . lua-mode))


;; markdown
(setq markdown-command "/usr/local/bin/multimarkdown")
(load "counsel-env")
(load "rust-env")

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


(require 'color)
(defun rainbow-delimiters-using-stronger-colors ()
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
    (cl-callf color-saturate-name (face-foreground face) 30))))
(add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dumb-jump slim-mode cargo lsp-ui yaml-mode flycheck-rust lsp-mode w3m twittering-mode toml-mode tide scss-mode rustic rubocop rinari rainbow-delimiters quickrun popup php-mode nyan-mode nasm-mode mmm-mode migemo lua-mode js2-mode indent-guide handlebars-mode gradle-mode foreign-regexp flymake-rust flymake fish-mode dockerfile-mode csharp-mode counsel company-go coffee-mode)))

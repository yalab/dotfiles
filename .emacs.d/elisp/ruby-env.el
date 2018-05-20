(provide 'ruby-env)
; rinari
(require 'rinari)
(add-to-list 'load-path "~/.emacs.d/rhtml")
(require 'rhtml-mode)

(add-hook 'rhtml-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))

(define-key global-map (kbd "C-\\") 'hs-toggle-hiding)

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

(require 'ruby-style)
(add-hook 'c-mode-hook '(lambda () (c-set-style "ruby")))
(add-hook 'yacc-mode-hook '(lambda () (c-set-style "ruby")))

; treetop-mode
(require 'treetop-mode)
(add-to-list 'auto-mode-alist '("\\.treetop$" . treetop-mode))


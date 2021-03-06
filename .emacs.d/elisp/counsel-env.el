(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-extra-directories nil)
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-;") 'counsel-ibuffer)
(define-key ivy-minibuffer-map (kbd "<return>") #'ivy-alt-done)
(setq counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))

(require 'dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setf dumb-jump-selector 'ivy)

(defun counsel-git-grep-function (string &optional _pred &rest _u)
  "Grep in the current git repository for STRING."
  (split-string
   (shell-command-to-string (format "git --no-pager grep --no-color -i -e \"%s\"" string)) "\n" t))

(defun counsel-git-grep ()
  "Grep for a string in the current git repository."
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory ".git"))
        (val (ivy-read "pattern: " 'counsel-git-grep-function))
        lst)
    (when val
      (setq lst (split-string val ":"))
      (find-file (car lst))
      (goto-char (point-min))
      (forward-line (1- (string-to-number (cadr lst)))))))
(global-set-key (kbd "M-'") 'counsel-git-grep)

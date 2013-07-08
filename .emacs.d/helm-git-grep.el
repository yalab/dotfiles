(eval-when-compile (require 'cl))
(require 'vc-git)
(provide 'helm-git-grep)

(defvar helm-c-source-git-grep-cache nil "path")

(defun helm-git-grep-init ()
  (setq helm-c-source-git-grep-cache
        (vc-git-root (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory))))

(defun helm-git-grep-process ()
  (if helm-c-source-git-grep-cache
      (let ((default-directory helm-c-source-git-grep-cache))
        (apply 'start-process "git-grep-process" nil
               "git" "--no-pager" "grep" "--full-name" "-n" "--no-color"
               (nbutlast
                (apply 'append
                       (mapcar
                        (lambda (x) (list "-e" x "--and"))
                        (split-string helm-pattern "[ \t]" t))))))
    '()))

(defun helm-git-submodule-grep-process ()
  (if helm-c-source-git-grep-cache
      (let ((default-directory helm-c-source-git-grep-cache))
        (start-process-shell-command
         "git-submodule-grep-process" nil
         "git" "--no-pager" "submodule" "--quiet" "foreach"
         (format "'git grep --full-name -n --no-color %s | sed s!^!$path/!'"
                 (mapconcat (lambda (x)
                              (format "-e %s " (shell-quote-argument x)))
                            (split-string helm-pattern "[ \t]" t)
                            "--and "))))
    '()))

(defun helm-git-grep-transformer (cds source)
  (mapcar (lambda (candidate)
            (let ((list (split-string candidate ":")))
              (if (not (>= (length list) 3))
                  candidate
                (let ((file-name (first list))
                      (line-number (second list))
                      (line (apply 'concat (cddr list))))
                  (cons (format "%s:%s:\n  %s" file-name line-number line)
                        candidate)))))
          cds))

(defun helm-git-grep-goto (candidate)
  (let ((list (split-string candidate ":")))
    (when (>= (length list) 3)
      (let ((top-dir helm-c-source-git-grep-cache)
            (file-name (first list))
            (line-number (second list)))
        (find-file (file-truename (expand-file-name file-name  top-dir)) top-dir)
        (goto-line (string-to-number line-number))))))

(defvar helm-c-source-git-grep
  '((name . "Git Grep")
    (multiline)
    (init . helm-git-grep-init)
    (candidates . helm-git-grep-process)
    (filtered-candidate-transformer helm-git-grep-transformer)
    (action . (("Git Grep Goto " . helm-git-grep-goto)))
    (candidate-number-limit . 300)
    (requires-pattern . 3)
    (volatile)
    (delayed)))

(defvar helm-c-source-git-submodule-grep
  '((name . "Git Submodule Grep")
    (multiline)
    (init . helm-git-grep-init)
    (candidates . helm-git-submodule-grep-process)
    (filtered-candidate-transformer helm-git-grep-transformer)
    (action . (("Git Submodule Grep Goto " . helm-git-grep-goto)))
    (requires-pattern . 3)
    (volatile)
    (delayed)))

(defun helm-git-grep ()
  "Helm Git Grep"
  (interactive)
  (helm-other-buffer 'helm-c-source-git-grep "*helm git grep*"))

(defun helm-git-submodule-grep ()
  "Helm Git Submodule Grep"
  (interactive)
  (helm-other-buffer 'helm-c-source-git-submodule-grep "*helm git submodule grep*"))


;; (eval-when-compile (require 'cl))
;; (provide 'helm-git-grep)

;; (helm-c-grep-default-command "git grep -n%cH --full-name -e %p %f")

;; (defvar helm-c-source-git-grep
;;   `((name . "Git grep")
;;     (init . helm-ls-git-init)
;;     (candidates-in-buffer)
;;     (keymap . ,helm-generic-files-map)
;;     (filtered-candidate-transformer . helm-ls-git-transformer)
;;     (action-transformer helm-c-transform-file-load-el)
;;     (action . ,(cdr (helm-get-actions-from-type helm-c-source-locate)))))

;; ;; (defvar helm-c-source-git-grep
;; ;;   '((name . "Git Grep")
;; ;;     (multiline)
;; ;;     (init . helm-git-grep-init)
;; ;;     (candidates . helm-git-grep-process)
;; ;;     (filtered-candidate-transformer helm-git-grep-transformer)
;; ;;     (action . (("Git Grep Goto " . helm-git-grep-goto)))
;; ;;     (candidate-number-limit . 300)
;; ;;     (requires-pattern . 3)
;; ;;     (volatile)
;; ;;     (delayed)))



;; (defun helm-git-grep ()
;;   "Helm Git Grep"
;;   (interactive)
;;   (helm-other-buffer 'helm-c-source-git-grep "*helm git grep*"))

(defun ylb-strkeytosym (start end)
  "ruby Hash key string to symbol"
  (interactive "r")
  (replace-regexp "\"\\([a-z_]+\\)\"=>" "\\1: " nil start end))

(defun ylb-hashkeyasjson (start end)
  "ruby Hash key json style"
  (interactive "r")
  (replace-regexp ":\\([a-z_]+\\) *=>" "\\1:" nil start end))


(defun ylb-htmlopttoerb (start end)
  "html attreibute to erb option"
  (interactive "r")
  (replace-regexp " \\([a-z_]+\\)=" ", \\1: " nil start end))

(defun ylb-strip-unused-html-attributes (start end)
  "remove unused html attributes"
  (interactive "r")
  (funcall (lambda ()
             (replace-regexp "\\(value\\|style\\|type\\): +\"[^\"]*\"[, ]?" "" nil start end)
             )))


(defun ylb-inputtoerb (start end)
  "input tag to erb"
  (interactive "r")
  (save-excursion
    (funcall (lambda ()
               (replace-regexp "value=\"[^\"]+\"" "" nil start end)
               (replace-regexp "input_" "" nil start end)
               (replace-regexp
                "<input\\([^>]+\\) ?name=\"\\([0-9a-z_]+\\)\"\\([^/>]+\\)/?>"
                "<%= f.text_field :\\2\\1\\3%>" nil start end)
               (call-interactively 'ylb-htmlopttoerb)
               (call-interactively 'ylb-strip-unused-html-attributes)))))

(defun ylb-optiontoyaml (start end)
  "strip option tags"
  (interactive "r")
  (funcall (lambda ()
             (replace-regexp
              "<option[^v]+value=\\(\"[0-9]+\"\\)>\\([^<]+\\)</option>"
              "\\1: \\2")
             (call-interactively 'indent-region))))

(defun ylb-ivar-to-let (start end)
 "instance_variable to let"
 (interactive "r")
 (funcall (lambda ()
            (replace-regexp "\n +" "" nil start end)
            (move-end-of-line nil)
            (replace-regexp "\@\\([^ ]+\\) = \\([^\n]*\\)" "let(:\\1){ \\2 }" nil start (point))
            (replace-regexp "," ", " nil start (point))
            )))


(defun ylb-php-var-to-erb (start end)
  "php variable to erb"
  (interactive "r")
  (replace-regexp "{.+input_\\([a-z_]+\\).+}" "<%= @applicant.\\1 %>" nil start end))


(defun ylb-field-to-show (start end)
  "php variable to erb"
  (interactive "r")
  (replace-regexp "form[^:]+:\\([^,]+\\)[^%]+" "@feature_page.\\1 " nil start end))

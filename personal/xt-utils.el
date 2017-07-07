(defun xt-create-workspaces (number)
  "Create n fullscreen workspaces."
  (interactive "p")
  (dotimes (i number)
    (toggle-frame-fullscreen)
    (sleep-for 1)
    (make-frame-command))
  (toggle-frame-fullscreen))

(defun xt-company-diag ()
  (interactive)
  (let* ((bb company-backends)
         backend
         (prefix (cl-loop for b in bb
                          thereis (let ((company-backend b))
                                    (setq backend b)
                                    (company-call-backend 'prefix))))
         cc)
    (when (stringp prefix)
      (setq cc (let ((company-backend backend))
                 (company-call-backend 'candidates prefix))))
    (pop-to-buffer (get-buffer-create "*company-diag*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "company-backends: " (pp-to-string bb))
    (insert "\n")
    (insert "Used backend: " (pp-to-string backend))
    (insert "\n")
    (insert "Prefix: " (pp-to-string prefix))
    (insert "\n")
    (insert (message  "Candidates number: %i" (length cc)))
    (special-mode)))

(defun replace-pretty-quotes ()
  "Replace pretty quotes with standard quotes."
  (interactive)
  (replace-regexp-and-return "[”“]" "\""))

(defun edit-util-functions ()
  "Open handy-functions.el for editing."
  (interactive)
  (find-file "~/.emacs.d/personal/xt-utils.el"))

(defun flush-blank-lines ()
  "Flush blank lines."
  (interactive)
  (flush-lines "^\s*$" nil nil t))

(defun snippy-comment ()
  "Insert a snip line - - 8< - - - comment."
  (interactive)
  (end-of-line)
  (newline)
  (insert "- - 8<")
  (cl-loop repeat 60 do (insert " -"))
  (beginning-of-line)
  (comment-region (point-at-bol) (point-at-eol)))


(provide 'xt-utils)

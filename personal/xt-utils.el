(defun xt-create-workspaces (number)
  "Create n fullscreen workspaces."
  (interactive "p")
  (dotimes (i number)
    (toggle-frame-fullscreen)
    (sleep-for 1)
    (make-frame-command))
  (toggle-frame-fullscreen))


(provide 'xt-utils)

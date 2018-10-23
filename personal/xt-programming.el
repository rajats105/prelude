(prelude-require-packages '(multiple-cursors realgud smart-forward auto-yasnippet))

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-word-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-word-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-words-like-this)

(global-set-key (kbd "s-<up>") 'smart-up)
(global-set-key (kbd "s-<down>") 'smart-down)
(global-set-key (kbd "s-<left>") 'smart-backward)
(global-set-key (kbd "s-<right>") 'smart-forward)

(global-set-key (kbd "M-c") #'aya-create)
(global-set-key (kbd "M-y") #'aya-expand)

(provide 'xt-programming)

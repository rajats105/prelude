(prelude-require-package 'realgud)
(require 'realgud)

(setq tab-width 4)


(prelude-require-package 'highlight-symbol)

(add-hook 'prog-mode-hook #'highlight-symbol-mode)
(add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)

;; (prelude-require-package 'highlight-indent-guides)
;; (prelude-require-package 'highlight-parentheses)

(prelude-require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;; Rainbow Delimiters
(prelude-require-package 'rainbow-identifiers)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

(prelude-require-package 'indent-guide)
(add-hook 'prog-mode-hook 'indent-guide-mode)

;; (global-set-key (kbd "C-q") 'isearch-forward-symbol-at-point)

;; put windows side by side
(setq ediff-split-window-function 'split-window-horizontally)

;;revert windows on exit - needs winner mode
;; (remove-hook 'ediff-after-quit-hook-internal 'winner-undo)

(setq whitespace-line-column 160)

(prelude-require-package 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-word-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-word-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-words-like-this)


(prelude-require-package 'smart-forward)
(require 'smart-forward)

(global-set-key (kbd "s-<up>") 'smart-up)
(global-set-key (kbd "s-<down>") 'smart-down)
(global-set-key (kbd "s-<left>") 'smart-backward)
(global-set-key (kbd "s-<right>") 'smart-forward)

(prelude-require-package 'helm-swoop)

(prelude-require-package 'restclient)
(prelude-require-package 'know-your-http-well)

(setq-default indent-tabs-mode nil)
(setq-default standard-indent 4)
(setq-default css-indent-offset 4)
(setq-default python-indent 4)
(setq-default js-indent-level 4)


(defun newline-for-code ()
  "Inserts a newline character, but from the end of the current line."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "M-RET") 'newline-for-code)

;; Clojure indentation
(setq clojure-indent-style :always-indent)

;; http://emacs.stackexchange.com/questions/3458/how-to-switch-between-windows-quickly
(windmove-default-keybindings)

(provide 'xt-programming)

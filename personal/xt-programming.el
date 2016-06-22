(prelude-require-package 'realgud)
(require 'realgud)

(setq tab-width 4)


(prelude-require-package 'highlight-symbol)

(add-hook 'prog-mode-hook #'highlight-symbol-mode)
(add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)

(prelude-require-package 'dumb-jump)
(require 'dumb-jump)

;; (prelude-require-package 'highlight-indent-guides)
;; (prelude-require-package 'highlight-parentheses)

(prelude-require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;; Rainbow Delimiters
(prelude-require-package 'rainbow-identifiers)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)


;; (global-set-key (kbd "C-q") 'isearch-forward-symbol-at-point)

;; put windows side by side
(setq ediff-split-window-function 'split-window-horizontally)

;;revert windows on exit - needs winner mode
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)



(provide 'xt-programming)

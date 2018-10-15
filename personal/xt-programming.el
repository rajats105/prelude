(prelude-require-package 'realgud)
(require 'realgud)

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

;;revert windows on exit - needs winner mode
;; (remove-hook 'ediff-after-quit-hook-internal 'winner-undo)

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


;; Whitelist helm ag results buffers
(setq helm-white-buffer-regexp-list '("\\*helm ag results"))

;; https://github.com/ShingoFukuyama/helm-swoop
(prelude-require-package 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
;; (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
;; (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
;; (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
;; (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
;; (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
;; (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
;; (setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
;; (setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

;; If you prefer fuzzy matching
(setq helm-swoop-use-fuzzy-match t)

(defun newline-for-code ()
  "Inserts a newline character, but from the end of the current line."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "M-RET") 'newline-for-code)

;; http://emacs.stackexchange.com/questions/3458/how-to-switch-between-windows-quickly
(windmove-default-keybindings)

(prelude-require-package 'restclient)
(prelude-require-package 'know-your-http-well)
(prelude-require-package 'nocomments-mode)
(prelude-require-package 'nginx-mode)

(prelude-require-package 'auto-yasnippet)

(global-set-key (kbd "M-c") #'aya-create)
(global-set-key (kbd "M-y") #'aya-expand)

(provide 'xt-programming)

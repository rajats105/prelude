(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq helm-recentf-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-locate-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)
(setq helm-semantic-fuzzy-match t)
(setq helm-imenu-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)

(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)


;; Disable guru mode
(setq prelude-guru nil)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Moving-Point.html
(setq line-move-visual nil)


(prelude-require-package 'paradox)


;; Allow access from emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))


(provide 'xt-defaults)

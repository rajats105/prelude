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

;; https://github.com/syohex/emacs-helm-ag#helm-ag-insert-at-pointdefault-nil
(setq helm-ag-insert-at-point 'symbol)
;; (setq helm-ag-fuzzy-match t)
(setq helm-ag-use-agignore t)


;; Disable guru mode
(setq prelude-guru nil)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Moving-Point.html
(setq line-move-visual nil)

;; (prelude-require-package 'paradox)

(prelude-require-package 'key-chord)

;; Most important functions I use
(key-chord-define-global "BB" 'helm-mini)
(key-chord-define-global "FF" 'helm-projectile-find-file)
(key-chord-define-global "PP" 'helm-projectile-switch-project)
(key-chord-define-global "GG" 'helm-projectile-ag)
(key-chord-define-global "II" 'helm-semantic-or-imenu)

(prelude-require-package 'zop-to-char)

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; Fixes C-SPC not working with C-=
;; https://github.com/magnars/expand-region.el/issues/220
(setq shift-select-mode nil)


;; Allow access from emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

(diminish 'editorconfig-mode)
(diminish 'helm-gtags-mode)
(diminish 'highlight-symbol-mode)
(diminish 'whitespace-mode)
(diminish 'smartparens-mode)
(diminish 'indent-guide-mode)
(diminish 'abbrev-mode)
(diminish 'which-key-mode)
(diminish 'prelude-mode)
(diminish 'helm-mode)
(diminish 'winner-mode)
(diminish 'company-mode)

(provide 'xt-defaults)

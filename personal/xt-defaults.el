(prelude-require-package 'key-chord)

;; Most important functions I use
(key-chord-define-global "BB" 'helm-mini)
(key-chord-define-global "FF" 'helm-projectile-find-file)
(key-chord-define-global "PP" 'helm-projectile-switch-project)
(key-chord-define-global "GG" 'helm-projectile-ag)
(key-chord-define-global "II" 'helm-semantic-or-imenu)

;; Help Fuzzy matching
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

(provide 'xt-defaults)

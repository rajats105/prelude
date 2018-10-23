(prelude-require-package 'key-chord)

;; Most important functions I use
(key-chord-define-global "BB" 'helm-mini)
(key-chord-define-global "FF" 'helm-projectile-find-file)
(key-chord-define-global "PP" 'helm-projectile-switch-project)
(key-chord-define-global "GG" 'helm-projectile-ag)
(key-chord-define-global "II" 'helm-semantic-or-imenu)

(provide 'xt-defaults)

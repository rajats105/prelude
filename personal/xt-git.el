(prelude-require-package 'use-package)

(use-package git-messenger
             :ensure t :defer t
             :bind (("C-x v m" . git-messenger:popup-message)))

(prelude-require-package 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.tern-project\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.tern-config\\'" . js-mode))

;; (setq js-indent-level 2)
;; (setq js2-indent-level 2)

;; http://writequit.org/org/settings.html#sec-1-3-12
(js2-imenu-extras-setup)
(setq-default js-auto-indent-flag nil
              js-indent-level 2)

(setq inferior-js-program-command "node")

(prelude-require-package 'tern)
(require 'tern)

(prelude-require-package 'company-tern)
(require 'company-tern)

(add-to-list 'company-backends 'company-tern)

;; (add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
;; (add-hook 'js3-mode-hook (lambda () (tern-mode t)))

(prelude-require-package 'nvm)
(require 'nvm)

(nvm--installed-versions)
(nvm-use "v6.4.0")

(prelude-require-package 'rvm)
(require 'rvm)

(provide 'xt-js)

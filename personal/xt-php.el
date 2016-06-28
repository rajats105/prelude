(prelude-require-package 'php-mode)

(prelude-require-package 'smarty-mode)

(setq php-lineup-cascaded-calls t)

;;phpcs settings
(setq flycheck-php-phpcs-executable "/usr/local/bin/phpcs")
(setq flycheck-phpcs-standard (expand-file-name "~/.emacs.d/personal/misc/php/phpcs.xml"))

;;phpmd settings
(setq flycheck-php-phpmd-executable "/usr/local/bin/phpmd")
(setq flycheck-phpmd-rulesets '())
(add-to-list 'flycheck-phpmd-rulesets (expand-file-name "~/.emacs.d/personal/misc/php/phpmd.xml"))

(add-hook 'php-mode-hook 'dumb-jump-mode)

(provide 'xt-php)

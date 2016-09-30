(prelude-require-package 'php-mode)

;; (prelude-require-package 'smarty-mode)

(setq php-lineup-cascaded-calls t)

;;phpcs settings
;; (setq flycheck-php-phpcs-executable "/usr/local/bin/phpcs")
(setq flycheck-phpcs-standard (expand-file-name "~/.emacs.d/personal/misc/php/phpcs.xml"))

;;phpmd settings
;; (setq flycheck-php-phpmd-executable "/usr/local/bin/phpmd")
(setq flycheck-phpmd-rulesets '())
(add-to-list 'flycheck-phpmd-rulesets (expand-file-name "~/.emacs.d/personal/misc/php/phpmd.xml"))

(prelude-require-package 'dumb-jump)
(require 'dumb-jump)
;; (add-hook 'php-mode-hook 'dumb-jump-mode)

;; (prelude-require-package 'company-php)
;; (require 'company-php)

;; (add-hook 'php-mode-hook
;;           '(lambda ()
;;              (require 'company-php)
;;              (company-mode t)
;;              (add-to-list 'company-backends 'company-ac-php-backend )
;;              (define-key php-mode-map  (kbd "M-.") 'ac-php-find-symbol-at-point)
;;              (define-key php-mode-map  (kbd "M-,") 'ac-php-location-stack-back)))

(add-hook 'php-mode-hook 'php-enable-symfony2-coding-style)

(provide 'xt-php)

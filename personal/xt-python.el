(setq python-indent-guess-indent-offset nil)

;; (prelude-require-package 'py-yapf)

;; (setq py-yapf-options '("--style" "google"))
;; (setq py-yapf-options nil)

;; (add-hook 'python-mode-hook 'py-yapf-enable-on-save)

(prelude-require-package 'pyvenv)

(setenv "WORKON_HOME" "/Users/rajat/anaconda2/envs")
(pyvenv-mode 1)

(pyvenv-workon "cm-cobra")

(add-hook 'python-mode-hook 'eldoc-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(flycheck-add-next-checker 'python-flake8 'python-pylint)

(eval-after-load 'anaconda-mode
  '(progn
     (define-key anaconda-mode-map (kbd "M-,") 'anaconda-mode-go-back)
     (define-key anaconda-mode-map (kbd "M-/") 'anaconda-mode-find-assignments)))



(provide 'xt-python)

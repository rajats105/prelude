(prelude-require-package 'helm-gtags)

;; Set key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))


;; customize
(setq helm-gtags-path-style 'relative)
(setq helm-gtags-ignore-case t)

;; If this variable is non-nil, TAG file is updated after saving buffer
(setq helm-gtags-auto-update t)

;; Tags are updated in `after-save-hook' if this seconds is passed from last update Always update if value of this variable is nil.
(setq helm-gtags-update-interval-second 5)

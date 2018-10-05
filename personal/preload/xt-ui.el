;;; xt-ui.el --- UI related
;;
;; Copyright (c) 2016 Xitkov
;;

;;; Commentary:

;;; License:

;;; Code:

;; choose your own fonts, in a system dependant way
(if (string-match "apple-darwin" system-configuration)
    (set-face-font 'default "Source Code Pro-13")
  (set-face-font 'default "Source Code Pro-11"))

(if (string-match "apple-darwin" system-configuration)
    (set-frame-font "Source Code Pro-13")
  (set-frame-font "Source Code Pro-11"))

;; Auto hide the menu bar
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(when (eq system-type 'darwin)
  (progn
    ;; http://stackoverflow.com/questions/20405433/how-to-force-emacs-not-use-mountain-lions-full-screen-style
    (setq ns-auto-hide-menu-bar nil)
    ;; Dont use the native full screen method
    (setq ns-use-native-fullscreen nil)

    ;; Anti alias the text in Mac OS
    (setq ns-antialias-text t)

    ;; set keys for Apple keyboard, for emacs in OS X
    (setq mac-option-key-is-meta nil)
    (setq mac-command-key-is-meta t)

    (setq mac-command-modifier 'meta)      ; make cmd key do Meta
    (setq mac-option-modifier 'super)      ; make opt key do Super
    (setq mac-control-modifier 'control)   ; make Control key do Control
    (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

    ;; Allow mac anti aliasing
    (setq mac-allow-anti-aliasing t)
    (setq default-input-method "MacOSX")
    ;; Make mouse wheel / trackpad scrolling less jerky
    ;; (setq mouse-wheel-scroll-amount '(1
    ;;                                   ((shift) . 5)
    ;;                                   ((control))))
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
    (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
    (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
    (dolist (multiple '("" "double-" "triple-"))
      (dolist (direction '("right" "left"))
        (global-set-key (kbd (concat "<" multiple "wheel-" direction ">")) 'ignore)))

    (global-set-key (kbd "M-`") 'ns-next-frame)
    (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
    (global-set-key (kbd "M-˙") 'ns-do-hide-others)
    ;; what describe-key reports for cmd-option-h
    (global-set-key (kbd "M-ˍ") 'ns-do-hide-others)))

;;(sleep-for 1)

(provide 'xt-ui)

;;; xt-ui.el ends here

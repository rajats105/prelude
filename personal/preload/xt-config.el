;; put windows side by side
(setq ediff-split-window-function 'split-window-horizontally)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Moving-Point.html
(setq line-move-visual nil)

;; Disable guru mode
(setq prelude-guru nil)

;; Fixes C-SPC not working with C-=
;; https://github.com/magnars/expand-region.el/issues/220
(setq shift-select-mode nil)

(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-r") #'isearch-backward-regexp)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(while custom-enabled-themes
  (disable-theme (car custom-enabled-themes)))

;; (disable-theme 'zenburn)

(setq prelude-theme 'zenburn)

(setq zenburn-override-colors-alist
  '(("zenburn-fg-1"     . "#767676")
    ("zenburn-fg-05"    . "#a8a8a8")
    ("zenburn-fg"       . "#d0d0d0")
    ("zenburn-fg+1"     . "#e0e0e0")
    ("zenburn-fg+2"     . "#eeeeee")
    ("zenburn-bg-2"     . "#121212")
    ("zenburn-bg-1"     . "#262626")
    ("zenburn-bg-08"    . "#3a3a3a")
    ("zenburn-bg-05"    . "#4e4e4e")
    ("zenburn-bg"       . "#000000")
    ("zenburn-bg+05"    . "#121212")
    ("zenburn-bg+1"     . "#262626")
    ("zenburn-bg+2"     . "#3a3a3a")
    ("zenburn-bg+3"     . "#4e4e4e")
    ("zenburn-red-6"    . "#5f0000")
    ("zenburn-red-5"    . "#870000")
    ("zenburn-red-4"    . "#af0000")
    ("zenburn-red-3"    . "#d70000")
    ("zenburn-red-2"    . "#d70000")
    ("zenburn-red-1"    . "#ef0000")
    ("zenburn-red"      . "#ff5f5f")
    ("zenburn-red+1"    . "#ff8787")
    ("zenburn-red+2"    . "#ff8787")
    ("zenburn-orange"   . "#00af00")
    ;; ("zenburn-yellow-2" . "#af8700")
    ;; ("zenburn-yellow-1" . "#d7af00")
    ;; ("zenburn-yellow"   . "#ffd700")
    ("zenburn-yellow-2" . "#5f0087")
    ("zenburn-yellow-1" . "#8700d7")
    ("zenburn-yellow" . "#875fff")
    ("zenburn-green-5"  . "#005f00")
    ("zenburn-green-4"  . "#5f8700")
    ("zenburn-green-3"  . "#008700")
    ("zenburn-green-2"  . "#87af00")
    ("zenburn-green-1"  . "#5fd700")
    ("zenburn-green"    . "#00af00")
    ("zenburn-green+1"  . "#87d75f")
    ("zenburn-green+2"  . "#5fff5f")
    ("zenburn-green+3"  . "#5fff00")
    ("zenburn-green+4"  . "#afff87")
    ("zenburn-cyan"     . "#00d7ff")
    ("zenburn-blue+3"   . "#5fd7ff")
    ("zenburn-blue+2"   . "#00d7ff")
    ("zenburn-blue+1"   . "#00afff")
    ("zenburn-blue"     . "#0087ff")
    ("zenburn-blue-1"   . "#005fff")
    ("zenburn-blue-2"   . "#0000ff")
    ("zenburn-blue-3"   . "#0000af")
    ("zenburn-blue-4"   . "#000087")
    ("zenburn-blue-5"   . "#00005f")
    ("zenburn-magenta"  . "#ff87d7")))

;; (load-theme 'zenburn t)

;; choose your own fonts, in a system dependant way
(if (string-match "apple-darwin" system-configuration)
    (progn
      (set-face-font 'default "Source Code Pro-12")
      (set-frame-font "Source Code Pro-12" nil t)
      (add-to-list 'default-frame-alist '(font . "Source Code Pro-12")))
  (progn
    (set-face-font 'default "Source Code Pro-11")
    (set-frame-font "Source Code Pro-11" nil t)
    (add-to-list 'default-frame-alist '(font . "Source Code Pro-11"))))

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

;; (defvar my:ediff-frame nil)
(frame-parameter nil 'my:ediff-frame)

(defun my:command-line-diff (switch)
  (let* ((file-a (pop command-line-args-left))
         (file-b (pop command-line-args-left)))
    (add-hook 'ediff-quit-hook 'kill-emacs)
    (ediff-files file-a file-b)))

(defun my:command-line-merge (switch)
  (let* ((file-a (pop command-line-args-left))
         (file-b (pop command-line-args-left))
         (file-out (pop command-line-args-left))
         (file-ancestor (pop command-line-args-left)))
    (add-hook 'ediff-quit-hook 'kill-emacs)
    (if (and file-ancestor
             (file-exists-p file-ancestor)
             (file-readable-p file-ancestor))
        (ediff-merge-files-with-ancestor file-a file-b file-ancestor nil file-out)
      (ediff-merge-files file-a file-b nil file-out))))

(add-to-list 'command-switch-alist '("diff" . my:command-line-diff))
(add-to-list 'command-switch-alist '("merge" . my:command-line-merge))

(defun my:ediff-cmd-register-frame (&optional frame)
  (set-frame-parameter nil 'my:ediff-frame (or frame (selected-frame))))

(defun my:ediff-cmd-cleanup-frame ()
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (when (and (boundp 'my:ediff-frame)
                 (framep my:ediff-frame)
                 (equal frame my:ediff-frame))
        (delete-frame my:ediff-frame)
        (set-frame-parameter nil 'my:ediff-frame nil)))))

(defun my:ediff-cmd (file-a file-b)
  (add-hook 'ediff-quit-hook #'my:ediff-cmd-cleanup-frame)
  (my:ediff-cmd-register-frame (selected-frame))
  (ediff-files file-a file-b))

(defun my:emerge-cmd (file-a file-b file-out &optional file-ancestor)
  (add-hook 'ediff-quit-hook #'my:ediff-cmd-cleanup-frame)
  (my:ediff-cmd-register-frame (selected-frame))
  (if (and file-ancestor
           (file-exists-p file-ancestor)
           (file-readable-p file-ancestor))
      (ediff-merge-files-with-ancestor file-a file-b file-ancestor nil file-out)
    (ediff-merge-files file-a file-b nil file-out)))


(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(provide 'xt-config)

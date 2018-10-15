;; Set variables used

(setq tab-width 4)

(setq-default indent-tabs-mode nil)
(setq-default standard-indent 4)
(setq-default python-indent 4)

;; Clojure indentation
(setq clojure-indent-style :always-indent)

(setq whitespace-line-column 240)

;; put windows side by side
(setq ediff-split-window-function 'split-window-horizontally)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Moving-Point.html
(setq line-move-visual nil)

;; Disable guru mode
;; (setq prelude-guru nil)
(setq prelude-guru t)

;; Fixes C-SPC not working with C-=
;; https://github.com/magnars/expand-region.el/issues/220
(setq shift-select-mode nil)

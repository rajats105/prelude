;;; prelude-php.el --- Emacs Prelude: A nice setup for Ruby (and Rails) devs.
;;
;; Copyright © 2011-2018 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for Ruby and Rails development.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'prelude-programming)
(require 'prelude-tags)

(prelude-require-packages '(php-mode))

(with-eval-after-load 'php-mode
  (defun prelude-php-mode-defaults ()
    (setq php-lineup-cascaded-calls t)
    (helm-gtags-mode 1)
    (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
    (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
    (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
    (subword-mode +1)
    (add-to-list (make-local-variable 'company-backends)
                 'company-gtags)))

(setq prelude-php-mode-hook 'prelude-php-mode-defaults)
(add-hook 'php-mode-hook (lambda () (run-hooks 'prelude-php-mode-hook)))

(provide 'prelude-php)
;;; prelude-php.el ends here

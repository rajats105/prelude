;;; prelude-tags.el --- Emacs Prelude: A nice setup for Ruby (and Rails) devs.
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

(require 'prelude-helm)
(require 'prelude-programming)

(prelude-require-packages '(helm-gtags))

(add-to-list 'prelude-diminish-list 'helm-gtags-mode)

(with-eval-after-load 'helm-gtags
  (defun prelude-helm-gtags-mode-defaults ()
    (setq helm-gtags-path-style 'relative)
    (setq helm-gtags-ignore-case t)
    (setq helm-gtags-auto-update t)
    (setq helm-gtags-update-interval-second 5)))

(setq prelude-helm-gtags-mode-hook 'prelude-helm-gtags-mode-defaults)
(add-hook 'helm-gtags-mode-hook (lambda () (run-hooks 'prelude-helm-gtags-mode-hook)))

(provide 'prelude-tags)
;;; prelude-tags.el ends here

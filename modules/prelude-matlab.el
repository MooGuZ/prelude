;;; prelude-matlab.el --- configuration of emacs-matlab package
;;
;; MooGu Z. <hzhu@case.edu>
;; Feb 2016

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration shared between all modes related to MATLAB language.

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

(load-library "matlab-load")
;; setup matlab shell
(add-to-list 'exec-path "/Applications/MATLAB_R2014a.app/bin/")
(setq matlab-shell-command "matlab")
(setq matlab-shell-command-switches '("-nodisplay"))
;; default settings
(defun matlab-mode-default ()
  "Apply default settings of matlab mode."
  (turn-on-auto-fill)
  (setq fill-column 120)
  (linum-on)
  (smartparens-mode t)
  (smartparens-strict-mode t)
  (yas-minor-mode))
;; attach to mode hook
(add-hook 'matlab-mode-hook 'matlab-mode-default)

(provide 'prelude-matlab)
;;; prelude-matlab.el ends here

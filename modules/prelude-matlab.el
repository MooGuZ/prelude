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

(prelude-require-packages '(matlab-mode))

(load-library "matlab-load")
;; find maltab executive folder
(defconst matlab-exec-dir
  (concat (file-name-as-directory
           (car (directory-files "/Applications" t "[Mm][Aa][Tt][Ll][Aa][Bb]")))
          "bin"))
;; setup matlab shell
(add-to-list 'exec-path matlab-exec-dir)
(setq matlab-shell-command "matlab")
(setq matlab-shell-command-switches '("-nodisplay"))
;; default settings
(defun matlab-mode-default ()
  "Apply default settings of matlab mode."
  (turn-on-auto-fill)
  (setq fill-column 87)
  (linum-mode +1)
  (smartparens-mode t)
  (smartparens-strict-mode t))
;; attach to mode hook
(add-hook 'matlab-mode-hook 'matlab-mode-default)

(provide 'prelude-matlab)
;;; prelude-matlab.el ends here

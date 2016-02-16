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
                                        ; Enable CEDET feature support for MATLAB code
                                        ; (matlab-cedet-setup)
                                        ; Matlab Shell
(add-to-list 'exec-path "/Applications/MATLAB_R2014a.app/bin/")
(setq matlab-shell-command "matlab")
(setq matlab-shell-command-switches '("-nodisplay"))
                                        ; Matlab Mode
(add-hook 'matlab-mode-hook
          '(lambda()
             (setq fill-column 87)))

(provide 'prelude-matlab)

;;; prelude-ui.el --- Emacs Prelude: UI optimizations and tweaks.
;;
;; Copyright © 2011-2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; We dispense with most of the point and click UI, reduce the startup noise,
;; configure smooth scolling and a nice theme that's easy on the eyes (zenburn).

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

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Prelude - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; use zenburn as the default theme
;; (when prelude-theme
;;   (load-theme prelude-theme t))

(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
;; delegate theming to the currently active theme
(setq sml/theme nil)
(add-hook 'after-init-hook #'sml/setup)

;; show the cursor when moving after big movements in the window
(require 'beacon)
(beacon-mode +1)

;;; Customized Code:

;; setup default value of frame properties
(defconst display-info
  (car (display-monitor-attributes-list (car (last (terminal-list))))))
(defvar default-frame-left
  (nth 1 (assoc 'workarea display-info)))
(defvar default-frame-top
  (nth 2 (assoc 'workarea display-info)))
(defvar default-frame-width  83)
(defvar default-frame-height 43)
;; alternative fonts :
;; 1. Source Code Pro
;; 2. Menlo (Default)
;; 3. Courier
;; 4. Andale Mono
;; 5. Monaco
;; 6. Consolas
(defvar default-frame-font
  (if (> (nth 3 (assoc 'geometry display-info)) 1920)
      "Source Code Pro 15" "Menlo 14"))

;; assistant function to modify associate list
(defun modify-alist (alist id update)
  "Modify item in ALIST, which has key ID, to a new value UPDATE."
  (setf (cdr (assoc id alist)) update)
  alist)

;; initialize settings for MAKE-FRAME
(add-to-list 'default-frame-alist (cons 'left   default-frame-left))
(add-to-list 'default-frame-alist (cons 'top    default-frame-top))
(add-to-list 'default-frame-alist (cons 'width  default-frame-width))
(add-to-list 'default-frame-alist (cons 'height default-frame-height))
(add-to-list 'default-frame-alist (cons 'font   default-frame-font))

;; update settings of MAKE-FRAME when close current frame
(defun update-frame-setting (frame)
  "Update settings of 'make-frame' by settings of FRAME."
  (modify-alist default-frame-alist 'left   (frame-parameter frame 'left))
  (modify-alist default-frame-alist 'top    (frame-parameter frame 'top))
  (modify-alist default-frame-alist 'font   (frame-parameter frame 'font))
  (modify-alist default-frame-alist 'width  (frame-width frame))
  (modify-alist default-frame-alist 'height (frame-height frame)))
(add-hook 'delete-frame-functions 'update-frame-setting)

;; recover frame setting if there are multiple monitors. In this case
;; if last frame didn't located in main monitor. 'make-frame' cannot
;; create a frame in the position according to the recorded setting.
(defun recover-frame-setting (frame)
  "Move FRAME to the position in default settings in case of multiple monitors."
  (when (or (listp (cdr (assoc 'top  default-frame-alist)))
            (listp (cdr (assoc 'left default-frame-alist))))
    (modify-frame-parameters frame default-frame-alist)
    (print "applied recover-frame-setting")))
(add-hook 'after-make-frame-functions 'recover-frame-setting)

(provide 'prelude-ui)
;;; prelude-ui.el ends here

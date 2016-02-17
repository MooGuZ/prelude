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

;; frame setting save and restore in daemon mode
(defvar frame-width-record  83)
(defvar frame-height-record 43)
(defvar frame-font-record  nil)

(defun save-frame-setting (&optional f)
  "Save current frame (F) info into pre-defined variables."
  (setq frame-width-record (frame-width))
  (setq frame-height-record (frame-height))
  (setq frame-font-record
        (frame-parameter (selected-frame) 'font)))

(defun restore-frame-setting (f)
  "Restore frame setting from records to current frame F."
  (set-frame-size f frame-width-record frame-height-record)
  (set-frame-font frame-font-record nil (list f)))

(add-hook 'delete-frame-functions 'save-frame-setting)
(add-hook 'after-make-frame-functions 'restore-frame-setting)

;; initialize frame setting in daemon mode
(defun monitor-geoinfo (d)
  "Fetch geometry information of display D."
  (assq 'geometry (car (display-monitor-attributes-list d))))

(defun set-frame-font-acrd-display (d)
  "Set frame font according to display (D) resolution."
  (unless frame-font-record
    (setq frame-font-record
          (if (> (nth 3 (monitor-geoinfo d)) 1920)
              "Source Code Pro 15"
            "Menlo 14"))))
;; NOTE: the hook below have to be added after hook of 'restore-frame-setting'
(add-hook 'after-make-frame-functions 'set-frame-font-acrd-display)

;; initialize frame setting in non-daemon mode
(setq default-frame-alist '((width  . 83) (height . 43)))
;; alternative fonts :
;; 1. Source Code Pro
;; 2. Menlo (Default)
;; 3. Courier
;; 4. Andale Mono
;; 5. Monaco
;; 6. Consolas
(set-frame-font "Menlo 14")

(provide 'prelude-ui)
;;; prelude-ui.el ends here

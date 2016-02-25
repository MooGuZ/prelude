;;; prelude-org.el --- Emacs Prelude: org-mode configuration.
;;
;; Copyright © 2011-2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for org-mode.

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

(add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

(defun prelude-org-mode-defaults ()
  (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (yas-minor-mode)                    ; Custom Code START
    (turn-on-auto-fill)
    (setq fill-column 87)               ; Custom Code END
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-c +") nil)
    (define-key newmap (kbd "C-c -") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist))
)

(setq prelude-org-mode-hook 'prelude-org-mode-defaults)

(add-hook 'org-mode-hook (lambda () (run-hooks 'prelude-org-mode-hook)))

;;; Custom Code:
(setq org-log-mode 'time)
(setq org-startup-indented t)
(setq org-startup-truncated nil)
;; setup org-mode folders and agenda files
(setq org-archive-location "%s.archive")
(setq org-directory "~/Dropbox/Record/")
(setq org-agenda-files '("~/Dropbox/Record/general.org"
                         "~/Dropbox/Record/research.org"
                         "~/Dropbox/Record/dev.org"))
;; open all notes in startup
(defun open-notes ()
  "Open all notes under my org-directory."
  (interactive)
  (defvar last-buffer (current-buffer))
  (mapc 'find-file (directory-files org-directory t ".org$"))
  (switch-to-buffer "*scratch*"))
;; support MobileOrg
(setq org-mobile-inbox-for-pull (concat org-directory "inbox.org"))
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
;; setup shortcut for fetching and pushing to MobileOrg
(add-hook 'org-mode-hook (lambda ()
  (progn
    (define-key org-mode-map (kbd "C-c C-p") 'org-mobile-push)
    (define-key org-mode-map (kbd "C-c C-f") 'org-mobile-pull))))

;; ob-ipython
(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(provide 'prelude-org)

;;; prelude-org.el ends here

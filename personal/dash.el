;;; dash-at-point --- configuration

;;; Commentary:
;;  This configuration enable Emacs to search for API documnets with shortcut.

;;; Code:

(prelude-require-package 'dash-at-point)

(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)
(global-set-key "\M-pd" 'dash-at-point)
(global-set-key "\M-pe" 'dash-at-point-with-docset)

;;; dash.el ends here

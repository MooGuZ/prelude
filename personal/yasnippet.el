;;; yasnippet --- configuration

;;; Commentary:
;;  This configuration enable Emacs to use snippets.

;;; Code:

(prelude-require-package 'yasnippet)

(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook 'yas-minor-mode)

;;; yasnippet.el ends here

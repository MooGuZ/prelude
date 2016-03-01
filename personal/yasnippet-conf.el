;;; yasnippet-conf --- configuration

;;; Commentary:
;;  This configuration enable Emacs to use snippets.

;;; Code:

(prelude-require-package 'yasnippet)

(require 'yasnippet)

(add-hook 'prog-mode-hook 'yas-minor-mode)

(eval-after-load 'yasnippet
  (progn
    (define-key yas-minor-mode-map (kbd "s-i")   'yas-expand)
    (define-key yas-minor-mode-map [(tab)]       nil)
    (define-key yas-minor-mode-map (kbd "TAB")   nil)
    (define-key yas-minor-mode-map (kbd "<tab>") nil)))

;;; yasnippet-conf.el ends here

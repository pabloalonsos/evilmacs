;;; package --- Summary
;;; Commentary:

;;; Code:
(use-package spaceline-config
  :ensure spaceline
  :ensure fancy-battery
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (setq spaceline-window-numbers-unicode t)
  (setq powerline-default-separator 'utf-8)
  :config
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-battery-on)
  (spaceline-spacemacs-theme)
  (fancy-battery-mode)
  (spaceline-helm-mode))

(provide 'init-spaceline)
;;; init-spaceline.el ends here

;;; package --- Summary

;;; Commentary:

;;; Code:
(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)

  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)

  (use-package helm-projectile
    :ensure t
    :config
    (setq projectile-switch-project-action 'helm-projectile))

  (use-package helm-smex
    :ensure t))

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(provide 'init-helm)
;;; init-helm.el ends here

;;; package --- Summary

;;; Commentary:

;;; Code:

(defun hexo-www ()
  (interactive)
  (hexo "~/Code/www/pabloalonsos.github.com/"))

(use-package hexo
  :ensure t
  :bind (("C-c w" . hexo-www))
  :config
  (evil-define-key 'normal hexo-mode-map (kbd "C-c C-d") 'hexo-server-deploy)
  (evil-define-key 'normal hexo-mode-map (kbd "C-c C-r") 'hexo-server-run)
  (evil-define-key 'normal hexo-mode-map (kbd "C-c C-s") 'hexo-server-stop)
  (evil-define-key 'normal hexo-mode-map (kbd "C-c C-m") 'hexo-command-mark)
  (evil-define-key 'normal hexo-mode-map (kbd "C-c C-a") 'hexo-command-add-tags)
  (evil-define-key 'normal hexo-mode-map (kbd "C-c C-r") 'hexo-command-remove-tags)
  (evil-define-key 'normal hexo-mode-map (kbd "RET") 'hexo-command-open-file)
  (evil-define-key 'normal hexo-mode-map (kbd "SPC") 'hexo-command-show-article-info)
  (evil-leader/set-key-for-mode 'hexo-mode
    "n" 'hexo-new
    "r" 'hexo-command-rename-file
    "t" 'hexo-toggle-article-status))

(provide 'init-hexo)
;;; init-hexo ends here

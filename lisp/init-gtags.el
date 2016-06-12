;;; package -- Summary

;;; Commentary:

;;; Code:
;;
;; GTAGS
;;

(when (and (maybe-require-package 'gtags)
           (maybe-require-package 'bpr))
  (add-hook 'gtags-select-mode-hook
            (lambda ()
              (evil-define-key 'normal gtags-select-mode-map (kbd "RET") 'gtags-select-tag)
              (evil-define-key 'normal gtags-select-mode-map (kbd "q") 'kill-buffer-and-window)
              ))
  (defun gtags-reindex ()
    "Kick off gtags reindexing."
    (interactive)
    (let* ((root-path (expand-file-name (vc-git-root (buffer-file-name))))
           (gtags-filename (expand-file-name "GTAGS" root-path)))
      (if (file-exists-p gtags-filename)
        (gtags-index-update root-path)
        (gtgs-index-initial root-path))))

  (defun gtags-index-initial (path)
    "Generate initial GTAGS files for PATH."
    (let ((bpr-process-directory path))
      (bpr-spawn "gtags")))

  (defun gtags-index-update (path)
    "Update GTAGS in PATH."
    (let ((bpr-process-directory path))
      (prw-spawn "global -uv"))))

(provide 'init-gtags)
;;; init-gtags.el ends here

;;; package -- Summary:
;;; Commentary:
;;
;; Elfeed
;;

;;; Code:
(use-package elfeed
  :ensure t
  :init
  (setq elfeed-feeds
	'(("https://blog.thimbleweedpark.com/rss" games)
	  ("https://www.reddit.com/.rss" reddit)))
  :config                               ;
  (global-set-key (kbd "C-x w") 'elfeed))

(provide 'init-elfeed)
;;; init-elfeed.el ends here

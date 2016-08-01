;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook (lambda ()
                                 (linum-on)
                                 (rainbow-mode)))
  (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp))

(use-package clojure-mode-extra-font-locking
  :ensure t)

(use-package cider
  :ensure t)

(provide 'init-clojure)
;;; init-clojure.el ends here

;;; package --- Summary:

;;; Commentary:
;;
;;  Org Mode Configuration
;;
;; Assumes presence of Evil & Evil Leader

;;; Code:
;; Helper Functions
(defun pop-to-file (file &optional split)
  "Visit a FILE, either in the current window or a SPLIT."
  (if split
      (find-file-other-window file)
    (find-file file)))

(defun org-todo-capture ()
  "Capture a task with my default template."
  (interactive)
  (org-capture nil "t"))

(defun pop-to-org-todo (split)
  "Visit my main TODO list, in the current window or a SPLIT."
  (interactive "P")
  (pop-to-file "~/BTSync/org/todo.org" split))

(defun pop-to-org-notes (split)
  "Visit my main notes file, in the current window or a SPLIT."
  (interactive "P")
  (pop-to-file "~/BTSync/org/notes.org" split))

(defun org-agenda-toggle-date (current-line)
  "Toggle `SCHEDULED' and `DEADLINE' tag in the capture buffer."
  (interactive "P")
  (save-excursion
    (let ((search-limit (if current-line
			    (line-end-position)
			  (point-max))))

      (if current-line (beginning-of-line)
	(beginning-of-buffer))
      (if (search-forward "DEADLINE:" search-limit t)
	  (replace-match "SCHEDULED:")
	(and (search-forward "SCHEDULED:" search-limit t)
	     (replace-match "DEADLINE:"))))))

(defun org-insert-scheduled-heading (&optional force-heading)
  "Insert a new org heading scheduled for today.
Insert the new heading at the end of the current subtree if
FORCE-HEADING is non-nil."
  (interactive "P")
  (org-insert-todo-heading t force-heading)
  (org-schedule nil (format-time-string "%Y-%m-%d")))

(use-package org
  :ensure t
  :defer t
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-todo-capture)
	 ("C-c t" . pop-to-org-todo)
	 ("C-c n" . pop-to-org-notes))
  :config
  ;; Basic setup
  (setq org-directory "~/BTSync/org")
  (setq org-agenda-files '("~/BTSync/org/"))

  ;; Progress Logging
  (setq org-log-done 'time)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)

  ;; Display
  (setq org-blank-before-new-entry '((heading)
				     (plain-list-item)))

  ;; TODO Keywords
  (setq org-todo-keywords
	'((sequence "TODO(t)"
	 	    "IN-PROGRESS(p)"
	 	    "WAITING(w)"
	 	    "|"
	 	    "DONE(d)"
	 	    "CANCELED(c)")
	;; '((sequence "☛ TODO(t)"
	;; 	    "○ IN-PROGRESS(p)"
	;; 	    "⚑ WAITING(w)"
	;; 	    "|"
	;; 	    "✓ DONE(d)"
	;; 	    "✗ CANCELED(c)")
	  (sequence "TO-READ"
		    "READING"
		    "|"
		    "FINISHED")))

  (setq org-todo-keyword-faces
	'(("TODO" . "blue")
	  ("IN-PROGRESS" . "yellow")
	  ("WAITING(w)". "orange")
	  ("DONE" . "green")
	  ("CANCELED" . "red")))
	;; '(("☛ TODO" . "blue")
	;;   ("○ IN-PROGRESS" . "yellow")
	;;   ("⚑ WAITING(w)". "orange")
	;;   ("✓ DONE" . "green")
	;;   ("✗ CANCELED" . "red")))
  
  (setq org-enforce-todo-dependencies t)

  ;; TAGS
  (setq org-tag-alist '(
			;; where
			(:startgroup)
			("@home" .     ?h)
			("@work" .     ?w)
			("@out" .      ?o)
			("@computer" . ?c)
			(:endgroup)

			;; when
			(:startgroup)
			("@morning" .   ?M)
			("@afternoon" . ?A)
			("@evening" .   ?E)
			("@night" .     ?N)
			("@weekend" .   ?W)
			(:endgroup)

			;; type
			("email" .    ?e)
			("read" .     ?r)
			("neosavvy" . ?n)
			("phone" .    ?p)
			("home" .     ?H)))

  ;; Capture
  (setq org-capture-templates
	'(("t" "TODO task template." entry
	   (file "todo.org")
	   "* TODO []: %?
		      DEADLINE: %t")
	  ("n" "NOTE template." entry
	   (file "notes.org")
	   "* DATE: %t : %?")))

  ;; Agenda
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-text-search-extra-files '(agenda-archives))
  (setq org-agenda-custom-commands
      '(("g" . "GTD contexts")
	("gh" "Home" tags-todo "home")
	("gw" "Work" tags-todo "work")
        ("go" "Out" tags-todo "out")
        ("gc" "Computer" tags-todo "computer")
        ("G" "GTD Block Agenda"
         ((tags-todo "home")
          (tags-todo "work")
          (tags-todo "out")
          (tags-todo "computer"))
         nil                      ;; i.e., no local settings
         ("~/BTSync/org/next-actions.html")))) ;; exports block to this file with C-c a e

  (setq org-agenda-custom-commands
      '(("p" . "Priorities")
        ("pa" "A items" tags-todo "+PRIORITY=\"A\"")
        ("pb" "B items" tags-todo "+PRIORITY=\"B\"")
        ("pc" "C items" tags-todo "+PRIORITY=\"C\"")))

  (evil-leader/set-key-for-mode 'org-mode
    "a" 'org-agenda
    "s" 'org-schedule
    "t" 'org-show-todo-tree)

  (add-hook 'org-agenda-mode-hook
	    (lambda ()
	      (setq org-habit-graph-column 50)
	      (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
	      (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
	      (define-key org-agenda-mode-map "n" 'org-agenda-next-date-line)
	      (define-key org-agenda-mode-map "p" 'org-agenda-previous-date-line)
	      (define-key org-agenda-mode-map "c" 'org-agenda-capture)
	      (define-key org-agenda-mode-map "R" 'org-revert-all-org-buffers)
	      (define-key org-agenda-mode-map (kbd "RET")  'org-agenda-switch-to)))

  (add-hook 'org-capture-mode-hook
	    (lambda ()
	      (evil-define-key 'insert org-capture-mode-map (kbd "C-d") 'org-agenda-toggle-date)
              (evil-define-key 'normal org-capture-mode-map (kbd "C-d") 'org-agenda-toggle-date)
              (evil-insert-state)))

  (add-hook 'org-mode-hook
	    (lambda ()
	      (setq-local my-timer
                          (run-with-idle-timer 1 t
                                               (lambda ()
                                                 (when (and (eq major-mode 'org-mode)
                                                            (and evil-state
                                                                 (not (eq evil-state 'insert)))
                                                            (buffer-file-name)
							    (buffer-modified-p))
						   (save-buffer)))))
	      ;; Normal Maps
	      (define-key org-mode-map (kbd "C-c d") (lambda ()
						       (interactive)
						       (air-org-agenda-toggle-date t)))
	      (define-key org-mode-map (kbd "C-c ,") 'org-time-stamp-inactive)
              (define-key org-mode-map (kbd "C-|")   'air-org-insert-scheduled-heading)
              (define-key org-mode-map (kbd "C-<")   'org-metaleft)
              (define-key org-mode-map (kbd "C->")   'org-metaright)
              (define-key org-mode-map (kbd "C-\\")  'org-insert-heading)
              (define-key org-mode-map (kbd "S-r")   'org-revert-all-org-buffers)

	      (evil-define-key 'normal org-mode-map (kbd "TAB")   'org-cycle)
              (evil-define-key 'normal org-mode-map ">>"          'org-metaright)
              (evil-define-key 'normal org-mode-map "<<"          'org-metaleft)
              (evil-define-key 'normal org-mode-map (kbd "C-S-l") 'org-shiftright)
              (evil-define-key 'normal org-mode-map (kbd "C-S-h") 'org-shiftleft)
              (evil-define-key 'insert org-mode-map (kbd "C-S-l") 'org-shiftright)
              (evil-define-key 'insert org-mode-map (kbd "C-S-h") 'org-shiftleft)

	      ;; Navigation
              (evil-define-key 'normal org-mode-map (kbd "]n") 'org-forward-heading-same-level)
              (evil-define-key 'normal org-mode-map (kbd "[n") 'org-backward-heading-same-level)
              (define-key org-mode-map (kbd "C-S-j") (lambda ()
                                                       (interactive)
                                                       (org-up-element)
                                                       (org-forward-heading-same-level 1)))
              (define-key org-mode-map (kbd "C-S-k") 'org-up-element)

	      ;; Use fill column, but not in agenda
	      (setq fill-column 100)
	      (when (not (eq major-mode 'org-agenda-mode))
		(visual-line-mode)
		(visual-fill-column-mode))
	      (flyspell-mode)
	      (org-indent-mode))))

(use-package org-bullets
  :ensure t
  :config
  ;(setq org-bullets-bullet-list '("◉" "◎" "⚫" "○" "►" "◇"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'init-org)
;;; init-org.el ends here

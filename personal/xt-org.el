;;; xt-org.el --- org-mode configuration.
;;
;; Author: Xitkov
;; URL: https://github.com/xitkov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration for org-mode.

;;; License:

;;; Code:

;; http://doc.norang.ca/org-mode.html
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(setq org-agenda-files (list "~/rajat/org/todo"))

(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "STARTED(s)"
         "DELEGATED(g)"
         "WAITING(w@/!)"
         "SOMEDAY(.)"
         "|"
         "DONE(d@!)"
         "CANCELED(c@/!)")))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "indian red" :weight bold)
              ("STARTED" :foreground "cornflower blue" :weight bold)
              ("DELEGATED" :foreground "dark salmon" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "plum" :weight bold)
              ("SOMEDAY" :foreground "dark khaki" :weight bold)
              ("CANCELED" :foreground "forest green" :weight bold))))


(setq org-tag-alist '(("@work" . ?b)
                      ("@home" . ?h)
                      ("@writing" . ?w)
                      ("@errands" . ?e)
                      ("@drawing" . ?d)
                      ("@coding" . ?c)
                      ("@phone" . ?p)
                      ("@reading" . ?r)
                      ("@computer" . ?l)
                      ("quantified" . ?q)
                      ("fuzzy" . ?0)
                      ("highenergy" . ?1)))


(setq org-tag-alist '(("PROJECT" . ?p)
                      ("READING" . ?r)
                      ("JENN" . ?j)
                      ("QUESTION" . ?q)
                      ("OBSERVATION" . ?o)
                      ("IDEA" . ?i)
                      ("BUY" . ?y)
                      ("MEETING" . ?m)
                      ("PHONE" . ?h)
                      ("TaskBucket" . ?B)
                      ("brainstorm" . ?b)))

;; Otherwise, scheduled tasks will still be listed on that date after it has been marked DONE.
;; (setq org-agenda-skip-scheduled-if-done t)

(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer t)

(setq org-directory "~/rajat/org")


(setq org-default-notes-file (concat org-directory "/notes.org"))

;; (define-key global-map "\C-cc" 'org-capture)


(setq org-agenda-skip-scheduled-if-done t) ;; Scheduled done items should be skipped.
(setq org-agenda-skip-deadline-if-done t)  ;; Deadline tasks done should be skipped.
;;(setq org-agenda-skip-timestamp-if-done t) ;; Entries with any timestamp, appointments just like scheduled and deadline entries.


;; Don't show deadline tasks in global TODO list.
(setq org-agenda-todo-ignore-deadlines t)

;; Don't show any tasks with a date in the global TODO list.
(setq org-agenda-todo-ignore-with-date t)

;; Don't show scheduled tasks in the global TODO list.
(setq org-agenda-todo-ignore-scheduled t)




(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-agenda-dim-blocked-tasks t)



;; Tracking Time
(setq org-clock-idle-time nil)
(setq org-log-done 'note)
(setq org-clock-continuously nil)
(setq org-clock-persist t)
(setq org-clock-in-switch-to-state "STARTED")
(setq org-clock-in-resume nil)
(setq org-show-notification-handler 'message)
(setq org-clock-report-include-clocking-task t)



(setq org-habit-graph-column 80)
(setq org-habit-show-habits-only-for-today nil)



;;show me tasks scheduled or due in next fortnight
(setq org-agenda-span (quote fortnight))
;;(setq org-agenda-span 2)

;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)

(setq org-agenda-tags-column -100) ; take advantage of the screen width
(setq org-agenda-sticky nil)
(setq org-agenda-inhibit-startup t)
(setq org-agenda-use-tag-inheritance t)
(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

;;don't give awarning colour to tasks with impending deadlines
;;if they are scheduled to be done
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))


;;don't show tasks that are scheduled or have deadlines in the
;;normal todo list
(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))



(setq org-agenda-time-grid
      '((daily today require-timed)
        "----------------"
        (800 1000 1200 1400 1600 1800)))
(setq org-columns-default-format "%14SCHEDULED %Effort{:} %1PRIORITY %TODO %50ITEM %TAGS")


(setq org-agenda-window-setup 'current-window) ;;reorganize-frame ;; other-window

(setq org-blank-before-bullet t)


(setq org-blank-before-new-entry (quote ((heading . auto) (plain-list-item . auto))))

;;Resume persisted clock when loading emacs
(setq org-clock-in-resume t)

(setq org-clock-out-remove-zero-time-clocks t)

(setq org-clock-out-when-done t)

;;Persist clock data
(setq org-clock-persist t)

;; Cleaner view
(setq org-hide-leading-stars t)
(setq org-odd-levels-only t)


(setq org-enforce-todo-dependencies t)
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))


;;don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

(setq org-agenda-custom-commands
      '(("P" "Project List"
         ( (tags "PROJECT")
           )
         )
        ("O" "Office"
         ( (agenda)
           (tags-todo "OFFICE")
           )
         )
        ("W" "Weekly Plan"
         ( (agenda)
           (todo "TODO")
           (tags "PROJECT")
           )
         )
        ("H" "Home NA Lists"
         ( (agenda)
           (tags-todo "HOME")
           (tags-todo "COMPUTER")
           )
         )
        )
      )

;;sort tasks in order of when they are due and then by priority
(setq org-agenda-sorting-strategy
      (quote
       ((agenda deadline-up priority-down)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep))))


(setq xt-default-notes-file (concat org-directory "/notes.org"))

(setq org-capture-templates
      (quote (("t" "Todo" entry (file xt-default-notes-file)
               "* TODO %?\n  OPENED: %U\n %i")
              ("n" "Note" entry (file xt-default-notes-file)
               "* %?\n  OPENED: %U\n %i")
              ("j" "Journal" entry (file xt-default-notes-file)
               "* %?\n  OPENED: %U\n %i")
              ("h" "Habit" entry (file xt-default-notes-file)
               "* TODO %?\n  SCHEDULED: %t\n  OPENED: %U\n  :PROPERTIES:\n  :STYLE: habit\n  :END:\n  %i"))))


(global-set-key (kbd "C-c c") 'org-capture)

(provide 'xt-org)

;;; xt-org.el ends here

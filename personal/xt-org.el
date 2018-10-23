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

;; Cleaner view ;;
(setq org-hide-leading-stars t)
(setq org-hide-emphasis-markers t)
(setq org-odd-levels-only t)

(setq org-blank-before-bullet t)
(setq org-blank-before-new-entry (quote ((heading . always) (plain-list-item . nil))))


;; Dirs
(setq org-directory "~/.orgnote/")
(setq org-default-notes-file (concat org-directory "inbox.org"))
(setq org-agenda-files (list (concat org-directory "inbox.org")
                             (concat org-directory "todo.org")
                             (concat org-directory "tickler.org")))


;; Custom org todo keywords
(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "NEXT(n)"
         "DEFER(e)"
         "IDEA(i)"
         "WAIT(w@/!)"
         "|"
         "DONE(d!)"
         "CANCEL(c@/!)")))

;; Set colors for keywords
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "indian red" :weight bold)
              ("DEFER" :foreground "dark khaki" :weight bold)
              ("IDEA" :foreground "cornflower blue" :weight bold)
              ("NEXT" :foreground "dark salmon" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAIT" :foreground "plum" :weight bold)
              ("CANCEL" :foreground "forest green" :weight bold))))

;; Set tags
;; (setq org-tag-alist '(("PROJECT" . ?p)
;;                       ("QUESTION" . ?q)
;;                       ("OBSERVATION" . ?o)
;;                       ("IDEA" . ?i)
;;                       ("MEETING" . ?m)))


;; (setq org-highest-priority ?A)
;; (setq org-lowest-priority ?C)
;; (setq org-default-priority ?A)

;;set colours for priorities
;; (setq org-priority-faces '((?A . (:foreground "SaddleBrown" :weight bold))
;;                            (?B . (:foreground "RoyalBlue4"))
;;                            (?C . (:foreground "SpringGreen4"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda setttings

;;show me tasks scheduled or due in next fortnight
(setq org-agenda-span (quote fortnight))

;; https://github.com/jwiegley/newartisans/blob/master/posts/2007-08-20-using-org-mode-as-a-day-planner.md
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-show-all-dates t)

(setq org-agenda-skip-scheduled-if-done t) ;; Scheduled done items should be skipped.
(setq org-agenda-skip-deadline-if-done t)  ;; Deadline tasks done should be skipped.
(setq org-agenda-skip-timestamp-if-done nil) ;; Entries with any timestamp, appointments just like scheduled and deadline entries.

(setq org-agenda-todo-ignore-deadlines t) ;; Don't show deadline tasks in global TODO list.
(setq org-agenda-todo-ignore-with-date t) ;; Don't show any tasks with a date in the global TODO list.
(setq org-agenda-todo-ignore-scheduled t) ;; Don't show scheduled tasks in the global TODO list.

(setq org-agenda-skip-scheduled-if-deadline-is-shown t) ;; Don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)) ;; Don't give a warning colour to tasks with impending deadlines if they are scheduled to be done


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda other settings

(setq org-agenda-tags-column -100) ; take advantage of the screen width
(setq org-agenda-sticky nil)
(setq org-agenda-inhibit-startup t)
(setq org-agenda-use-tag-inheritance t)
(setq org-agenda-show-log t)
(setq org-agenda-window-setup 'current-window) ;; reorganize-frame ;; other-window
(setq org-agenda-dim-blocked-tasks t)

;;(setq org-agenda-use-time-grid t) ;; Use time grid or not, http://orgmode.org/manual/Time_002dof_002dday-specifications.html

;; Sort tasks in order of when they are due and then by priority
(setq org-agenda-sorting-strategy
      (quote
       ((agenda deadline-up priority-down)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep))))

;; http://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
(setq org-agenda-text-search-extra-files '(agenda-archives))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Refile

(setq org-refile-targets '(("todo.org" :maxlevel . 3)
                           ("tickler.org" :level . 2)
                           ("someday.org" :maxlevel . 1)))

; Use full outline paths for refile targets
(setq org-refile-use-outline-path 'file)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(setq org-return-follows-link t)
(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)

;; (setq org-reverse-note-order t) ;; Reverse order anyone?

;; Warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)

;; Habit
(setq org-habit-graph-column 80)
(setq org-habit-show-habits-only-for-today nil)


(setq org-agenda-time-grid
      '((daily today require-timed)
        "--------------------"
        (800 1000 1200 1400 1600 1800 2000 2200)))

(setq org-columns-default-format "%14SCHEDULED %Effort{:} %1PRIORITY %TODO %50ITEM %TAGS")

;; (setq org-cycle-include-plain-lists nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clock

;; (setq org-clock-into-drawer t)
;; (setq org-clock-idle-time nil)

;; (setq org-clock-continuously nil)
;; (setq org-clock-persist t) ;; Persist clock data
;; (setq org-clock-in-switch-to-state "STARTED")
;; (setq org-clock-in-resume t) ;;Resume persisted clock when loading emacs

;; (setq org-show-notification-handler 'message)
;; (setq org-clock-report-include-clocking-task t)
;; (setq org-clock-out-remove-zero-time-clocks t)
;; (setq org-clock-out-when-done t)

;; Copied from https://github.com/aaronbieber/dotfiles/blob/master/configs/emacs.d/lisp/init-org.el
;; (setq org-log-into-drawer "LOGBOOK")
;; (setq org-log-done (quote time))
;; (setq org-log-redeadline (quote time))
;; (setq org-log-reschedule (quote time))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org capture templates

;; Copied from
;; https://github.com/jwiegley/dot-emacs/blob/f9d7e331dd8a6048af3c5ac88fea092b86da7ac5/org-settings.el

(setq org-capture-templates
      (quote
       (("w" "a new task" entry
         (file+headline "inbox.org" "Task")
         "* %^{state|TODO|TODO|IDEA|DEFER} %?"
         :prepend t
         :empty-lines 1
         :created t
         :kill-buffer t)
        ("t" "a new tickler" entry
         (file+headline "tickler.org" "Tickler")
         "* %i%? \n %^u"
         :empty-lines 1
         :created t
         :kill-buffer t)
        ("i" "a new idea" entry
         (file+headline "inbox.org" "Idea")
         "* %?"
         :empty-lines 1
         :created t
         :kill-buffer t)
        ("n" "a new note" entry
         (file+headline "inbox.org" "Note")
         "* %?"
         :empty-lines 1
         :created t
         :kill-buffer t)
        ("c" "save from clipboard" plain
         (file+headline "inbox.org" "Later")
         "%x"
         :empty-lines 1
         :created t
         :kill-buffer t))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Agenda custom commands

;; (setq org-agenda-custom-commands
;;    (quote
;;     (("e" "Emacs Tasks" tags "TODO<>\"PROJECT\"&LEVEL<>1"
;;       ((org-agenda-overriding-header "Emacs Tasks")
;;        (org-agenda-files
;;         (quote
;;          ("~/doc/tasks/emacs.txt")))))
;;      ("h" "Current Hotlist" tags "HOT&TODO=\"PROJECT\""
;;       ((org-agenda-overriding-header "Current Hotlist")))
;;      ("H" "Non-Hot Projects" tags "-HOT&TODO=\"PROJECT\""
;;       ((org-agenda-overriding-header "Non-Hot Projects")))
;;      ("A" "Priority #A tasks" agenda ""
;;       ((org-agenda-ndays 1)
;;        (org-agenda-overriding-header "Today's priority #A tasks: ")
;;        (org-agenda-skip-function
;;         (quote
;;          (org-agenda-skip-entry-if
;;           (quote notregexp)
;;           "\\=.*\\[#A\\]")))))
;;      ("b" "Priority #A and #B tasks" agenda ""
;;       ((org-agenda-ndays 1)
;;        (org-agenda-overriding-header "Today's priority #A and #B tasks: ")
;;        (org-agenda-skip-function
;;         (quote
;;          (org-agenda-skip-entry-if
;;           (quote regexp)
;;           "\\=.*\\[#C\\]")))))
;;      ("r" "Uncategorized items" tags "CATEGORY=\"Inbox\"&LEVEL=2"
;;       ((org-agenda-overriding-header "Uncategorized items")))
;;      ("W" "Waiting/delegated tasks" tags "TODO=\"WAITING\"|TODO=\"DELEGATED\""
;;       ((org-agenda-overriding-header "Waiting/delegated tasks:")
;;        (org-agenda-sorting-strategy
;;         (quote
;;          (todo-state-up priority-down category-up)))))
;;      ("u" "Unscheduled tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT}&CATEGORY<>{Assembly}"
;;       ((org-agenda-overriding-header "Unscheduled tasks: ")
;;        (org-agenda-skip-function
;;         (quote
;;          (org-agenda-skip-entry-if
;;           (quote scheduled)
;;           (quote deadline)
;;           (quote timestamp)
;;           (quote regexp)
;;           "\\* \\(DEFERRED\\|SOMEDAY\\)")))
;;        (org-agenda-sorting-strategy
;;         (quote
;;          (priority-down)))))
;;      ("U" "Deferred tasks" tags "TODO=\"DEFERRED\""
;;       ((org-agenda-files
;;         (quote
;;          ("~/doc/tasks/todo.txt")))
;;        (org-agenda-overriding-header "Deferred tasks:")))
;;      ("Y" "Someday tasks" tags "TODO=\"SOMEDAY\""
;;       ((org-agenda-overriding-header "Someday tasks:")))
;;      ("w" "Unscheduled work-related tasks" tags "TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT}"
;;       ((org-agenda-overriding-header "Unscheduled work-related tasks")
;;        (org-agenda-files
;;         (quote
;;          ("~/doc/tasks/BAE.txt")))
;;        (org-agenda-sorting-strategy
;;         (quote
;;          (todo-state-up priority-down category-up)))
;;        (org-agenda-skip-function
;;         (quote
;;          (org-agenda-skip-entry-if
;;           (quote scheduled)
;;           (quote deadline)
;;           (quote timestamp))))))
;;      ("S" "Assembly Action Items" tags-todo "TODO<>\"PROJECT\""
;;       ((org-agenda-files
;;         (quote
;;          ("~/doc/tasks/assembly.txt")))
;;        (org-agenda-overriding-header "Assembly Action Items")
;;        (org-agenda-sorting-strategy
;;         (quote
;;          (alpha-up time-up)))))
;;      ("c" "Appointment Calendar" agenda ""
;;       ((org-agenda-overriding-header "Appointment Calendar")
;;        (org-agenda-sorting-strategy
;;         (quote
;;          (time-up)))
;;        (org-agenda-span 14)
;;        (org-agenda-ndays 14)
;;        (org-agenda-regexp-filter-preset
;;         (quote
;;          ("+APPT")))))
;;      ("Z" "MobileOrg Tasks" agenda ""
;;       ((org-agenda-overriding-header "MobileOrg Tasks")
;;        (org-agenda-span 14)
;;        (org-agenda-ndays 14))))))


(provide 'xt-org)
;;; xt-org.el ends here

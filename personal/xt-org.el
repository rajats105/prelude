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

(global-set-key (kbd "C-c c") 'org-capture)

;; Cleaner view ;;
(setq org-hide-leading-stars t)
(setq org-odd-levels-only t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PATHS

(setq org-directory "~/rajat/org")
(setq org-todo-directory (concat org-directory "/todo"))
(setq org-note-directory (concat org-directory "/note"))

(setq org-agenda-files (list (concat org-todo-directory "/work.org")
                             (concat org-todo-directory "/personal.org")
                             (concat org-todo-directory "/calendar.org")))
(setq org-default-notes-file (concat org-note-directory "/notes.org"))
(setq xt-refile-file (concat org-todo-directory "/refile.org"))
(setq xt-journal-file (concat org-todo-directory "/journal.org"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO

(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "STARTED(s)"
         "DELEGATED(g)"
         "WAITING(w@/!)"
         "SOMEDAY(.)"
         "|"
         "DONE(d!)"
         "CANCELED(c@/!)")))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "indian red" :weight bold)
              ("STARTED" :foreground "cornflower blue" :weight bold)
              ("DELEGATED" :foreground "dark salmon" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "plum" :weight bold)
              ("SOMEDAY" :foreground "dark khaki" :weight bold)
              ("CANCELED" :foreground "forest green" :weight bold))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Priorities

(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)

;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "SaddleBrown" :weight bold))
                           (?B . (:foreground "RoyalBlue4"))
                           (?C . (:foreground "SpringGreen4"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda setttings

;;show me tasks scheduled or due in next fortnight
(setq org-agenda-span (quote fortnight))
;;(setq org-agenda-span 2)

(setq org-agenda-skip-scheduled-if-done t) ;; Scheduled done items should be skipped.
(setq org-agenda-skip-deadline-if-done t)  ;; Deadline tasks done should be skipped.
(setq org-agenda-skip-timestamp-if-done t) ;; Entries with any timestamp, appointments just like scheduled and deadline entries.

(setq org-agenda-todo-ignore-deadlines t) ;; Don't show deadline tasks in global TODO list.
(setq org-agenda-todo-ignore-with-date t) ;; Don't show any tasks with a date in the global TODO list.
(setq org-agenda-todo-ignore-scheduled t) ;; Don't show scheduled tasks in the global TODO list.

(setq org-agenda-skip-scheduled-if-deadline-is-shown t) ;; Don't show tasks as scheduled if they are already shown as a deadline

(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)) ;; Don't give awarning colour to tasks with impending deadlines if they are scheduled to be done


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

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


(setq org-blank-before-bullet t)
(setq org-blank-before-new-entry (quote ((heading . auto) (plain-list-item . auto))))

;; (setq org-cycle-include-plain-lists nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clock

(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer t)

(setq org-clock-idle-time nil)
(setq org-log-done 'note)
(setq org-clock-continuously nil)
(setq org-clock-persist t) ;; Persist clock data
(setq org-clock-in-switch-to-state "STARTED")
(setq org-clock-in-resume t) ;;Resume persisted clock when loading emacs

(setq org-show-notification-handler 'message)
(setq org-clock-report-include-clocking-task t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-out-when-done t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda custom commands

(setq org-agenda-custom-commands
      '(("d" todo "DELEGATED" nil)
        ("w" todo "WAITING" nil)
        ("P" "Project List"
         ((tags "PROJECT")))
        ("O" "Office"
         ((agenda)
          (tags-todo "OFFICE")))
        ("W" "Weekly Plan"
         ((agenda)
          (todo "TODO")
           (tags "PROJECT")))
        ("H" "Home NA Lists"
         ((agenda)
           (tags-todo "HOME")
           (tags-todo "COMPUTER")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org capture templates

(setq org-capture-templates
      '(("j" "Journal Entry" plain
         (file+datetree xt-journal-file)
         "%U\n\n%?" :empty-lines-before 1)
        ("w" "Log Work Task" entry
         (file+datetree xt-worklog-file)
         "* TODO %^{Description}  %^g\n%?\n\nAdded: %U"
         :clock-in t
         :clock-keep t)))

(defvar my/org-basic-task-template "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>
%?

%i
" "Basic task data")

(setq org-capture-templates
      `(("t" "Tasks" entry
         (file+headline "~/personal/organizer.org" "Inbox")
         ,my/org-basic-task-template)
        ("T" "Quick task" entry
         (file+headline "~/personal/organizer.org" "Inbox")
         "* TODO %^{Task}\nSCHEDULED: %t\n"
         :immediate-finish t)
        ("i" "Interrupting task" entry
         (file+headline "~/personal/organizer.org" "Inbox")
         "* STARTED %^{Task}"
         :clock-in :clock-resume)
        ("e" "Emacs idea" entry
         (file+headline "~/code/emacs-notes/tasks.org" "Emacs")
         "* TODO %^{Task}"
         :immediate-finish t)
        ("E" "Energy" table-line
         (file+headline "~/personal/organizer.org" "Track energy")
         "| %U | %^{Energy 5-awesome 3-fuzzy 1-zzz} | %^{Note} |"
         :immediate-finish t
         )
        ("b" "Business task" entry
         (file+headline "~/personal/business.org" "Tasks")
         ,my/org-basic-task-template)
        ("p" "People task" entry
         (file+headline "~/personal/people.org" "Tasks")
         ,my/org-basic-task-template)
        ("j" "Journal entry" plain
         (file+datetree "~/personal/journal.org")
         "%K - %a\n%i\n%?\n"
         :unnarrowed t)
        ("J" "Journal entry with date" plain
         (file+datetree+prompt "~/personal/journal.org")
         "%K - %a\n%i\n%?\n"
         :unnarrowed t)
        ("s" "Journal entry with date, scheduled" entry
         (file+datetree+prompt "~/personal/journal.org")
         "* \n%K - %a\n%t\t%i\n%?\n"
         :unnarrowed t)
        ("c" "Protocol Link" entry (file+headline ,org-default-notes-file "Inbox")
         "* [[%:link][%:description]] \n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?\n\nCaptured: %U")
        ("db" "Done - Business" entry
         (file+headline "~/personal/business.org" "Tasks")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("dp" "Done - People" entry
         (file+headline "~/personal/people.org" "Tasks")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("dt" "Done - Task" entry
         (file+headline "~/personal/organizer.org" "Inbox")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("q" "Quick note" item
         (file+headline "~/personal/organizer.org" "Quick notes"))
        ("l" "Ledger entries")
        ("lm" "MBNA" plain
         (file "~/personal/ledger")
         "%(org-read-date) %^{Payee}
    Liabilities:MBNA
    Expenses:%^{Account}  $%^{Amount}
  " :immediate-finish t)
        ("ln" "No Frills" plain
         (file "~/personal/ledger")
         "%(let ((org-read-date-prefer-future nil)) (org-read-date)) * No Frills
    Liabilities:MBNA
    Assets:Wayne:Groceries  $%^{Amount}
  " :immediate-finish t)
        ("lc" "Cash" plain
         (file "~/personal/ledger")
         "%(org-read-date) * %^{Payee}
    Expenses:Cash
    Expenses:%^{Account}  %^{Amount}
  ")
        ("B" "Book" entry
         (file+datetree "~/personal/books.org" "Inbox")
         "* %^{Title}  %^g
  %i
  *Author(s):* %^{Author} \\\\
  *ISBN:* %^{ISBN}

  %?

  *Review on:* %^t \\
  %a
  %U"
         :clock-in :clock-resume)
        ("C" "Contact" entry (file "~/personal/contacts.org")
         "* %(org-contacts-template-name)
  :PROPERTIES:
  :EMAIL: %(my/org-contacts-template-email)
  :END:")
        ("n" "Daily note" table-line (file+olp "~/personal/organizer.org" "Inbox")
         "| %u | %^{Note} |"
         :immediate-finish t)
        ("r" "Notes" entry
         (file+datetree "~/personal/organizer.org")
         "* %?\n\n%i\n"
         )))


(setq org-capture-templates
      (quote (("t" "Todo" entry (file+headline xt-refile-file "To Do")
               "* TODO %?\n  OPENED: %U\n %i")
              ("n" "Note" entry (file xt-refile-file)
               "* %?\n  OPENED: %U\n %i")
              ("j" "Journal"
               entry (file+datetree xt-journal-file)
               "* %?"
               :empty-lines 1)
              ("l" "Logbook" entry (file+datetree xt-work-logbook-file) "** %U - %^{Activity}  :LOG:"))))

;; https://aws.amazon.com/new/feed/
(setq org-feed-alist
      '(("Slashdot"
         "http://rss.slashdot.org/Slashdot/slashdot"
         "~/feeds.org" "Slashdot Entries")
        ("AWS"
         "https://aws.amazon.com/new/feed/"
         "~/feeds.org" "AWS entries")))





(prelude-require-packages '(elfeed elfeed-org elfeed-web))

(setq elfeed-feeds
      '("http://rss.slashdot.org/Slashdot/slashdot"
        "https://aws.amazon.com/new/feed/"
        "https://8thlight.com/blog/feed/atom.xml"
        "http://feeds.feedburner.com/codinghorror?format=xml"
        "http://www.joelonsoftware.com/rss.xml"
        "http://feeds.feedburner.com/HighScalability?format=xml"
        "http://nerds.airbnb.com/feed/"
        "http://allegro.tech/feed.xml"
        "http://autodeskcloudengineering.typepad.com/blog/atom.xml"
        "http://feeds.feedburner.com/AmazonWebServicesBlog?format=xml"
        "http://nullprogram.com/feed/"
        "http://www.bigeng.io/rss/"
        "http://benchling.engineering/rss/"
        "http://word.bitly.com/rss"
        "http://engineering.bittorrent.com/feed/"
        "http://rockthecode.io/feed/"
        "http://engineering.bloomreach.com/feed/"
        "https://blog.box.com/blog/feed/"
        "http://engineering.brandwatch.com/rss/"
        "https://www.ctl.io/developers/blog/rss"
        "https://blog.chaps.io/feed.xml"
        "https://www.digitalocean.com/community/tutorials/feed"
        "https://blog.docker.com/feed/"
        "https://blogs.dropbox.com/tech/feed/"
        "http://engineering.dailymotion.com/rss/"
        "http://research.baidu.com/feed/"
        "http://research.baidu.com/category/baidutechblog/baidu-research-technology/feed/"
        "http://www.ebaytechblog.com/feed/"
        "https://medium.com/feed/unexpected-token"
        "https://blog.engineyard.com/rss.xml"
        "http://webuild.envato.com/atom.xml"
        "https://codeascraft.com/feed/"
        "https://www.eventbrite.com/engineering/feed/"
        "https://blog.evernote.com/feed/"
        "https://evilmartians.com/chronicles.atom"
        "https://blog.chef.io/feed/"
        "http://engineering.cerner.com/atom.xml"
        "http://blog.codeship.com/feed/"
        "https://blog.asana.com/feed/"
        "http://nerds.airbnb.com/feed/"
        "https://code.facebook.com/posts/rss"
        "http://tech.finn.no/atom.xml"
        "http://making.fiftythree.com/feed.xml"
        "http://code.flickr.net/feed/"
        "http://engineering.flipboard.com/feed.xml"
        "http://tech-blog.flipkart.net/feed/"
        "http://engineering.foursquare.com/feed/"
        "https://engineering.fundingcircle.com/feed.xml"
        "https://www.future-processing.pl/feed/?post_type=post"
        "https://galois.com/blog/feed/"
        "http://tech.gc.com/atom.xml"
        "http://tech.gilt.com/atom.xml"
        "http://githubengineering.com/atom.xml"
        "https://github.com/blog.atom"
        "https://engineering.gnip.com/feed/"
        "https://gocardless.com/blog/atom.xml"
        "https://engineering.gnip.com/feed/"
        "http://bites.goodeggs.com/rss"
        "http://googleresearch.blogspot.com/atom.xml"
        "https://googleonlinesecurity.blogspot.com/atom.xml"
        "https://engineering.gosquared.com/feed"
        "https://tech.grammarly.com/blog/rss.xml"
        "http://eng.joingrouper.com/atom.xml"
        "https://engineering.groupon.com/feed/"
        "https://www.theguardian.com/info/developer-blog/rss"
        "http://engineering.gusto.com/rss/"
        "http://engineering.harrys.com/feed.xml"
        "http://feeds.feedburner.com/hashrocket-blog"
        "http://engineering.heroku.com/feed.xml"
        "http://code.hireart.com/feed.xml"
        "http://blog.honeybadger.io/feed.xml"
        "http://code.hootsuite.com/feed/"
        "http://product.hubspot.com/blog/rss.xml"
        "http://tldr.huddle.com/atom.xml"
        "http://engineering.ifttt.com/feed.xml"))

(provide 'xt-org)

;;; xt-org.el ends here

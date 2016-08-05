;; https://github.com/jwiegley/alert
(prelude-require-package 'alert)

(setq alert-default-style 'notifier)


(defvar terminal-notifier-command (executable-find "terminal-notifier") "The path to terminal-notifier.")
;; (terminal-notifier-notify "Emacs notification" "Something amusing happened")

(defun terminal-notifier-notify (title message)
  "Show a message with `terminal-notifier-command`."
  (start-process "terminal-notifier"
                 "*terminal-notifier*"
                 terminal-notifier-command
                 "-title" title
                 "-message" message
                 "-activate" "org.gnu.Emacs"))

(defun timed-notification(time msg)
  (interactive "sNotification when (e.g: 2 minutes, 60 seconds, 3 days): \nsMessage: ")
  (run-at-time time nil (lambda (msg) (terminal-notifier-notify "Emacs" msg)) msg))


(setq org-show-notification-handler
      (lambda (msg) (timed-notification nil msg)))




(require 'appt)
(setq appt-time-msg-list nil)    ;; clear existing appt list
(setq appt-display-interval '10) ;; warn every 10 minutes from t - appt-message-warning-time
(setq
  appt-message-warning-time '10  ;; send first warning 10 minutes before appointment
  appt-display-mode-line nil     ;; don't show in the modeline
  appt-display-format 'window)   ;; pass warnings to the designated window function
(appt-activate 1)                ;; activate appointment notification
(display-time)                   ;; activate time display

(org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
(run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view


;; designate the window function for my-appt-send-notification
(defun my-appt-display (min-to-app new-time msg)
  (terminal-notifier-notify
    (format "'Appointment in %s minutes'" min-to-app)    ;; passed to -title in terminal-notifier call
    (format "'%s'" msg)))                                ;; passed to -message in terminal-notifier call
(setq appt-disp-window-function (function my-appt-display))

(provide 'xt-misc)

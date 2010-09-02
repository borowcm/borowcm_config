;; Diary, Time 
(setq diary-file "~/.diary")
(setq display-time-day-and-date t)
(display-time)

(add-hook 'diary-hook 'appt-make-list)
(in-package :cl-user)

(defpackage peak.options
  (:use :cl)
  (:import-from :sqlite)
  (:import-from :str)

  (:export :show-options
		   :handle-option
		   :*db*
		   :*options*))
(in-package :peak.options)

(defvar *db* (sqlite:connect "peak.db"))
(sqlite:execute-non-query 
  *db* "create table if not exists tasks (
  date_created timestamp default current_timestamp, id integer primary key, task varchar(255) not null, duration int not null,
  complete boolean default false)")
(sqlite:execute-non-query 
  *db* "create table if not exists distractions (date_created timestamp default current_timestamp, distraction varchar(255) not null)")
(sqlite:execute-non-query 
  *db* "create table if not exists milestones (date_created timestamp default current_timestamp, milestone varchar(255) not null)")
(sqlite:execute-non-query *db* "create table if not exists cookies (cookies int not null)")
(unless (sqlite:execute-single *db* "select cookies from cookies")
  (sqlite:execute-non-query *db* "insert into cookies (cookies) values (?)" 101))

(defvar *options* 
  (list
	(list :com "pom" :desc "Start pomodoro" :action 'start-pomodoro) 
	(list :com "n" :desc "Create new task" :action 'new-task) 
	(list :com "r" :desc "Remove task" :action 'remove-task)
	(list :com "mc" :desc "Mark task as complete" :action 'mark-complete)
	(list :com "x" :desc "Log distraction" :action 'log-distraction)
	(list :com "m" :desc "Log personal milestone" :action 'log-milestone)
	(list :com "vi" :desc "View incomplete tasks" :action 'view-incomplete)
	(list :com "vc" :desc "View complete tasks" :action 'view-complete)
	(list :com "vd" :desc "View distractions" :Action 'view-distractions)
	(list :com "vm" :desc "View milestones" :action 'view-milestones)
	(list :com "q" :desc "Quit peak" :action 'quit-peak)))

(defun add-cookies (quantity)
  (sqlite:execute-non-query *db* "update cookies set cookies = ?" (+ (sqlite:execute-single *db* "select cookies from cookies") quantity)))
(defun remove-cookies (quantity)
  (sqlite:execute-non-query *db* "update cookies set cookies = ?" (- (sqlite:execute-single *db* "select cookies from cookies") quantity)))

(defun show-options ()
  (format t "~%~c[1m112 101 97 107~c[0m ---- \[cookies : ~c[1m~a~c[0m\]~%~%" #\Esc #\Esc #\Esc 
		  (sqlite:execute-single *db* "select cookies from cookies") #\Esc)
  (dolist (i *options*)
	(format t "\[~c[1m~a~c[0m\]	- ~a~%" #\Esc (getf i :com) #\Esc (getf i :desc)))
  (handle-option (read-line *standard-input*)))

(defun handle-option (option-str)
  (dolist (i *options*)
	(if (string-equal (getf i :com) option-str)
		(funcall (getf i :action)))))

(defun new-task ()
  (princ "Task | Duration in days or hours separated by ',': ") (force-output)
  (apply #'sqlite:execute-non-query *db* "insert into tasks (task, duration) values (?, ?)" 
		 (str:split "," (read-line *standard-input*) :omit-nulls t))
  (format t "Task added~%")
  (show-options))

(defun actions (prompt-str-1 sql-command prompt-str-2 cleanup-f cleanup-args)
  (format t prompt-str-1 #\Esc #\Esc) (force-output)
  (let ((user-input (read-line *standard-input*)))
	(if (str:containsp "insert" sql-command) (sqlite:execute-non-query *db* sql-command user-input)
		(if (and (or (str:containsp "delete" sql-command) (str:containsp "update" sql-command))
				 (sqlite:execute-single *db* "select id from tasks where id=?" user-input))
			(sqlite:execute-non-query *db* sql-command user-input)
			(progn (format t "~%~c[1mItem with the given id doesn't exist in tasks~c[0m~%" #\Esc #\Esc) (show-options)))))
  (format t "~a~%" prompt-str-2)
  (apply cleanup-f (list cleanup-args))
  (show-options))

(defun remove-task ()
  (actions "Enter task id (view id in incomplete task list) \[~c[1mWarning: You will lose 5 cookies~c[0m\]: "
		   "delete from tasks where id=?"
		   "Task removed. You've lost 5 cookies" #'remove-cookies 5))

(defun mark-complete ()
  (actions "Enter task id (view id in incomplete task list): "
		   "update tasks set complete = true where id = ?"
		   "Task marked as complete. You've earned 5 cookies" #'add-cookies 5))

(defun log-distraction ()
  (actions "Enter distraction. Use single words e.g (Instagram) \[~c[1mWarning: You will lose 1 cookie~c[0m\]: "
		   "insert into distractions (distraction) values (?)"
		   "Distraction logged. You've lost a cookie" #'remove-cookies 1))

(defun log-milestone ()
  (actions "Enter milestone. These are ~c[1mpeak~c[0m results of your focus: "
		   "insert into milestones (milestone) values (?)"
		   "Milestone logged. You've earned 10 cookies" #'add-cookies 10))

(defun view-data (table-name clause clause-arg)
  (let ((headers '()))
	(dolist (i (sqlite:execute-to-list *db* (format nil "pragma table_info(~a)" table-name)))
	  (push (format nil "~:(~a~)" (str:join " " (str:split "_" (nth 1 i)))) headers))
	(format t "~%|~{~a~^ | ~}|~%" (reverse headers))
	(format t "~{|~{~a~^ | ~}|~%~}" 
			(sqlite:execute-to-list *db* 
									(if clause (format nil "select * from ~a where ~a = ~a" table-name clause clause-arg)
										(format nil "select * from ~a" table-name)))))
  (show-options))
(defun view-incomplete () (view-data "tasks" "complete" "false"))
(defun view-complete () (view-data "tasks" nil nil))
(defun view-milestones () (view-data "milestones" nil nil))
(defun view-distractions () (view-data "distractions" nil nil))

;; default at 25 5 (short) 15 (long) breaks . requires mpv :(
(defun start-pomodoro ()
  (format t "---- starting pomodoro ----~%")
  (format t "")
  (flet ((safe-cmd (cmd-lst) 
		   (handler-case (uiop:run-program cmd-lst) 
			 (error (e) (format t "Unexpected error occurred ~a" e)))))
	(loop :for i :from 0 :while (< i 4) :do 
		  (format t ".... starting 25 minute session ....~%") (force-output)
		  (dotimes (i 5)
			(sleep 300)
			(format t " *~a* " (* (1+ i) 5)) (force-output))

		  (if (< i 3)
			  (progn 
				(safe-cmd `("mpv" "ping-82822.mp3"))
				(format t "~%. starting 5 minute break ..") (force-output)
				(sleep 300) 
				(format t ".. ended 5 minute break .~%") (force-output)
				(safe-cmd `("mpv" "din-ding-89718.mp3")))
			  (safe-cmd `("mpv" "ping-82822.mp3"))))

	(format t ".... starting 15 minute long break ....~%") (force-output)
	(sleep 900)
	(format t "**** ended pomodoro session ****")
	(safe-cmd `("mpv" "subtle-95660.mp3"))
	(show-options)))

(defun quit-peak () (sqlite:disconnect *db*) (uiop:quit))

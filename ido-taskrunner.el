(require 'ido)
(require 'taskrunner)
;;; Code:

(defun ido-taskrunner--check-if-in-project ()
  "Check if the currently visited buffer is in a project.
If it is not, prompt the user to select a project"
  ;; If we are not in a project, ask the user to switch to one
  (if (not (projectile-project-p))
      (projectile-switch-project)
    )
  )

(defun ido-taskrunner-project-root ()
  "Launch ido to select a task and run it in the current project."
  (interactive)
  (ido-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (ido-completing-read "Task to run: " (taskrunner-get-tasks-from-cache) nil t))
  )

(defun ido-taskrunner-project-root-prompt ()
  "Launch ido to select a task and run it in the current project."
  (interactive)
  (ido-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (ido-completing-read "Task to run: " (taskrunner-get-tasks-from-cache) nil t))
  )

(defun ido-taskrunner-project-curr-dir ()
  "Launch ido to select a task and run it in the current project."
  (interactive)
  (ido-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (ido-completing-read "Task to run: " (taskrunner-get-tasks-from-cache) nil t))
  )

(defun ido-taskrunner-project-curr-dir-prompt ()
  "Launch ido to select a task and run it in the current project."
  (interactive)
  (ido-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (let ((task 
             (ido-completing-read "Task to run: "
                                  (taskrunner-get-tasks-from-cache) nil t)))
        (if task
            (taskrunner-run-task task (file-name-directory (buffer-file-name)) t)))
    )
  )

(provide 'ido-taskrunner)
;;; ido-taskrunner.el ends here

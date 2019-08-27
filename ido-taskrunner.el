;;; ido-taskrunner.el --- Retrieve build system/taskrunner tasks via ido -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Yavor Konstantinov

;; Author: Yavor Konstantinov <ykonstantinov1 AT gmail DOT com>
;; URL: https://github.com/emacs-taskrunner/ido-taskrunner
;; Version: 0.8
;; Package-Requires: ((emacs "25.1"))
;; Keywords: build-system taskrunner build task-runner tasks ido

;; This file is not part of GNU Emacs.

;;; Commentary:
;; This package provides an ido interface to the taskrunner library

;;;; Installation

;;;;; MELPA
;; If installed form MELPA then simply add:
;; (require 'ido-taskrunner) to your init.el

;;;;; Manual

;; Install these required packages:

;; projectile
;; taskrunner

;; Then put this folder in your load-path, and put this in your init:

;; (require 'ido-taskrunner)

;;;; Usage
;; TODO: Fill this in



;;;; Credits

;; This package would not have been possible without the following
;; packages:
;; ido which helped me create the interface

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'ido)
(require 'taskrunner)

;;;; Variables

;; Variable aliases for customizable variables used in the backend
(defvaralias 'ido-taskrunner-preferred-js-package-manager 'taskrunner-preferred-js-package-manager)
(defvaralias 'ido-taskrunner-get-all-make-targets 'taskrunner-retrieve-all-make-targets)
(defvaralias 'ido-taskrunner-gradle-heading-regexps 'taskrunner-gradle-heading-regexps)
(defvaralias 'ido-taskrunner-build-dir-list 'taskrunner-build-dir-list)
(defvaralias 'ido-taskrunner-source-dir-list 'taskrunner-source-dir-list)
(defvaralias 'ido-taskrunner-go-task-bin-path 'taskrunner-go-task-bin-path)
(defvaralias 'ido-taskrunner-mage-bin-path 'taskrunner-mage-bin-path)
(defvaralias 'ido-taskrunner-doit-bin-path 'taskrunner-doit-bin-path)
(defvaralias 'ido-taskrunner-no-previous-command-ran-warning 'taskrunner-no-previous-command-ran-warning)

(defconst ido-taskrunner-no-task-warning
  "ido-taskrunner: No task has been selected!"
  "Warning used to indicate that the user has not selected any task.")

(defconst ido-taskrunner-no-project-warning
  "ido-taskrunner: You need to be visiting a project buffer!"
  "Warning used to indicate that the user is not visiting a project buffer.")

(defconst ido-taskrunner--command-location-choices
  '("Project Root"
    "Project Root with args"
    "Current Dir"
    "Current Dir with args"
    "Another Dir"
    "Another Dir with args")
  "Commands used to select the location where the selected task should be ran.")

;;;; Functions

(defun ido-taskrunner--check-if-in-project ()
  "Check if the currently visited buffer is in a project.
If it is not, prompt the user to select a project"
  ;; If we are not in a project, ask the user to switch to one
  (if (not (projectile-project-p))
      (projectile-switch-project)
    t))

(defvar ido-taskrunner-no-targets-found-warning
  "ido-taskrunner: No targets found for current project!")
(defvar ido-taskrunner-prompt-before-show nil)

;; Functions which run tasks in a specific directory
(defun ido-taskrunner--root-task (TASK)
  "Run the task TASK in the project root without asking for extra args.
This is the default command when selecting/running a task/target."
  (taskrunner-run-task TASK nil nil t))

(defun ido-taskrunner--root-task-prompt (TASK)
  "Run the task TASK in the project root and ask the user for extra args."
  (taskrunner-run-task TASK nil t t))

(defun ido-taskrunner--current-dir (TASK)
  "Run the task TASK in the directory visited by the current buffer.
Do not prompt the user to supply any extra arguments."
  (let ((curr-file (buffer-file-name)))
    (when curr-file
      (taskrunner-run-task TASK (file-name-directory curr-file) nil t))))

(defun ido-taskrunner--current-dir-prompt (TASK)
  "Run the task TASK in the directory visited by the current buffer.
Prompt the user to supply extra arguments."
  (let ((curr-file (buffer-file-name)))
    (when curr-file
      (taskrunner-run-task TASK (file-name-directory curr-file) t t))))

(defun ido-taskrunner--select-dir (TASK)
  "Run the task TASK in a directory chosen by the user."
  (let ((command-directory (read-directory-name "Directory: " (projectile-project-root))))
    (when command-directory
      (taskrunner-run-task TASK command-directory nil t))))

(defun ido-taskrunner--select-dir-prompt (TASK)
  "Run the task TASK in a directory chosen by the user.
Prompt the user to supply extra arguments."
  (let ((command-directory (read-directory-name "Directory: " (projectile-project-root))))
    (when command-directory
      (taskrunner-run-task TASK command-directory t t))))

(defun ido-taskrunner--command-dispatch (COMMAND LOCATION)
  "Determine which location was chosen by the user during task selection.
COMMAND is the command to run and LOCATION is the location chosen."
  (cond
   ((null LOCATION)
    (message "You need to select a location to run it in!"))
   ((string-equal LOCATION "Project Root")
    (ido-taskrunner--root-task COMMAND))
   ((string-equal LOCATION "Project Root with args")
    (ido-taskrunner--root-task-prompt COMMAND))
   ((string-equal LOCATION "Current Dir")
    (ido-taskrunner--current-dir COMMAND))
   ((string-equal LOCATION "Current Dir with args")
    (ido-taskrunner--current-dir-prompt COMMAND))
   ((string-equal LOCATION "Another Dir")
    (ido-taskrunner--select-dir COMMAND))
   ((string-equal LOCATION "Another Dir with args")
    (ido-taskrunner--select-dir-prompt COMMAND))))

(defmacro ido-taskrunner--show-ido-task-instance (TARGET-LIST)
  "Show in an instance of `ido' for TARGET-LIST."
  `(let ((command-choice)
         (folder-choice))
     (setq command-choice (ido-completing-read "Task to run: " ,TARGET-LIST nil t))
     (when command-choice
       (setq folder-choice (ido-completing-read
                            "Location to run task in: "
                            ido-taskrunner--command-location-choices
                            nil t))
       (ido-taskrunner--command-dispatch command-choice folder-choice))))

(defun ido-taskrunner--run-ido-for-targets (TARGETS)
  "Launch an ido instance with candidates TARGETS.
If TARGETS is nil then a warning is shown to indicate that no targets were found."
  (if (null TARGETS)
      (message "No tasks")
    (if (and ido-taskrunner-prompt-before-show
             (not (taskrunner-project-cached-p (projectile-project-root))))
        (when (y-or-n-p "Show ido-taskrunner? ")
          (ido-taskrunner--show-ido-task-instance TARGETS))
      (ido-taskrunner--show-ido-task-instance TARGETS))))

;;;###autoload
(defun ido-taskrunner ()
  "Launch ido to select a task which is ran in the currently visited project.
This command runs asynchronously and depending on the number of tasks which
have to be retrieved, it might take several seconds."
  (interactive)
  (ido-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (if (and ido-taskrunner-minor-mode
               ido-taskrunner--retrieving-tasks-p)
          (progn
            (setq ido-taskrunner--tasks-queried-p t)
            (message ido-taskrunner-tasks-being-retrieved-warning))
        (taskrunner-get-tasks-async 'ido-taskrunner--run-ido-for-targets))
    (message ido-taskrunner-project-warning)))

;;;###autoload
(defun ido-taskrunner-update-cache ()
  "Refresh the task cache for the current project and show all tasks."
  (interactive)
  (ido-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (taskrunner-refresh-cache-async 'ido-taskrunner--run-ido-for-targets)
    (message ido-taskrunner-no-project-warning)))

;;;###autoload
(defun ido-taskrunner-rerun-last-command ()
  "Rerun the last command/task ran in the currently visited project."
  (ido-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (taskrunner-rerun-last-task (projectile-project-root))
    (message ido-taskrunner-no-project-warning)))

;;;###autoload
(defun ido-taskruner-buffers ()
  "Show all ido-taskrunner compilation buffers and switch to the selected one."
  (interactive)
  (let ((task-buffs (taskrunner-get-compilation-buffers))
        (selected-buffer nil))
    (setq selected-buffer (ido-completing-read "Buffer to open: " task-buffs nil t))
    (when selected-buffer
      (switch-to-buffer selected-buffer))))

;;;###autoload
(defun ido-taskrunner-kill-all-buffers ()
  "Kill all ido-taskrunner compilation buffers."
  (interactive)
  (taskrunner-kill-compilation-buffers))

;; Functions related to command history
;;;###autoload
(defun ido-taskrunner-command-history ()
  "Show the command history for the currently visited project."
  (interactive)
  (ido-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (let ((commands-ran (taskrunner-get-commands-from-history (projectile-project-root))))
        (if commands-ran
            (ido-taskrunner--run-ido-for-targets commands-ran)
          (message ido-taskrunner-command-history-empty-warning)))
    (message ido-taskrunner-project-warning)))

(provide 'ido-taskrunner)
;;; ido-taskrunner.el ends here

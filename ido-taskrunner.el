;;; ido-taskrunner.el --- Retrieve build system/taskrunner tasks via ido -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Yavor Konstantinov

;; Author: Yavor Konstantinov <ykonstantinov1 AT gmail DOT com>
;; URL: https://github.com/emacs-taskrunner/helm-taskrunner
;; Version: 1.0
;; Package-Requires: ((emacs "24"))
;; Keywords: build-system taskrunner build task-runner tasks helm

;; This file is not part of GNU Emacs.

;;; Commentary:
;; This package provides an helm interface to the taskrunner library

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

(defconst ido-taskrunner-no-task-warning
  "ido-taskrunner: No task has been selected!"
  "Warning used to indicate that the user has not selected any task.")

(defconst ido-taskrunner-no-project-warning
  "ido-taskrunner: You need to be visiting a project buffer!"
  "Warning used to indicate that the user is not visiting a project buffer.")

;;;; Functions

(defun ido-taskrunner--check-if-in-project ()
  "Check if the currently visited buffer is in a project.
If it is not, prompt the user to select a project"
  ;; If we are not in a project, ask the user to switch to one
  (if (not (projectile-project-p))
      (projectile-switch-project)
    )
  )

;;;###autoload
(defun ido-taskrunner-project-root ()
  "Launch ido to select a task and run it in the current project."
  (interactive)
  (ido-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (let ((TASK
             (ido-completing-read "Task to run: "
                                  (taskrunner-get-tasks-from-cache) nil t)))
        (if TASK
            (taskrunner-run-task TASK (projectile-project-root) nil)
          (message ido-taskrunner-no-task-warning))
        )
    (message ido-taskrunner-no-project-warning))
  )

;;;###autoload
(defun ido-taskrunner-project-root-prompt ()
  "Launch ido to select a task and run it in the current project.
Additionally, ask the user to supply extra arguments to the task ran."
  (interactive)
  (ido-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (let ((TASK
             (ido-completing-read "Task to run: "
                                  (taskrunner-get-tasks-from-cache) nil t)))
        (if TASK
            (taskrunner-run-task TASK (projectile-project-root) nil)
          (message ido-taskrunner-no-task-warning))
        )
    )
  )

;;;###autoload
(defun ido-taskrunner-project-curr-dir ()
  "Select a task and run it in the directory visited by the currend buffer."
  (interactive)
  (ido-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (let ((TASK
             (ido-completing-read "Task to run: "
                                  (taskrunner-get-tasks-from-cache) nil t)))
        (if TASK
            (taskrunner-run-task TASK (file-name-directory (buffer-file-name)) nil)
          (message ido-taskrunner-no-task-warning))
        )
    )
  )

;;;###autoload
(defun ido-taskrunner-project-curr-dir-prompt ()
  "Select a task and run it in the directory visited by the currend buffer.
Additionally, ask the user to supply extra arguments."
  (interactive)
  (ido-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (let ((TASK
             (ido-completing-read "Task to run: "
                                  (taskrunner-get-tasks-from-cache) nil t)))
        (when TASK
          (taskrunner-run-task TASK (file-name-directory (buffer-file-name)) t)))
    (message ido-taskrunner-no-project-warning)))

;;;###autoload
(defun ido-taskruner-task-buffers ()
  "Show all ido-taskrunner compilation buffers."
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

(provide 'ido-taskrunner)
;;; ido-taskrunner.el ends here

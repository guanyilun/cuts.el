;;; cuts-slurm.el --- Cuts slurm plugin for emacs  -*- lexical-binding: t -*-

;; Copyright © 2020 Yilun Guan <yilun.guan@pitt.edu>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This code aims to provide a few useful utility function for dealing
;; with cuts slurm jobs such as viewing and cancelling slurm jobs

;;; Code:

(require 's)
(require 'bui)

;;; user defined parameters
(setq slurm-user "yilung")

;; command to see your jobs
(setq slurm-jobs-command
      (concat "squeue -u "
              slurm-user
              " | awk '{print $1,$2,$3,$4,$5,$6,$7,$8}'"))

;; if you want to see all jobs instead
;; (setq slurm-jobs-command "squeue | awk '{print $1,$2,$3,$4,$5,$6,$7,$8}'")

;;; utility functions
(defun kill-slurm-job (job-id)
  (shell-command (concat "scancel " job-id)))

;;; slurm related functions
;; get fields of a job
(defun cuts-slurm->get-job (entry)
  (let ((fields (s-split " " entry)))
    `((id . ,(nth 0 fields))
      (job-id . ,(nth 0 fields))
      (part . ,(nth 1 fields))
      (name . ,(nth 2 fields))
      (user . ,(nth 3 fields))
      (st . ,(nth 4 fields))
      (time . ,(nth 5 fields))
      (nodes . ,(nth 6 fields))
      (node-list . ,(nth 7 fields)))))

;; get all jobs
(defun cuts-slurm->get-all-jobs ()
  (mapcar 'cuts-slurm->get-job
          (cdr
           (s-split "\n"
                    (shell-command-to-string slurm-jobs-command) t))))

;; define bui interface
(bui-define-interface cuts-slurm list
  :buffer-name "*Cuts Slurm*"
  :get-entries-function 'cuts-slurm->get-all-jobs
  :format '((job-id nil 12 t)
            (part nil 10 t)
            (name nil 10 t)
            (user nil 10 t)
            (st nil 2 t)
            (time nil 10 t)
            (nodes nil 5 t)
            (node-list nil 15 t))
  :sort-key '(job-id))

;; define keymap
(let ((map cuts-slurm-list-mode-map))
  (define-key map (kbd "k") 'cuts-slurm->kill-jobs)
  (define-key map (kbd "x") 'cuts-slurm->kill-jobs))

;;; interactive functions
;; list all tags
;;;###autoload
(defun cuts-slurm-show-jobs ()
  "Display a list of buffers."
  (interactive)
  (bui-get-display-entries 'cuts-slurm 'list))

;; kill marked tags or the current tag
(defun cuts-slurm->kill-jobs ()
  (interactive)
  (if (y-or-n-p "Are you sure you want to kill the selected jobs(s)?")
      (progn
        (dolist (job (or (bui-list-get-marked-id-list)
                         (list (bui-list-current-id))))
          (kill-slurm-job job))
        (revert-buffer nil t))))

(provide 'cuts-slurm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cuts-slurm.el ends here

;;; cuts-run.el --- Cuts run plugin for emacs  -*- lexical-binding: t -*-

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
;; cuts parameters such as openning and running cuts parameters

;;; Code:

(require 'dash)
(require 'bui)
(require 's)

;;; user defined parameters
(setq cuts-run-list-command "cuts run list")

;;; cuts depot functions
(defun cuts-run->get-entry (entry)
  (let ((fields (s-split " " entry t)))
    `((tag . ,(nth 0 fields))
      (name . ,(nth 1 fields))
      (progress . ,(nth 2 fields))
      (percent . ,(nth 3 fields))
      (slurm-summary . ,(nth 4 fields))
      (id . ,(nth 5 fields)))))

(defun cuts-run->get-all-entries ()
  (mapcar 'cuts-run->get-entry
          (s-split "\n" (shell-command-to-string cuts-run-list-command) t)))

;; define bui interface
(bui-define-interface cuts-run list
  :buffer-name "*Cuts Run*"
  :get-entries-function 'cuts-run->get-all-entries
  :format '((tag nil 20 t)
            (name nil 20 t)
            (progress nil 10 t)
            (percent nil 10 t)
            (slurm-summary nil 20 t))
  :sort-key '(tag))

;; define keymap
(let ((map cuts-run-list-mode-map))
  (define-key map (kbd "RET") 'cuts-run->switch-to-folder)
  (define-key map (kbd "P")   'cuts-run->promote-version)
  (define-key map (kbd "x")   'cuts-run->kill-version)
  (define-key map (kbd "S")   'cuts-run->submit-job)
  (define-key map (kbd "c")   'cuts-run->combine-jobs)
  (define-key map (kbd "e")   'cuts-run->show-errors)
  (define-key map (kbd "o")   'cuts-run->switch-to-cutparam)
  (define-key map (kbd "O")   'cuts-run->switch-to-cutParam)
  (define-key map (kbd "F")   'cuts-run->postprocess))

;;; interactive functions
(defun cuts-run->switch-to-cutparam ()
  "Switch to a given cutparam"
  (interactive)
  (find-file (bui-list-current-id)))

(defun cuts-run->switch-to-cutParam ()
  "Switch to a given cutParam"
  (interactive)
  (find-file (s-replace "cutp" "cutP" (bui-list-current-id))))

(defun cuts-run->switch-to-folder ()
  "Switch to the given folder"
  (interactive)
  (dired (file-name-directory (bui-list-current-id))))

(defun cuts-run->show-errors ()
  (interactive)
  (shell-command (concat "cuts run errors " (bui-list-current-id))))

(defun cuts-run->promote-version ()
  "Promote param version of selected tag"
  (interactive)
  (if (y-or-n-p "Are you sure you want to promote the version of selected tag(s)?")
      (progn
        (dolist (cpar (or (bui-list-get-marked-id-list)
                          (list (bui-list-current-id))))
          (shell-command (concat "cuts results promote " cpar)))
        (revert-buffer nil t))))

(defun cuts-run->kill-version ()
  "kill param version of selected tag"
  (interactive)
  (if (y-or-n-p "Are you sure you want to kill the version of selected tag(s)?")
      (progn
        (dolist (cpar (or (bui-list-get-marked-id-list)
                          (list (bui-list-current-id))))
          (delete-file cpar)
          (delete-file (s-replace "cutp" "cutP" cpar)))
        (revert-buffer nil t))))

(defun cuts-run->submit-job ()
  "submit jobs of the given param of selected tags"
  (interactive)
  (if (y-or-n-p "Are you sure you want to submit jobs of selected tag(s)?")
      (progn
        (dolist (cpar (or (bui-list-get-marked-id-list)
                          (list (bui-list-current-id))))
          (shell-command (concat "cuts run submit " cpar)))
        (revert-buffer nil t))))

(defun cuts-run->combine-jobs ()
  "combine the mpi sub-jobs of a given run of selected tags"
  (interactive)
  (if (y-or-n-p "Are you sure you want to combine sub-jobs of the selected tag(s)?")
      (progn
        (dolist (cpar (or (bui-list-get-marked-id-list)
                          (list (bui-list-current-id))))
          (shell-command (concat "cuts results combine " cpar)))
        (revert-buffer nil t))))

;; this definition works but it's a blocking process so i commented off
;; and leave it here for future reference
;; (defun cuts-run->postprocess ()
;;   (interactive)
;;   (if (y-or-n-p "Are you sure you want to run postprocessing on the selected tag(s)?")
;;       (progn
;;         (dolist (cpar (or (bui-list-get-marked-id-list)
;;                           (list (bui-list-current-id))))
;;           (let ((cutdir (file-name-directory cpar)))
;;             (shell-command (concat "cd " cutdir "; cutspipe post.ini")))))))


;; Note the use of with-current-buffer here, without which the
;; bui-list-* commands will not be able to get the selection because i
;; will have moved away from the bui buffers, with-current-buffer
;; makes sure that i always come back to the original buffer after
;; running the commands in its body
(defun cuts-run->postprocess ()
  "Run postprocess on the selected tags in an eshell"
  (interactive)
  (if (y-or-n-p "Are you sure you want to run postprocessing on the selected tag(s)?")
      (progn
        (with-current-buffer (get-buffer-create "*Cuts Shell*")
          (switch-to-buffer-other-window "*Cuts Shell*")
          (eshell-mode))
        (dolist (cpar (or (bui-list-get-marked-id-list)
                          (list (bui-list-current-id))))
          (let ((cutdir (file-name-directory cpar)))
            (with-current-buffer "*Cuts Shell*"
              (insert (concat "cd " cutdir "; cutspipe post.ini; ")))))
        (with-current-buffer "*Cuts Shell*"
          (eshell-send-input)))))

;;;###autoload
(defun cuts-run-show-tags ()
  "Display a list of buffers."
  (interactive)
  (bui-get-display-entries 'cuts-run 'list))

(provide 'cuts-run)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cuts-run.el ends here

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
          (cdr (s-split "\n" (shell-command-to-string cuts-run-list-command) t))))

(cuts-run->get-all-entries)
;; define bui interface
(bui-define-interface cuts-run list
  :buffer-name "*Cuts Run*"
  :get-entries-function 'cuts-run->get-all-entries
  :format '((tag nil 20 t)
            (name nil 15 t)
            (progress nil 10 t)
            (percent nil 10 t)
            (slurm-summary nil 20 t))
  :sort-key '(tag))

;;; interactive functions
(defun cuts-run->switch-to-file ()
  "Switch to a given file"
  (interactive)
  (find-file (bui-list-current-id)))

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

;; define keymap
(let ((map cuts-run-list-mode-map))
  (define-key map (kbd "RET") 'cuts-run->switch-to-file)
  (define-key map (kbd "p")   'cuts-run->promote-version)
  (define-key map (kbd "k")   'cuts-run->kill-version))

;;;###autoload
(defun cuts-run-show-tags ()
  "Display a list of buffers."
  (interactive)
  (bui-get-display-entries 'cuts-run 'list))

(provide 'cuts-run)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cuts-run.el ends here

;;; cuts-depot.el --- Cuts depot plugin for emacs  -*- lexical-binding: t -*-

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
;; with cuts depot such as deleting and navigating.

;;; Code:

(require 'dash)
(require 'bui)
(require 's)

;;; user defined parameters
;; define depot
(setq depot (getenv "CUTS_DEPOT"))

;; define categories of output of interests
(setq categories '("Calibration" "Pathologies" "Postprocess" "SelectedTODs" "TODCuts" "darkDets"))

;;; utility functions
;; get last modified
(defun get-last-modified (file)
  "Returns the last modified date of a FILE."
  (format-time-string "%Y-%m-%d %T"
                      (nth 5 (file-attributes file))))

;;; cuts depot functions
;; get tags for each category
(defun cuts-depot->get-tags (category)
  (let ((dir (concat depot category)))
    (mapcar (lambda (tag)
              `((id . ,(concat dir "/" tag))
                (category . ,category)
                (tag . ,tag)
                (last-modified . ,(get-last-modified (concat dir "/" tag)))))
            ;; get rid of "." ".."
            (cddr (directory-files dir)))))

;; get all tags from all categories
(defun cuts-depot->get-all-tags ()
  (-flatten-n 1 (mapcar (lambda (category) (cuts-depot->get-tags category))
                        categories)))

;; define bui interface
(bui-define-interface cuts-depot list
  :buffer-name "*Cuts Depot*"
  :get-entries-function 'cuts-depot->get-all-tags
  :format '((category nil 20 t)
            (tag nil 30 t)
            (last-modified nil 30 t))
  :sort-key '(tag))

;; switch to folder
(defun cuts-depot->switch-to-folder ()
  (interactive)
  (dired (bui-list-current-id)))

;; kill marked tags or the current tag
(defun cuts-depot->kill-tags ()
  (interactive)
  (if (y-or-n-p "Are you sure you want to delete the selected tag(s)?")
      (progn
        (dolist (tag (or (bui-list-get-marked-id-list)
                         (list (bui-list-current-id))))
          (delete-directory tag t))
        (revert-buffer nil t))))

;; note that this mode map is created by default based on the
;; definition of 'cuts 'list
(let ((map cuts-depot-list-mode-map))
  (define-key map (kbd "RET") 'cuts-depot->switch-to-folder)
  (define-key map (kbd "x")   'cuts-depot->kill-tags))

;;; interactive functions
;; list all tags
;;;###autoload
(defun cuts-depot-show-tags ()
  "Display a list of buffers."
  (interactive)
  (bui-get-display-entries 'cuts-depot 'list))

;; switch to folder
(defun cuts-depot->switch-to-folder ()
  (interactive)
  (dired (bui-list-current-id)))

;; kill marked tags or the current tag
(defun cuts-depot->kill-tags ()
  (interactive)
  (if (y-or-n-p "Are you sure you want to delete the selected tag(s)?")
      (progn
        (dolist (tag (or (bui-list-get-marked-id-list)
                         (list (bui-list-current-id))))
          (delete-directory tag t))
        (revert-buffer nil t))))

;; archive marked tags or the current tag into tar.gz
(defun cuts-depot->archive-tags ()
  (interactive)
  (let* ((tag (or (bui-list-get-marked-id-list)
                  (list (bui-list-current-id))))
         (output-name (bui-assoc-value
                       (car (bui-entries-by-ids
                             (bui-current-entries) tag))
                       'tag)))
    (shell-command (concat
                    "tar -czvf " depot "/"
                    output-name ".tar.gz "
                    (s-join " " tag)))))

(provide 'cuts-depot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cuts-depot.el ends here

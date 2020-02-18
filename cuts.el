;;; cuts.el --- Cuts utilities plugin for emacs  -*- lexical-binding: t -*-

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
;; with cuts results

;;; Code:

(require 'dash)
(require 'bui)

;;; user defined parameters
;; define depot
(setq depot "/scratch/gpfs/yilung/depot/")
;; define categories of output of interests
(setq categories '("Calibration" "Pathologies" "Postprocess" "SelectedTODs" "TODCuts" "darkDets"))

;;; utility functions
;; get last modified
(defun get-last-modified (file)
  "Returns the last modified date of a FILE."
  (format-time-string "%Y-%m-%d %T"
                      (nth 5 (file-attributes file))))

;;; cuts functions
;; get tags for each category
(defun cuts->get-tags (category)
  (mapcar (lambda (tag) `((category . ,category) (tag . ,tag) (last-modified . ,(get-last-modified (concat depot category)))))
          (cddr (directory-files
                 (concat depot category)))))

;; get all tags from all categories
(defun cuts->get-all-tags ()
  (-flatten-n 1 (mapcar (lambda (category) (cuts->get-tags category))
                        categories)))

;; define bui interface
(bui-define-interface cuts list
  :buffer-name "*Cuts*"
  :get-entries-function 'cuts->get-all-tags
  :format '((category nil 30 t)
            (tag nil 30 t)
            (last-modified nil 30 t))
  :sort-key '(tag))

;; define interactive function to call
(defun cuts-show-tags ()
  "Display a list of buffers."
  (interactive)
  (bui-get-display-entries 'cuts 'list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cuts.el ends here

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
;; get tags for each category
(defun cuts->get-tags (category)
  (mapcar (lambda (tag) `(,category ,tag))
          (cddr (directory-files
                 (concat "/scratch/gpfs/yilung/depot/" category)))))

;; get all tags from all categories
(defun cuts->get-all-tags ()
  (-flatten-n 1 (mapcar (lambda (category) (get-tags category))
                        categories)))

;; get each tabulated entry
(defun cuts->entry (entry)
  `((category . ,(car entry))
    (tag . ,(cadr entry))))

;; get all tabulated entry
(defun cuts->get-entries ()
  (mapcar 'cuts->entry (cuts->get-all-tags)))

;; define bui interface
(bui-define-interface cuts list
  :buffer-name "*Cuts*"
  :get-entries-function 'cuts->get-entries
  :format '((category nil 30 t)
            (tag nil 30 t)))

;; define interactive function to call
(defun show-cuts-tags ()
  "Display a list of buffers."
  (interactive)
  (bui-get-display-entries 'cuts 'list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cuts.el ends here

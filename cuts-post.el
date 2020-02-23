;;; cuts-post.el --- Cuts post-process with emacs  -*- lexical-binding: t -*-

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

;; This library aims to provide an easier method for me to work with
;; the post-processing script. At this moment what bothers me the most
;; is managing the modules that i run in the post.ini file. I will try
;; to implement a helm based approach to help me manage them more easily

;;; Code:

(require 'helm)

;; Define the modules instead of parsing them from the library so that
;; i can maintain a good running order between them as some of them
;; have inter-dependencies
(setq cuts-post-modules '("collect_crit"
                          "select_good_tods"
                          "plot_cuts_thresholds"
                          "plot_array"
                          "plot_killed_array"
                          "get_flatfield"
                          "plot_ff"
                          "todlist_for_map"
                          "plot_planet_cal"
                          "plot_ld_loading"
                          "plot_live_fraction"
                          "plot_rms_gain"
                          "report"
                          "export_json"))

(defun cuts-post->insert-modules (candidate)
  "Insert modules at the end of the current module list"
  (goto-char (point-min))
  (re-search-forward "^pipeline")
  (forward-paragraph)
  (backward-char)
  (delete-horizontal-space)
  (insert " ")
  (insert (mapconcat 'identity (helm-marked-candidates) " ")))

(defun cuts-post->update-modules (candidate)
  "Update the modules instead of inserting"
  (goto-char (point-min))
  (re-search-forward "^pipeline")
  (mark-paragraph)
  (next-line)
  (delete-region (region-beginning) (region-end))
  (insert "pipeline = ")
  (insert (mapconcat 'identity (helm-marked-candidates) " "))
  (insert "\n"))

;; define cuts post-processing modules as a helm source
(setq cuts-post-helm-source
      '((name . "cuts-post-helm")
        (candidates . cuts-post-modules)
        (action . (("insert" . cuts-post->insert-modules)
                   ("update" . cuts-post->update-modules)))))

;;; interactive function
;;;###autoload
(defun cuts-post-modules-helm ()
  "Load postprocessing modules with helm"
  (interactive)
  (helm :sources '(cuts-post-helm-source)))


(provide 'cuts-post)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cuts-post.el ends here

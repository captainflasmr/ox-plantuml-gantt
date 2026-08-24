;;; ox-plantuml-gantt.el --- Org Export Backend for PlantUML Gantt Charts -*- lexical-binding: t; -*-

;; Author: James
;; Keywords: org, gantt, plantuml, export
;; Package-Requires: ((emacs "27.1") (org "9.3"))

;;; Commentary:
;; An Org mode export backend that converts structured Org headlines,
;; timestamps (SCHEDULED/DEADLINE), properties (EFFORT, BLOCKER, COMPLETED),
;; and TODO statuses into PlantUML Gantt syntax.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'ox)
(require 'subr-x)

(defgroup ox-plantuml-gantt nil
  "Options for exporting Org documents to PlantUML Gantt charts."
  :tag "Org PlantUML Gantt"
  :group 'org-export)

(defcustom ox-plantuml-gantt-print-scale "daily"
  "Default print scale for Gantt chart (daily, weekly, monthly)."
  :type '(choice (const "daily") (const "weekly") (const "monthly"))
  :group 'ox-plantuml-gantt)

(defcustom ox-plantuml-gantt-close-weekends t
  "When non-nil, mark Saturdays and Sundays as closed days in PlantUML."
  :type 'boolean
  :group 'ox-plantuml-gantt)

(defun ox-plantuml-gantt--format-duration (effort-str)
  "Convert Org EFFORT-STR (e.g., '2d', '4h', '1w', '12:00') into PlantUML duration."
  (if (not effort-str)
      "1 days"
    (cond
     ((string-match "\\([0-9]+\\)w" effort-str)
      (format "%s days" (* 7 (string-to-number (match-string 1 effort-str)))))
     ((string-match "\\([0-9]+\\)d" effort-str)
      (format "%s days" (match-string 1 effort-str)))
     ((string-match "\\([0-9]+\\)h" effort-str)
      (format "%s hours" (match-string 1 effort-str)))
     ((string-match "\\([0-9]+\\):\\([0-9]+\\)" effort-str)
      (format "%s hours" (match-string 1 effort-str)))
     (t (format "%s days" effort-str)))))

(defun ox-plantuml-gantt--format-timestamp (timestamp)
  "Format TIMESTAMP object as an ISO date string."
  (if (fboundp 'org-format-timestamp)
      (org-format-timestamp timestamp "%Y-%m-%d")
    (with-no-warnings (org-timestamp-format timestamp "%Y-%m-%d"))))

(defun ox-plantuml-gantt--earliest-task-date (info)
  "Return earliest SCHEDULED or DEADLINE date in INFO as an ISO string.
Return nil when the parse tree contains no such dates."
  (let* ((tree (plist-get info :parse-tree))
         (date-lists
          (org-element-map tree 'headline
            (lambda (h)
              (delq nil
                    (list (when-let* ((sched (org-element-property :scheduled h)))
                            (ox-plantuml-gantt--format-timestamp sched))
                          (when-let* ((dead (org-element-property :deadline h)))
                            (ox-plantuml-gantt--format-timestamp dead)))))))
         (dates (apply #'append (delq nil date-lists))))
    (when dates
      (car (sort (copy-sequence dates) #'string<)))))

(defun ox-plantuml-gantt-headline (headline contents info)
  "Transcode HEADLINE element into PlantUML Gantt syntax."
  (let* ((title-raw (org-element-property :raw-value headline))
         ;; Clean percentage cookies from title string
         (title (string-trim (replace-regexp-in-string "\\[[0-9]+%\\]" "" title-raw)))
         (todo (org-element-property :todo-keyword headline))
         (sched-obj (org-element-property :scheduled headline))
         (dead-obj (org-element-property :deadline headline))
         (start (when sched-obj (ox-plantuml-gantt--format-timestamp sched-obj)))
         (end (when dead-obj (ox-plantuml-gantt--format-timestamp dead-obj)))
         (effort (org-element-property :EFFORT headline))
         (blocker (org-element-property :BLOCKER headline))
         (completed-prop (org-element-property :COMPLETED headline))
         (has-children (org-element-map (org-element-contents headline) 'headline #'identity info t))
         (out '()))

    (if has-children
        ;; Parent headlines act as visual section headers in PlantUML
        (concat "-- " title " --\n" contents)
      ;; Leaf headlines act as tasks
      (when (or start end blocker)
        (let ((task-id (format "[%s]" title)))
          ;; Timing setup
          (cond
           ((and start effort)
            (push (format "%s starts %s and lasts %s" task-id start (ox-plantuml-gantt--format-duration effort)) out))
           ((and start end)
            (push (format "%s starts %s and ends %s" task-id start end) out))
           (start
            (push (format "%s starts %s" task-id start) out))
           (end
            (push (format "%s ends %s" task-id end) out))
           (blocker
            (push (format "%s starts at [%s]'s end" task-id blocker) out)))

          ;; Optional duration fallback when dependent on blocker
          (when (and blocker effort (not start))
            (push (format "%s lasts %s" task-id (ox-plantuml-gantt--format-duration effort)) out))

          ;; Progress calculation
          (cond
           (completed-prop
            (push (format "%s is %s%% completed" task-id completed-prop) out))
           ((equal todo "DONE")
            (push (format "%s is 100%% completed" task-id) out))
           ((equal todo "IN-PROGRESS")
            (push (format "%s is 50%% completed" task-id) out)))

          (concat (string-join (nreverse out) "\n") "\n" contents))))))

(defun ox-plantuml-gantt-template (contents info)
  "Wrap CONTENTS in PlantUML Gantt chart tags and header rules."
  (let* ((scale (if (plist-member info :gantt-print-scale)
                    (plist-get info :gantt-print-scale)
                  ox-plantuml-gantt-print-scale))
         (weekends (org-not-nil
                    (if (plist-member info :gantt-close-weekends)
                        (plist-get info :gantt-close-weekends)
                      ox-plantuml-gantt-close-weekends)))
         (project-start (ox-plantuml-gantt--earliest-task-date info)))
    (concat
     "@startgantt\n"
     (format "printscale %s\n" scale)
     (if weekends
         "saturdays are closed\nsundays are closed\n"
       "")
     (when project-start
       (format "Project starts %s\n" project-start))
     "\n"
     contents
     "@endgantt\n")))

;; Register export backend
(org-export-define-backend 'plantuml-gantt
  '((headline . ox-plantuml-gantt-headline)
    (template . ox-plantuml-gantt-template))
  :options-alist
  '((:gantt-print-scale "GANTT_PRINT_SCALE" nil ox-plantuml-gantt-print-scale)
    (:gantt-close-weekends "GANTT_CLOSE_WEEKENDS" nil ox-plantuml-gantt-close-weekends t))
  :menu-entry
  '(?g "Export to PlantUML Gantt"
       ((?p "As PlantUML buffer" ox-plantuml-gantt-export-as-plantuml)
        (?f "To PlantUML file" ox-plantuml-gantt-export-to-plantuml))))

;;;###autoload
(defun ox-plantuml-gantt-export-as-plantuml (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a PlantUML Gantt buffer."
  (interactive)
  (org-export-to-buffer 'plantuml-gantt "*Org PlantUML Gantt Export*"
    async subtreep visible-only body-only ext-plist
    (lambda ()
      (when (fboundp 'plantuml-mode)
        (plantuml-mode)))))

;;;###autoload
(defun ox-plantuml-gantt-export-to-plantuml (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a PlantUML Gantt file (.puml)."
  (interactive)
  (let ((outfile (org-export-output-file-name ".puml" subtreep)))
    (org-export-to-file 'plantuml-gantt outfile
      async subtreep visible-only body-only ext-plist)))

(provide 'ox-plantuml-gantt)
;;; ox-plantuml-gantt.el ends here
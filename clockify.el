;;; clockify.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 oxalorg
;;
;; Author: Mitesh <http://github/oxalorg>
;; Maintainer: John Doe <john@doe.com>
;; Created: February 24, 2021
;; Modified: February 24, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/oxalorg/emacs-clockify
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(require 'json)
(require 'request)

(defvar clockify-api-key)
(defvar clockify-workspace)
(defvar clockify-projects "")
(defvar clockify-project-client "")
(defvar clockify-api-base-url)

(setq clockify-times '("00:00"
                "00:15" "00:30" "00:45" "01:00"
                "01:15" "01:30" "01:45" "02:00"
                "02:15" "02:30" "02:45" "03:00"
                "03:15" "03:30" "03:45" "04:00"
                "04:15" "04:30" "04:45" "05:00"
                "05:15" "05:30" "05:45" "07:00"
                "07:15" "07:30" "07:45" "08:00"
                "08:15" "08:30" "08:45" "09:00"
                "09:15" "09:30" "09:45" "10:00"
                "10:15" "10:30" "10:45" "11:00"
                "11:15" "11:30" "11:45" "12:00"
                "12:15" "12:30" "12:45" "13:00"
                "13:15" "13:30" "13:45" "14:00"
                "14:15" "14:30" "14:45" "15:00"
                "15:15" "15:30" "15:45" "16:00"
                "16:15" "16:30" "16:45" "17:00"
                "17:15" "17:30" "17:45" "18:00"
                "18:15" "18:30" "18:45" "19:00"
                "19:15" "19:30" "19:45" "20:00"
                "20:15" "20:30" "20:45" "21:00"
                "21:15" "21:30" "21:45" "22:00"
                "22:15" "22:30" "22:45" "23:00"
                "23:15" "23:30" "23:45"))

(defun clockify-api (method path &optional data)
  (let ((response (request-response-data
                   (request
                     (concat clockify-api-base-host path)
                     :type method
                     :data (json-encode data)
                     :parser 'json-read
                     :sync t
                     :headers `(("Content-Type" . "application/json")
                                ("X-Api-Key" . ,clockify-api-key))))))
    response))

(defun clockify-get-projects ()
  (interactive)
  (setq clockify-projects (clockify-api "GET" (concat "/workspaces/" clockify-workspace "/projects")))
  (setq clockify-project-client
        (mapcar (lambda (project)
                  (let ((clientName (cdr (assoc 'clientName project)))
                        (name (cdr (assoc 'name project)))
                        (id (cdr (assoc 'id project))))
                    (list clientName name id)))
                clockify-projects)))

(defun clockify-clock (selected-project start-time end-time desc)
  (interactive
   (list (completing-read
          "Choose clockify project: "
          (mapcar (lambda (project)
                    (concat
                     (nth 2 project)
                     " - "
                     (nth 0 project)
                     " / "
                     (nth 1 project)))
                  clockify-project-client))
         (completing-read "Start time (hh:mm:ss):" clockify-times nil t)
         (completing-read "End time (hh:mm:ss):" clockify-times nil t)
         ;; add a description field since it is required for some projects
         (completing-read "Description: " nil nil nil)))

  (let ((pid (car (split-string selected-project "\s")))
        (now (decode-time))
        (entry (format-time-string "%Y-%m-%dT%TZ" (current-time)))
        (start-utc (get-iso-utc-for-clock-time start-time))
        (end-utc (get-iso-utc-for-clock-time end-time))
        (description desc))
    (message "Here")
    (message pid)
    (message start-utc)
    (message end-utc)
    (clockify-api "POST" (concat "/workspaces/" clockify-workspace "/time-entries")
                  (list
                    (cons "start" start-utc)
                    (cons "end" end-utc)
                    (cons "description" desc)
                    (cons "projectId" pid)))))

(defun clockify-clock-start (selected-project )
  (interactive
   (list (completing-read
          "Choose clockify project: "
          (mapcar (lambda (project)
                    (concat
                     (nth 2 project)
                     " - "
                     (nth 0 project)
                     " / "
                     (nth 1 project)))
                  clockify-project-client))
         ))
    (clockify-api "POST" (concat "/workspaces/" clockify-workspace "/time-entries")
                  (list
                    (cons "start" (shell-command-to-string "echo -n (date -d '-1 hour' +%Y-%m-%dT%TZ)" ))
                  (cons "projectId" (car (split-string selected-project "\s"))))))

(defun clockify-clock-stop ()
        (interactive)
      (clockify-api "PATCH" (concat "/workspaces/" clockify-workspace "/user/" clockify-user-id "/time-entries")
                    (list (cons  "end"  (shell-command-to-string "echo -n (date -d '-1 hour' +%Y-%m-%dT%TZ)" )))))

(defun get-iso-utc-for-clock-time (time)
  (let ((split (split-string time "[\s:]"))
        (now (decode-time)))
    (setf (elt now 2) (if (string-equal (elt split 2) "PM")
                          (+ 12 (string-to-number (elt split 0)))
                        (string-to-number (elt split 0)))
          (elt now 1) (string-to-number (elt split 1))
          (elt now 0) 0)
    (format-time-string "%Y-%m-%dT%TZ"
                        (encode-time now) "UTC")))

(provide 'clockify)
;;; clockify.el ends here

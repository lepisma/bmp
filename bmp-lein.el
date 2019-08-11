;;; bmp-lein.el --- Bmper for leiningen projects -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Bmper for leiningen projects
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'bmp-base)
(require 'eieio)

(defclass bmp-lein-project (bmp-project)
  ((root-dir :initarg :root-dir)
   (project-file :initarg :project-file))
  "Clojure project package with leiningen")

(defun bmp-lein-get-project ()
  (let ((project-file "project.clj"))
    (when (file-exists-p (concat default-directory project-file))
      (bmp-lein-project :root-dir default-directory
                        :project-file project-file))))

(cl-defmethod bmp-get-version-str ((obj bmp-lein-project))
  (let ((project-file-path (concat (oref obj :root-dir) (oref obj :project-file))))
    (save-excursion
      (with-current-buffer (find-file-noselect project-file-path)
        (let ((buffer-text (buffer-substring-no-properties (point-min) (point-max))))
          (nth 2 (member 'defproject (car (read-from-string buffer-text)))))))))

(cl-defmethod bmp-set-version-str ((obj bmp-lein-project))
  (let ((project-file-path (concat (oref obj :root-dir) (oref obj :project-file)))
        (old-version-str (oref obj :version-str)))
    (save-excursion
      (with-current-buffer (find-file-noselect project-file-path)
        (goto-char (point-min))
        (re-search-forward (regexp-quote old-version-str))
        (replace-match version-str)
        (save-buffer)))))

(cl-defmethod bmp-changed-files ((obj bmp-lein-project))
  (list (oref obj :project-file)))

(provide 'bmp-lein)

;;; bmp-lein.el ends here

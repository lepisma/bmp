;;; bmp-bmpfile.el --- Bmper for bmpfile based projects -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Bmper for bmpfile based projects
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

(defclass bmp-bmpfile-project (bmp-project)
  ((root-dir :initarg :root-dir)
   (bmpfile :initarg :bmpfile)
   (spec :initform nil :initarg :spec))
  "A project with bmpfile. A .bmpfile specifies an alist mapping
file paths relative to project root along with a regex pattern
that maps to a version string. The pattern should capture the
version string in its first group.

NOTE: We might need to change the way of specification but most
      of the cases work okay with regex.")

(defun bmp-bmpfile-get-project ()
  "Return a bmpfile project."
  (let ((bmpfile (concat default-directory ".bmpfile")))
    (when (file-exists-p bmpfile)
      (bmp-bmpfile-project :root-dir default-directory
                           :bmpfile bmpfile
                           :spec (car (read-from-string
                                       (with-temp-buffer
                                         (insert-file-contents-literally bmpfile)
                                         (buffer-string))))))))

(defun bmp-bmpfile-read-version-str (spec-pair)
  "Read version string provided the spec pair of filepath and
regex pattern. If pattern doesn't match, we return null."
  (save-excursion
    (with-current-buffer (find-file-noselect (car spec-pair))
      (goto-char (point-min))
      (when (re-search-forward (cdr spec-pair) nil t)
        (match-string-no-properties 1)))))

(defun bmp-bmpfile-version-strs-consistent? (version-strs)
  "Tell if all provided version strings are non-nil and same."
  (and version-strs
       (not (cl-some #'null version-strs))
       (cl-every (lambda (str) (string= (car version-strs) str)) version-strs)))

(defun bmp-bmpfile-write-version-str (spec-pair version-str)
  (save-excursion
    (with-current-buffer (find-file-noselect (car spec-pair))
      (goto-char (point-min))
      ;; We now assume that the regex 'should' match
      (re-search-forward (cdr spec-pair))
      (replace-match version-str nil nil nil 1)
      (save-buffer))))

(cl-defmethod bmp-get-version-str ((obj bmp-bmpfile-project))
  "Return string representation of project version. We also do a
check here to make sure that all patterns are same."
  (let ((version-strs (mapcar #'bmp-bmpfile-read-version-str (oref obj :spec))))
    (unless (bmp-bmpfile-version-strs-consistent? version-strs)
      (error "Version strings inconsistent %s" version-strs))
    (car version-strs)))

(cl-defmethod bmp-set-version-str ((obj bmp-bmpfile-project) version-str)
  "Set version string in all patterns."
  (dolist (spec-pair (oref obj :spec))
    (bmp-bmpfile-write-version-str spec-pair version-str)))

(cl-defmethod bmp-changed-files ((obj bmp-bmpfile-project))
  (mapcar #'car (oref obj :spec)))

(provide 'bmp-bmpfile)

;;; bmp-bmpfile.el ends here

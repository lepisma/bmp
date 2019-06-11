;;; bmp-base.el --- Base components for bmp -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Base components for bmp
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

(require 'cl-lib)
(require 'eieio)
(require 'magit)

;; Forward declaration, definition is in bmp.el
(defvar bmp-release-branch)

(defun bmp-parse-version (version-str)
  "Convert semver VERSION-STR to a list of numbers."
  (mapcar #'string-to-number (split-string version-str "\\.")))

(defun bmp-format-version (version)
  "Convert list of three numbers (VERSION) to semver."
  (string-join (mapcar #'number-to-string version) "."))

(defun bmp-bump-version (version bmp-type)
  "Return new version for the BMP-TYPE"
  (cl-destructuring-bind (major minor patch) version
    (cl-ecase bmp-type
      ('patch (list major minor (+ 1 patch)))
      ('minor (list major (+ 1 minor) 0))
      ('major (list (+ 1 major) 0 0)))))

(defun bmp-bump-version-str (version-str bmp-type)
  "Return new VERSION-STR based on BMP-TYPE."
  (bmp-format-version (bmp-bump-version (bmp-parse-version version-str) bmp-type)))

(defclass bmp-project ()
  ((version-str :initarg :version-str))
  "Base class for a project"
  :abstract t)

(cl-defgeneric bmp-get-version-str ((obj bmp-project))
  "Method to get version-str for a project. This has to be
implemented for a new project type.")

(cl-defgeneric bmp-changed-files ((obj bmp-project))
  "List of files which are affected after a version bump.")

(cl-defmethod bmp-bump ((obj bmp-project) bmp-type)
  "Update project's version based on given BMP-TYPE."
  (let ((new-version-str (bmp-bump-version-str (oref obj :version-str) bmp-type)))
    (bmp-set-version-str obj new-version-str)
    (bmp-commit obj)
    (bmp-tag obj)))

(cl-defmethod bmp-set-version-str :after ((obj bmp-project) version-str)
  "Set version-str in the object too. A primary set method still
needs to be implemented which actually goes in the file system
and updates version in files."
  (oset obj :version-str version-str))

(cl-defmethod bmp-commit ((obj bmp-project))
  "Simple blocking git add and commit. Not using magit for now
since I want the commands to block before running tag. There
should be a magit way here."
  (let ((args (string-join (mapcar #'shell-quote-argument (bmp-changed-files obj)) " ")))
    (shell-command-to-string (format "git add %s" args))
    (shell-command-to-string (format "git commit -m \"%s\"" (oref obj :version-str)))))

(cl-defmethod bmp-tag ((obj bmp-project))
  "Tag release branch with given version-str."
  (magit-tag-create (oref obj :version-str) bmp-release-branch))

(cl-defmethod initialize-instance :after ((obj bmp-project) &rest _args)
  "Initializer which handles setting the current version-str in
the newly created object."
  (oset obj :version-str (bmp-get-version-str obj)))

(provide 'bmp-base)

;;; bmp-base.el ends here

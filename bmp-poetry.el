;;; bmp-poetry.el --- Poetry support for bmp -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Poetry support for bmp
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

(defclass bmp-poetry-project (bmp-project)
  ((name :initarg :name)
   (root-dir :initarg :root-dir)
   (toml-file :initarg :toml-file)
   (test-file :initarg :test-file)
   (init-file :initarg :init-file))
  "A poetry project")

(defun bmp-poetry-get-project ()
  (let ((toml-file "pyproject.toml"))
    (when (file-exists-p (concat default-directory toml-file))
      (let* ((name (bmp-poetry-get-name (concat default-directory toml-file)))
             (test-filepath (format "tests/test_%s.py" name))
             (init-filepath (format "%s/__init__.py" name)))
        (bmp-poetry-project
         :name name
         :root-dir default-directory
         :toml-file toml-file
         :test-file (when (file-exists-p test-filepath) test-filepath)
         :init-file (when (file-exists-p init-filepath) init-filepath))))))

(cl-defmethod bmp-get-version-str ((obj bmp-poetry-project))
  (let ((toml-path (concat (oref obj :root-dir) (oref obj :toml-file))))
    (bmp-poetry-get-meta toml-path "version")))

(cl-defmethod bmp-set-version-str ((obj bmp-poetry-project) version-str)
  (let ((default-directory (oref obj :root-dir)))
    (shell-command-to-string (format "poetry version %s" version-str))
    (when (oref obj :test-file)
      (bmp-poetry-set-test (oref obj :test-file) version-str))
    (when (oref obj :init-file)
      (bmp-poetry-set-init (oref obj :init-file) version-str))))

(cl-defmethod bmp-changed-files ((obj bmp-poetry-project))
  (cl-remove-if #'null (list (oref obj :toml-file)
                             (oref obj :test-file)
                             (oref obj :init-file))))

(defun bmp-poetry-set-init (init-path version-str)
  (save-excursion
    (with-current-buffer (find-file-noselect init-path)
      (goto-char (point-min))
      (re-search-forward "^__version__ = [\"']?\\(.*?\\)[\"']?$")
      (replace-match version-str nil nil nil 1)
      (save-buffer))))

(defun bmp-poetry-set-test (test-path version-str)
  (save-excursion
    (with-current-buffer (find-file-noselect test-path)
      (goto-char (point-min))
      (re-search-forward "assert __version__ == [\"']?\\(.*?\\)[\"']?$")
      (replace-match version-str nil nil nil 1)
      (save-buffer))))

(defun bmp-poetry-get-name (toml-path)
  "Return name of the package converted according to python
naming convention."
  (replace-regexp-in-string "-" "_" (bmp-poetry-get-meta toml-path "name")))

(defun bmp-poetry-get-meta (toml-path key)
  "Parse values from toml"
  (save-excursion
    (with-current-buffer (find-file-noselect toml-path)
      (goto-char (point-min))
      (re-search-forward (format "^%s ?= ?[\"']?\\(.*?\\)[\"']?$" key))
      (match-string-no-properties 1))))

(provide 'bmp-poetry)

;;; bmp-poetry.el ends here

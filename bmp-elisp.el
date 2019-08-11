;;; bmp-elisp.el --- Elisp support for bmp -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Elisp support for bmp
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

(defclass bmp-elisp-project (bmp-project)
  ((root-dir :initarg :root-dir)
   (main-file :initarg :main-file))
  "An elisp project")

(defun bmp-elisp-get-project ()
  "Return project if we are in an elisp project (based on
default-directory."
  (let ((main-file (car (last (directory-files default-directory nil "^.*\\.el$")))))
    (save-excursion
      (when (with-current-buffer (find-file-noselect (concat default-directory main-file))
              (goto-char (point-min))
              (re-search-forward "^;; Version:" nil t))
        (bmp-elisp-project :root-dir default-directory :main-file main-file)))))

(cl-defmethod bmp-get-version-str ((obj bmp-elisp-project))
  "Parse string representation of version from main project file."
  (let ((file-path (concat (oref obj :root-dir) (oref obj :main-file))))
    (save-excursion
      (with-current-buffer (find-file-noselect file-path)
        (goto-char (point-min))
        (re-search-forward "^;; Version: \\(.*?\\)$")
        (match-string-no-properties 1)))))

(cl-defmethod bmp-set-version-str ((obj bmp-elisp-project) version-str)
  "Set string version in main file."
  (let ((file-path (concat (oref obj :root-dir) (oref obj :main-file))))
    (save-excursion
      (with-current-buffer (find-file-noselect file-path)
        (goto-char (point-min))
        (re-search-forward "^;; Version: \\(.*?\\)$")
        (replace-match version-str nil nil nil 1)
        (save-buffer)))))

(cl-defmethod bmp-changed-files ((obj bmp-elisp-project))
  (list (oref obj :main-file)))

(provide 'bmp-elisp)

;;; bmp-elisp.el ends here

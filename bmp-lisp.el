;;; bmp-lisp.el --- Bmper for lisp project -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Bmper for lisp project
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


(require 'eieio)

(defclass bmp-lisp-project ()
  ((root-dir :initarg :root-dir)
   (system-file :initarg :system-file))
  "A common lisp project")

(defun bmp-lisp-get-project ()
  (let ((system-file (car (last (directory-files default-directory nil "^.*\\.asd$")))))
    (bmp-lisp-project :root-dir default-directory :system-file system-file)))

(cl-defmethod bmp-get-version ((obj bmp-lisp-project))
  (let ((system-file-path (concat (oref obj :root-dir) (oref obj :system-file))))
    (with-current-buffer (find-file-noselect system-file-path)
      (let ((buffer-text (buffer-substring-no-properties (point-min) (point-max))))
        (plist-get (car (read-from-string buffer-text)) :version)))))

(cl-defmethod bmp-set-version ((obj bmp-lisp-project) version-str)
  (let ((system-file-path (concat (oref obj :root-dir) (oref obj :system-file)))
        (old-version-str (bmp-get-version obj))) ; TODO: should also provide this in the class maybe
    (with-current-buffer (find-file-noselect system-file-path)
      (goto-char (point-min))
      (re-search-forward old-version-str)
      (replace-match version-str)
      (save-buffer))))

(cl-defmethod bmp-get-files ((obj bmp-lisp-project))
  (list (oref obj :system-file)))

(provide 'bmp-lisp)

;;; bmp-lisp.el ends here

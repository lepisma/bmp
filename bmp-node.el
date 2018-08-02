;;; bmp-node.el --- Node support for bmp -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Node support for bmp
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
(require 'json)

(defclass bmp-node-project ()
  ((root-dir :initarg :root-dir)
   (json-file :initarg :json-file))
  "A node project")

(defun bmp-node-get-project ()
  (let ((json-file "package.json"))
    (if (file-exists-p (concat default-directory json-file))
        (bmp-node-project :root-dir default-directory
                          :json-file json-file))))

(cl-defmethod bmp-get-version ((obj bmp-node-project))
  (let* ((json-path (concat (oref obj :root-dir) (oref obj :json-file)))
         (data (json-read-file json-path)))
    (cdr (assoc 'version data))))

(cl-defmethod bmp-set-version ((obj bmp-node-project) version-str)
  (let ((default-directory (oref obj :root-dir)))
    (shell-command-to-string (format "npm --no-git-tag-version version %s" version-str))))

(cl-defmethod bmp-get-files ((obj bmp-node-project))
  (list (oref obj :json-file)))

(provide 'bmp-node)

;;; bmp-node.el ends here

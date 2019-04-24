;;; bmp.el --- Version bmper for projects -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.2.5
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/bmp

;;; Commentary:

;; Version bmper for projects. Assumes semver.
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

(require 'magit)
(require 'helm)
(require 'projectile)
(require 'cl-lib)

(require 'bmp-node)
(require 'bmp-poetry)
(require 'bmp-elisp)
(require 'bmp-lisp)

(defcustom bmp-project-fns
  '(bmp-poetry-get-project
    bmp-node-get-project
    bmp-elisp-get-project
    bmp-lisp-get-project)
  "Functions for getting projects")

;;;###autoload
(defun bmp ()
  "Bump version for current project."
  (interactive)
  (helm :sources (helm-build-sync-source "bump type"
                   :candidates '(("patch" . patch) ("minor" . minor) ("major" . major))
                   :action #'bmp-bump)
        :buffer "*helm bump*"))

(defun bmp-bump (bmp-type)
  "Bump version using the given bmp-type"
  (let ((default-directory (projectile-project-root)))
    (let ((project (bmp-get-project bmp-project-fns)))
      (cl-block nil
        (cond ((null project) (cl-return (message "No project detected")))
              ((bmp-git-dirty-p) (cl-return (message "Git repository dirty")))
              ((not (bmp-git-master-p)) (cl-return (message "Not on master"))))

        (let* ((version-str (bmp-get-version project))
               (new-ver-str (bmp-new-version version-str bmp-type)))
          (bmp-set-version project new-ver-str)
          (let ((affected-files (bmp-get-files project)))
            (bmp-commit affected-files new-ver-str)
            (bmp-tag new-ver-str)))))))

(defun bmp-git-dirty-p ()
  (let ((diffs (mapcar #'string-trim (split-string (shell-command-to-string "git status --porcelain") "\n"))))
    (not (null (cl-remove-if (lambda (l) (or (string-prefix-p "?" l) (string-equal l ""))) diffs)))))

(defun bmp-git-master-p ()
  (magit-branch-p "master"))

(defun bmp-get-project (fns)
  (unless (null fns)
    (or (funcall (car fns))
        (bmp-get-project (cdr fns)))))

(defun bmp-new-version (version-str bmp-type)
  "Return new version for the BMP-TYPE"
  (cl-destructuring-bind (major minor patch) (bmp-parse-version version-str)
    (bmp-unparse-version
     (cl-ecase bmp-type
       ('patch (list major minor (+ 1 patch)))
       ('minor (list major (+ 1 minor) 0))
       ('major (list (+ 1 major) 0 0))))))

(defun bmp-parse-version (version-str)
  (mapcar #'string-to-number (split-string version-str "\\.")))

(defun bmp-unparse-version (version)
  (string-join (mapcar #'number-to-string version) "."))

(defun bmp-commit (files version-str)
  "Simple blocking git add and commit. Should use magit here someday."
  (let ((args (string-join (mapcar #'shell-quote-argument files) " ")))
    (shell-command-to-string (format "git add %s" args))
    (shell-command-to-string (format "git commit -m \"%s\"" version-str))))

(defun bmp-tag (version-str)
  (magit-tag-create version-str "master"))

(provide 'bmp)

;;; bmp.el ends here

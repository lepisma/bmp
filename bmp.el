;;; bmp.el --- Version bmper for projects -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.5.1
;; Package-Requires: ((emacs "26") (magit "2.90.1") (helm "3.2") (projectile "2.0.0"))
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

(require 'bmp-base)
(require 'bmp-bmpfile)
(require 'bmp-node)
(require 'bmp-poetry)
(require 'bmp-elisp)
(require 'bmp-lein)
(require 'bmp-lisp)

(defcustom bmp-project-fns
  '(bmp-bmpfile-get-project
    bmp-poetry-get-project
    bmp-node-get-project
    bmp-elisp-get-project
    bmp-lein-get-project
    bmp-lisp-get-project)
  "Functions for getting projects")

(defcustom bmp-release-branches (list "master" "main")
  "Branches where releases (and so bumps) happen.")

(defun bmp-current-project ()
  "Return current project if we are in one."
  (let ((default-directory (projectile-project-root)))
    (cl-block nil
      (dolist (fn bmp-project-fns)
        (let ((project (funcall fn)))
          (when project (cl-return project)))))))

(defun bmp-git-dirty-p ()
  "Check if git is dirty. Git not being dirty is a precondition
for bumping. We might need some changes here if this becomes a
problem."
  (let ((diffs (mapcar #'string-trim (split-string (shell-command-to-string "git status --porcelain") "\n"))))
    (not (null (cl-remove-if (lambda (l) (or (string-prefix-p "?" l) (string-equal l ""))) diffs)))))

(defmacro bmp-git-release-branch-p ()
  "Check if we are on the release branch for current project."
  `(or ,@(mapcar #'magit-branch-p bmp-release-branches)))

;;;###autoload
(defun bmp ()
  "Bump version for current project."
  (interactive)
  (let ((project (bmp-current-project)))
    (cl-block nil
      (when (null project)
        (cl-return (message "No project detected")))

      (when (bmp-git-dirty-p)
        (cl-return (message "Git repository dirty")))

      (when (not (bmp-git-release-branch-p))
        ;; TODO: This doesn't look like a good strategy. Maybe we should allow
        ;;       to create custom preconditions.
        (cl-return (message "Not on release branch %s" bmp-release-branches)))

      (let ((version-str (oref project :version-str))
            (bmp-types '(patch minor major)))
        (helm :sources (helm-build-sync-source "bump type"
                         :candidates (mapcar (lambda (bmp-type) (cons (format "%s [%s → %s]"
                                                                         (symbol-name bmp-type)
                                                                         version-str
                                                                         (bmp-bump-version-str version-str bmp-type))
                                                                 bmp-type))
                                             bmp-types)
                         :action (lambda (bmp-type) (bmp-bump project bmp-type)))
              :buffer "*helm bump*")))))

(provide 'bmp)

;;; bmp.el ends here

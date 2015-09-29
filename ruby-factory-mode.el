;;; ruby-factory-mode.el --- Minor mode for Ruby test object generation libraries

;; Author: Skye Shaw <skye.shaw@gmail.com>
;; Version: 0.0.1 (unreleased)
;; URL: http://github.com/sshaw/ruby-factory-mode
;; Created: 2015
;; Keywords: ruby, rails, convenience
;; Package-Requires: ((inflections "1.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;;

(require 'inflections)

;; TODO: Windows issues, Non-Rails projects
(defconst ruby-factory-mode--model-regex "\\(.+\\)/app/models/\\(.+\\)\\.rb\\'")
(defconst ruby-factory-mode--factory-regex "\\(.+\\)/\\(?:test\\|spec\\)/\\(?:factories\\|fabricators\\)/\\(.+\\)\\.rb\\'")
(defconst ruby-factory-mode--fabrication-name-regex "_fabricator\\'")

(define-prefix-command 'ruby-factory-mode-map)
(define-key 'ruby-factory-mode-map (kbd "C-c f t") 'ruby-factory-mode-switch-to-buffer)

(define-minor-mode ruby-factory-mode
  "Minor mode for Ruby test object generation libraries

\\{ruby-factory-mode-map}"
  :lighter " Factory" :keymap ruby-factory-mode-map
  (if ruby-factory-mode))

(defun ruby-factory-mode--build-path (root &rest dirs)
  (apply 'concat (mapcar
		  (lambda (name) (file-name-as-directory name))
		  (push root dirs))))

(defun ruby-factory-mode--model-path (root name)
  (concat (ruby-factory-mode--build-path root "app" "models") name ".rb"))

(defun ruby-factory-mode--factory-girl-factory (root name)
  (let ((factory-path (concat (ruby-factory-mode--build-path root "factories")
			      (concat (pluralize-string name) ".rb"))))
    (when (file-exists-p factory-path)
      factory-path)))

(defun ruby-factory-mode--factory-girl-model (root name)
  (ruby-factory-mode--model-path root (singularize-string name)))

(defun ruby-factory-mode--fabrication-factory (root name)
  (let ((factory-path (concat (ruby-factory-mode--build-path root "fabricators")
			       (concat name "_fabricator.rb"))))
    (when (file-exists-p factory-path)
      factory-path)))

(defun ruby-factory-mode--fabrication-model (root name)
  (ruby-factory-mode--model-path root (replace-regexp-in-string
				       ruby-factory-mode--fabrication-name-regex "" name t)))

(defun ruby-factory-mode--find-model (root name)  (let ((model-path
	 (if (string-match ruby-factory-mode--fabrication-name-regex name)
	     (ruby-factory-mode--fabrication-model root name)
	   (ruby-factory-mode--factory-girl-model root name))))
    (when (file-exists-p model-path)
      model-path)))

(defun ruby-factory-mode--find-factory (root name)
  (let ((spec (ruby-factory-mode--build-path root "spec"))
	(test (ruby-factory-mode--build-path root "test")))
    (cond
     ((file-directory-p spec)
      (or (ruby-factory-mode--factory-girl-factory spec name)
      	  (ruby-factory-mode--fabrication-factory spec name)))
     ((file-directory-p test)
       (or (ruby-factory-mode--factory-girl-factory test name)
	   (ruby-factory-mode--fabrication-factory test name))))))

(defun ruby-factory-mode--find-file (path)
  (let ((case-fold-search))
    (cond
     ((string-match ruby-factory-mode--model-regex path)
      (ruby-factory-mode--find-factory (match-string 1 path) (match-string 2 path)))
     ((string-match ruby-factory-mode--factory-regex path)
      (ruby-factory-mode--find-model (match-string 1 path) (match-string 2 path))))))

(defun ruby-factory-mode--maybe-enable ()
  (let ((case-fold-search))
    (if (and (buffer-file-name)
	     (or (string-match ruby-factory-mode--model-regex (buffer-file-name))
		 (string-match ruby-factory-mode--factory-regex (buffer-file-name))))
	(ruby-factory-mode))))

(defun ruby-factory-mode-switch-to-buffer ()
  (interactive)
  (let ((new-path)
	(cur-path (buffer-file-name)))
    (if (null cur-path)
	(message "Buffer has no file")
      (setq new-path (ruby-factory-mode--find-file cur-path))
      (if new-path
	  (find-file new-path)
	(message "Nothing to switch to")))))

(add-hook 'ruby-mode-hook 'ruby-factory-mode--maybe-enable)

(provide 'ruby-factory-mode)

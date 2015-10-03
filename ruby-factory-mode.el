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

;; TODO: Windows issues, Non-Rails projects, namespaces
(defconst ruby-factory-mode--model-regex "\\(.+\\)/app/models/\\(.+\\)\\.rb\\'")
(defconst ruby-factory-mode--factory-regex "\\(.+\\)/\\(?:test\\|spec\\)/\\(factories\\|fabricators\\)/\\(.+\\)\\.rb\\'")

(make-variable-buffer-local
 (defvar ruby-factory-mode--finder 'ruby-factory-mode--find-factory))

(define-prefix-command 'ruby-factory-mode-map)
(define-key 'ruby-factory-mode-map (kbd "C-c f t") 'ruby-factory-mode-switch-to-buffer)

(define-minor-mode ruby-factory-girl-mode
  "Minor mode for the Ruby factory_girl object generation library

\\{ruby-factory-mode-map}"
  :lighter " Factory" :keymap ruby-factory-mode-map
  (when ruby-factory-girl-mode
    (setq ruby-factory-mode--finder 'ruby-factory-mode--find-factory-girl-model)
    (when (fboundp 'yas-activate-extra-mode)
      (yas-activate-extra-mode 'ruby-factory-girl-mode))))

(define-minor-mode ruby-factory-fabrication-mode
    "Minor mode for the Ruby Fabrication object generation library

\\{ruby-factory-mode-map}"
  :lighter " Fabrication" :keymap ruby-factory-mode-map
  (when ruby-factory-fabrication-mode
    (setq ruby-factory-mode--finder 'ruby-factory-mode--find-fabrication-model)
    (when (fboundp 'yas-activate-extra-mode)
      (yas-activate-extra-mode 'ruby-factory-fabrication-mode))))

(define-minor-mode ruby-factory-mode
  "Minor mode for Ruby test object generation libraries

\\{ruby-factory-mode-map}"
  :lighter "" :keymap ruby-factory-mode-map
  (when ruby-factory-mode
    (setq ruby-factory-mode--finder 'ruby-factory-mode--find-factory)))

(defun ruby-factory-mode--factory= (name)
  (and (string-match ruby-factory-mode--factory-regex (buffer-file-name))
       (string= (match-string 2 (buffer-file-name)) name)))

(defun ruby-factory-mode--model-p ()
  (string-match ruby-factory-mode--model-regex (buffer-file-name)))

(defun ruby-factory-mode--factory-girl-p ()
  (ruby-factory-mode--factory= "factories"))

(defun ruby-factory-mode--fabrication-p ()
  (ruby-factory-mode--factory= "fabricators"))

(defun ruby-factory-mode--build-path (root &rest dirs)
  (apply 'concat (mapcar
		  (lambda (name) (file-name-as-directory name))
		  (push root dirs))))

(defun ruby-factory-mode--model-path (root name)
  (concat (ruby-factory-mode--build-path root "app" "models") name ".rb"))

(defun ruby-factory-mode--factory-girl-factory-path (root name)
  (concat (ruby-factory-mode--build-path root "factories")
	  (concat (pluralize-string name) ".rb")))

(defun ruby-factory-mode--fabrication-factory-path (root name)
  (concat (ruby-factory-mode--build-path root "fabricators")
	  (concat name "_fabricator.rb")))

(defun ruby-factory-mode--factory-girl-model-path (root name)
  (ruby-factory-mode--model-path root (singularize-string name)))

(defun ruby-factory-mode--fabrication-model-path (root name)
  (ruby-factory-mode--model-path root (replace-regexp-in-string
				       "_fabricator\\'" "" name t)))

(defun ruby-factory-mode--find-model (factory-path action)
  (when (string-match ruby-factory-mode--factory-regex factory-path)
    (let* ((root (match-string 1 factory-path))
	   (name (match-string 3 factory-path))
	   (model-path
	    (funcall action root name)))
      (when (file-exists-p model-path)
      model-path))))

(defun ruby-factory-mode--find-fabrication-model (path)
  (ruby-factory-mode--find-model path
				 (lambda (root name)
				   (ruby-factory-mode--fabrication-model-path root name))))

(defun ruby-factory-mode--find-factory-girl-model (path)
  (ruby-factory-mode--find-model path
				 (lambda (root name)
				   (ruby-factory-mode--factory-girl-model-path root name))))

(defun ruby-factory-mode--find-factory (model-path)
  (when (string-match ruby-factory-mode--model-regex model-path)
    (let ((factory-path)
	  (root (match-string 1 model-path))
	  (name (match-string 2 model-path)))
      (catch 'break
	(dolist (test '("spec" "test"))
	  (dolist (factory '("factory-girl" "fabrication"))
	    (setq factory-path
		  (funcall (intern (format "ruby-factory-mode--%s-factory-path" factory))
			   (ruby-factory-mode--build-path root test) name))
	    (when (file-exists-p factory-path)
	      (throw 'break factory-path))))
      nil))))

(defun ruby-factory-mode--maybe-enable ()
  (when (buffer-file-name)
    (cond
     ((ruby-factory-mode--model-p)
      (ruby-factory-mode))
     ((ruby-factory-mode--factory-girl-p)
      (ruby-factory-girl-mode))
     ((ruby-factory-mode--fabrication-p)
      (ruby-factory-fabrication-mode)))))

(defun ruby-factory-mode-switch-to-buffer ()
  (interactive)
  (let ((new-path)
	(cur-path (buffer-file-name)))

    (if (null cur-path)
	(message "Buffer has no file.")

      (setq new-path (funcall ruby-factory-mode--finder cur-path))
      (if (and new-path (file-exists-p new-path))
	  (find-file new-path)
	(message "Nothing to switch to.")))))

(add-hook 'ruby-mode-hook 'ruby-factory-mode--maybe-enable)

(provide 'ruby-factory-mode)

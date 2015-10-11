;;; ruby-factory.el --- Minor mode for Ruby test object generation libraries

;; Author: Skye Shaw <skye.shaw@gmail.com>
;; Version: 0.0.1
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

;; Emacs minor mode for Ruby test object generation libraries.
;; Currently supports factory_girl and Fabrication and only under Rails (for now).
;;
;; Allows one to switch between factory and backing class via `ruby-factory-switch-to-buffer'.
;; YASnippet snippets are provided for all supported libraries.
;;
;; To enable the mode automatically add a `ruby-mode-hook`:
;;  (add-hook 'ruby-mode-hook 'ruby-factory-mode)

;;; Code:

(require 'inflections)

(defconst ruby-factory--model-regex "\\(.+\\)/app/models/\\(.+\\)\\.rb\\'")
(defconst ruby-factory--factory-regex "\\(.+\\)/\\(?:test\\|spec\\)/\\(factories\\|fabricators\\)/\\(.+\\)\\.rb\\'")

(defvar ruby-factory--snippets nil)
(setq ruby-factory--snippets
      (expand-file-name "snippets" (file-name-directory (or load-file-name (buffer-file-name)))))

(make-variable-buffer-local
 (defvar ruby-factory--finder 'ruby-factory--find-factory))

(define-prefix-command 'ruby-factory-mode-map)
(define-key 'ruby-factory-mode-map (kbd "C-c , j") 'ruby-factory-switch-to-buffer)

;;;###autoload
(define-minor-mode ruby-factory-girl-mode
  "Minor mode for the Ruby factory_girl object generation library

\\{ruby-factory-mode-map}"
  :lighter " Factory" :keymap ruby-factory-mode-map
  (when ruby-factory-girl-mode
    (setq ruby-factory--finder 'ruby-factory--find-factory-girl-model))
  (ruby-factory--toggle-yas 'ruby-factory-girl-mode))

;;;###autoload
(define-minor-mode ruby-factory-fabrication-mode
    "Minor mode for the Ruby Fabrication object generation library

\\{ruby-factory-mode-map}"
  :lighter " Fabrication" :keymap ruby-factory-mode-map
  (when ruby-factory-fabrication-mode
    (setq ruby-factory--finder 'ruby-factory--find-fabrication-model))
  (ruby-factory--toggle-yas 'ruby-factory-fabrication-mode))

;;;###autoload
(define-minor-mode ruby-factory-model-mode
  "Minor mode for Ruby test object generation libraries

\\{ruby-factory-mode-map}"
  :lighter "" :keymap ruby-factory-mode-map
  (when ruby-factory-model-mode
    (setq ruby-factory--finder 'ruby-factory--find-factory)))

(defun ruby-factory--factory= (name)
  (and (string-match ruby-factory--factory-regex (buffer-file-name))
       (string= (match-string 2 (buffer-file-name)) name)))

(defun ruby-factory--model-p ()
  (string-match ruby-factory--model-regex (buffer-file-name)))

(defun ruby-factory--factory-girl-p ()
  (ruby-factory--factory= "factories"))

(defun ruby-factory--fabrication-p ()
  (ruby-factory--factory= "fabricators"))

(defun ruby-factory--build-path (root &rest dirs)
  (apply 'concat (mapcar
		  (lambda (name) (file-name-as-directory name))
		  (push root dirs))))

(defun ruby-factory--model-path (root name)
  (concat (ruby-factory--build-path root "app" "models") name ".rb"))

(defun ruby-factory--factory-girl-factory-path (root name)
  (concat (ruby-factory--build-path root "factories")
	  (concat (pluralize-string name) ".rb")))

(defun ruby-factory--fabrication-factory-path (root name)
  (concat (ruby-factory--build-path root "fabricators")
	  (concat name "_fabricator.rb")))

(defun ruby-factory--factory-girl-model-name (name)
  (singularize-string name))

(defun ruby-factory--factory-girl-model-path (root name)
  (ruby-factory--model-path root (ruby-factory--factory-girl-model-name name)))


(defun ruby-factory--fabrication-model-name (name)
  (replace-regexp-in-string  "_fabricator\\'" "" name t))

(defun ruby-factory--fabrication-model-path (root name)
  (ruby-factory--model-path root (ruby-factory--fabrication-model-name name)))

(defun ruby-factory--find-model (factory-path action)
  (when (string-match ruby-factory--factory-regex factory-path)
    (let* ((root (match-string 1 factory-path))
	   (name (match-string 3 factory-path))
	   (model-path
	    (funcall action root name)))
      (when (file-exists-p model-path)
      model-path))))

(defun ruby-factory--find-fabrication-model (path)
  (ruby-factory--find-model path
				 (lambda (root name)
				   (ruby-factory--fabrication-model-path root name))))

(defun ruby-factory--find-factory-girl-model (path)
  (ruby-factory--find-model path
				 (lambda (root name)
				   (ruby-factory--factory-girl-model-path root name))))

(defun ruby-factory--find-factory (model-path)
  (when (string-match ruby-factory--model-regex model-path)
    (let ((factory-path)
	  (root (match-string 1 model-path))
	  (name (match-string 2 model-path)))
      (catch 'break
	(dolist (test '("spec" "test"))
	  (dolist (factory '("factory-girl" "fabrication"))
	    (setq factory-path
		  (funcall (intern (format "ruby-factory--%s-factory-path" factory))
			   (ruby-factory--build-path root test) name))
	    (when (file-exists-p factory-path)
	      (throw 'break factory-path))))
      nil))))

(defun ruby-factory--toggle-yas (mode)
  (when (boundp 'yas-snippet-dirs)
    (if (not (symbol-value mode))
	(yas-deactivate-extra-mode mode)
      ;; Only want to call yas-load-directory once. Better way to check that's been loaded?
      (when (not (member ruby-factory--snippets yas-snippet-dirs))
	(yas-load-directory ruby-factory--snippets)
	(add-to-list 'yas-snippet-dirs ruby-factory--snippets t))

      (yas-activate-extra-mode mode))))

;; YASnippet helpers
;; ----
;; TODO: get "model" from outter factory definition
(defun ruby-factory--yas-factory-girl-model-name ()
  (if (buffer-file-name)
      (ruby-factory--factory-girl-model-name (file-name-base (buffer-file-name)))
    "model"))

(defun ruby-factory--yas-fabrication-model-name ()
  (if (buffer-file-name)
      (ruby-factory--fabrication-model-name (file-name-base (buffer-file-name)))
    "model"))
;; ---

;;;###autoload
(defun ruby-factory-mode (&optional prefix)
  (interactive)
  (when (buffer-file-name)
    (let ((mode
	   (cond
	    ((ruby-factory--model-p)
	     'ruby-factory-model-mode)
	    ((ruby-factory--factory-girl-p)
	     'ruby-factory-girl-mode)
	    ((ruby-factory--fabrication-p)
	     'ruby-factory-fabrication-mode))))
      (if mode
	  (call-interactively mode)))))

;;;###autoload
(defun ruby-factory-switch-to-buffer ()
  (interactive)
  (let ((new-path)
	(cur-path (buffer-file-name)))

    (if (null cur-path)
	(message "Buffer has no file.")

      (setq new-path (funcall ruby-factory--finder cur-path))
      (if (and new-path (file-exists-p new-path))
	  (find-file new-path)
	(message "Nothing to switch to.")))))

(provide 'ruby-factory)
;;; ruby-factory.el ends here

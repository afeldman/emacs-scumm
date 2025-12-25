;;; scumm-script.el --- DSL macros for game scripting -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Anton Feldmann
;; License: GPL-3.0-or-later

;; Author: Anton Feldmann
;; Keywords: games

;;; Commentary:

;; Provides convenient macros for defining game content.

;;; Code:

(require 'scumm-world)

;;; ========== ROOM DEFINITION MACRO ==========

(defmacro defroom (name &rest props)
  "Define a room NAME with PROPS.
Usage:
  (defroom kitchen
    :description \"A messy kitchen.\"
    :objects (fridge table)
    :exits ((north . hallway))
    :on-enter (lambda () (scumm-message \"You enter the kitchen.\")))"
  (declare (indent defun))
  `(scumm-define-room ',name (list ,@props)))

;;; ========== OBJECT DEFINITION MACRO ==========

(defmacro defobject (name &rest props)
  "Define an object NAME with PROPS.
Usage:
  (defobject fridge
    :name \"refrigerator\"
    :room kitchen
    :description \"A white refrigerator.\"
    :verbs '((open . fridge-open-handler)
             (look . fridge-look-handler)))"
  (declare (indent defun))
  `(scumm-define-object ',name (list ,@props)))

;;; ========== ACTOR DEFINITION MACRO ==========

(defmacro defactor (name &rest props)
  "Define an actor NAME with PROPS.
Usage:
  (defactor pirate
    :name \"Pirate\"
    :room tavern
    :dialog pirate-dialog)"
  (declare (indent defun))
  `(scumm-define-actor ',name (list ,@props)))

;;; ========== DIALOG DEFINITION MACRO ==========

(defmacro defdialog (name &rest choices)
  "Define a dialog tree NAME with CHOICES.
Usage:
  (defdialog pirate-dialog
    (\"Hello.\" . \"Arr!\")
    (\"Who are you?\" . \"A mighty pirate!\")
    (\"Bye.\" . :end))"
  (declare (indent defun))
  `(let ((dialogs (scumm-get-state :dialogs)))
     (puthash ',name ',choices dialogs)))

;;; ========== VERB HANDLER HELPERS ==========

(defun scumm-verb-handler (object verb handler)
  "Add VERB HANDLER to OBJECT."
  (let* ((objects (scumm-get-state :objects))
         (obj-def (gethash object objects))
         (verbs (plist-get obj-def :verbs)))
    (setf (alist-get verb verbs) handler)
    (plist-put obj-def :verbs verbs)
    (puthash object obj-def objects)))

(provide 'scumm-script)
;;; scumm-script.el ends here

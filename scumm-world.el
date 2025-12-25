;;; scumm-world.el --- World model (rooms, objects, actors) -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Anton Feldmann

;; Author: Anton Feldmann
;; Keywords: games

;;; Commentary:

;; Defines rooms, objects, and actors for the SCUMM engine.

;;; Code:

(require 'scumm-core)

;;; ========== ROOM DEFINITIONS ==========

(defun scumm-define-room (symbol plist)
  "Define a room SYMBOL with properties PLIST.
Properties:
  :name - display name
  :description - room description
  :objects - list of object symbols in this room
  :exits - alist of (direction . room-symbol)
  :on-enter - function to call when entering room"
  (let ((rooms (scumm-get-state :rooms)))
    (puthash symbol plist rooms)))

(defun scumm-get-room (symbol)
  "Get room definition for SYMBOL."
  (gethash symbol (scumm-get-state :rooms)))

(defun scumm-room-objects (room-symbol)
  "Get list of objects in ROOM-SYMBOL."
  (let ((room (scumm-get-room room-symbol)))
    (plist-get room :objects)))

(defun scumm-add-object-to-room (object room)
  "Add OBJECT to ROOM."
  (let* ((rooms (scumm-get-state :rooms))
         (room-def (gethash room rooms))
         (objects (plist-get room-def :objects)))
    (unless (memq object objects)
      (plist-put room-def :objects (cons object objects))
      (puthash room room-def rooms))))

(defun scumm-remove-object-from-room (object room)
  "Remove OBJECT from ROOM."
  (let* ((rooms (scumm-get-state :rooms))
         (room-def (gethash room rooms))
         (objects (plist-get room-def :objects)))
    (plist-put room-def :objects (delq object objects))
    (puthash room room-def rooms)))

;;; ========== OBJECT DEFINITIONS ==========

(defun scumm-define-object (symbol plist)
  "Define an object SYMBOL with properties PLIST.
Properties:
  :name - display name
  :room - initial room (or nil if in inventory)
  :description - examine text
  :verbs - alist of (verb . function)
  :state - custom state plist"
  (let ((objects (scumm-get-state :objects)))
    (puthash symbol plist objects)))

(defun scumm-get-object (symbol)
  "Get object definition for SYMBOL."
  (gethash symbol (scumm-get-state :objects)))

(defun scumm-object-in-room? (object room)
  "Check if OBJECT is in ROOM."
  (let ((obj-def (scumm-get-object object)))
    (eq (plist-get obj-def :room) room)))

(defun scumm-get-object-verb (object verb)
  "Get verb handler for OBJECT and VERB."
  (let* ((obj-def (scumm-get-object object))
         (verbs (plist-get obj-def :verbs)))
    (alist-get verb verbs)))

(defun scumm-set-object-state (object key value)
  "Set object state KEY to VALUE for OBJECT."
  (let* ((objects (scumm-get-state :objects))
         (obj-def (gethash object objects))
         (state (plist-get obj-def :state)))
    (plist-put obj-def :state (plist-put state key value))
    (puthash object obj-def objects)))

(defun scumm-get-object-state (object key)
  "Get object state KEY for OBJECT."
  (let* ((obj-def (scumm-get-object object))
         (state (plist-get obj-def :state)))
    (plist-get state key)))

;;; ========== ACTOR DEFINITIONS ==========

(defun scumm-define-actor (symbol plist)
  "Define an actor SYMBOL with properties PLIST.
Properties:
  :name - display name
  :room - current room
  :dialog - dialog tree symbol"
  (let ((actors (scumm-get-state :actors)))
    (setf (alist-get symbol actors) plist)
    (scumm-set-state :actors actors)))

(defun scumm-get-actor (symbol)
  "Get actor definition for SYMBOL."
  (alist-get symbol (scumm-get-state :actors)))

(defun scumm-actor-in-room? (actor)
  "Check if ACTOR is in current room."
  (let ((actor-def (scumm-get-actor actor)))
    (eq (plist-get actor-def :room) (scumm-get-current-room))))

;;; ========== UTILITIES ==========

(defun scumm-find-objects-in-current-room ()
  "Get all objects in current room."
  (let ((room (scumm-get-current-room))
        (objects (scumm-get-state :objects))
        result)
    (maphash (lambda (sym obj)
               (when (eq (plist-get obj :room) room)
                 (push sym result)))
             objects)
    result))

(defun scumm-find-actors-in-current-room ()
  "Get all actors in current room."
  (let ((room (scumm-get-current-room)))
    (cl-loop for (actor . def) in (scumm-get-state :actors)
             when (eq (plist-get def :room) room)
             collect actor)))

(provide 'scumm-world)
;;; scumm-world.el ends here

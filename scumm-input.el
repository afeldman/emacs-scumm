;;; scumm-input.el --- Verb commands and input handling -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Anton Feldmann

;; Author: Anton Feldmann
;; Keywords: games

;;; Commentary:

;; Implements verb commands (look, open, use, talk, etc.).

;;; Code:

(require 'scumm-core)
(require 'scumm-world)
(require 'scumm-render)

;;; ========== VERB COMMANDS ==========

(defun scumm-cmd-look ()
  "Look around current room."
  (interactive)
  (let ((room (scumm-get-current-room)))
    (scumm-render-room room)))

(defun scumm-cmd-examine ()
  "Examine an object."
  (interactive)
  (let* ((objects (append (scumm-find-objects-in-current-room)
                         (scumm-get-state :inventory)))
         (choices (mapcar (lambda (obj)
                           (cons (or (plist-get (scumm-get-object obj) :name)
                                    (symbol-name obj))
                                 obj))
                         objects))
         (choice (completing-read "Examine: " choices nil t))
         (obj (cdr (assoc choice choices))))
    (if obj
        (let ((desc (plist-get (scumm-get-object obj) :description)))
          (scumm-message (or desc "Nothing special.")))
      (scumm-message "I don't see that."))))

(defun scumm-cmd-open ()
  "Open an object."
  (interactive)
  (scumm-do-verb 'open "Open"))

(defun scumm-cmd-use ()
  "Use an object."
  (interactive)
  (scumm-do-verb 'use "Use"))

(defun scumm-cmd-talk ()
  "Talk to an actor."
  (interactive)
  (let* ((actors (scumm-find-actors-in-current-room))
         (choices (mapcar (lambda (actor)
                           (cons (or (plist-get (scumm-get-actor actor) :name)
                                    (symbol-name actor))
                                 actor))
                         actors))
         (choice (completing-read "Talk to: " choices nil t))
         (actor (cdr (assoc choice choices))))
    (if actor
        (let ((dialog (plist-get (scumm-get-actor actor) :dialog)))
          (if dialog
              (progn
                (require 'scumm-dialog)
                (scumm-start-dialog dialog))
            (scumm-message "They don't want to talk.")))
      (scumm-message "Nobody here."))))

(defun scumm-cmd-inventory ()
  "Show inventory."
  (interactive)
  (let ((inv (scumm-get-state :inventory)))
    (if inv
        (scumm-message
         (format "Inventory: %s"
                 (mapconcat (lambda (obj)
                             (or (plist-get (scumm-get-object obj) :name)
                                 (symbol-name obj)))
                           inv ", ")))
      (scumm-message "Inventory is empty."))))

(defun scumm-cmd-go ()
  "Go to another room."
  (interactive)
  (let* ((room (scumm-get-room (scumm-get-current-room)))
         (exits (plist-get room :exits))
         (choices (mapcar (lambda (exit)
                           (cons (format "%s" (car exit))
                                 (cdr exit)))
                         exits))
         (choice (completing-read "Go: " choices nil t))
         (dest (cdr (assoc choice choices))))
    (if dest
        (scumm-goto-room dest)
      (scumm-message "You can't go that way."))))

;;; ========== VERB HELPER ==========

(defun scumm-do-verb (verb verb-name)
  "Execute VERB on selected object with VERB-NAME prompt."
  (let* ((objects (append (scumm-find-objects-in-current-room)
                         (scumm-get-state :inventory)))
         (choices (mapcar (lambda (obj)
                           (cons (or (plist-get (scumm-get-object obj) :name)
                                    (symbol-name obj))
                                 obj))
                         objects))
         (choice (completing-read (format "%s: " verb-name) choices nil t))
         (obj (cdr (assoc choice choices))))
    (if obj
        (let ((handler (scumm-get-object-verb obj verb)))
          (if handler
              (funcall handler)
            (scumm-message (format "You can't %s that." (downcase verb-name)))))
      (scumm-message "I don't see that."))))

(provide 'scumm-input)
;;; scumm-input.el ends here

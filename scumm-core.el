;;; scumm-core.el --- Core game loop and dispatcher for SCUMM engine -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Anton Feldmann

;; Author: Anton Feldmann
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games

;;; Commentary:

;; Core game state management and event loop for the SCUMM-like
;; adventure game engine.

;;; Code:

(require 'cl-lib)

;;; ========== GAME STATE ==========

(defvar scumm-game-state nil
  "Current game state.
Plist containing:
  :current-room - symbol of current room
  :inventory - list of object symbols
  :flags - alist of game flags
  :actors - alist of actor states
  :rooms - hash table of room definitions
  :objects - hash table of object definitions
  :dialogs - hash table of dialog definitions")

(defun scumm-init-game ()
  "Initialize a new game state."
  (setq scumm-game-state
        (list :current-room nil
              :inventory nil
              :flags nil
              :actors nil
              :rooms (make-hash-table :test 'eq)
              :objects (make-hash-table :test 'eq)
              :dialogs (make-hash-table :test 'eq))))

(defun scumm-get-state (key)
  "Get value from game state by KEY."
  (plist-get scumm-game-state key))

(defun scumm-set-state (key value)
  "Set game state KEY to VALUE."
  (setq scumm-game-state (plist-put scumm-game-state key value)))

(defun scumm-get-flag (flag)
  "Get game flag FLAG."
  (alist-get flag (scumm-get-state :flags)))

(defun scumm-set-flag (flag &optional value)
  "Set game flag FLAG to VALUE (default t)."
  (let ((flags (scumm-get-state :flags)))
    (setf (alist-get flag flags) (or value t))
    (scumm-set-state :flags flags)))

(defun scumm-clear-flag (flag)
  "Clear game flag FLAG."
  (let ((flags (scumm-get-state :flags)))
    (setf (alist-get flag flags nil 'remove) nil)
    (scumm-set-state :flags flags)))

;;; ========== INVENTORY ==========

(defun scumm-add-to-inventory (object)
  "Add OBJECT to player inventory."
  (let ((inv (scumm-get-state :inventory)))
    (unless (memq object inv)
      (scumm-set-state :inventory (cons object inv)))))

(defun scumm-remove-from-inventory (object)
  "Remove OBJECT from player inventory."
  (let ((inv (scumm-get-state :inventory)))
    (scumm-set-state :inventory (delq object inv))))

(defun scumm-in-inventory? (object)
  "Check if OBJECT is in player inventory."
  (memq object (scumm-get-state :inventory)))

;;; ========== ROOM MANAGEMENT ==========

(defun scumm-get-current-room ()
  "Get current room symbol."
  (scumm-get-state :current-room))

(defun scumm-goto-room (room-symbol)
  "Go to ROOM-SYMBOL and trigger on-enter hooks."
  (let* ((rooms (scumm-get-state :rooms))
         (room (gethash room-symbol rooms)))
    (if room
        (progn
          (scumm-set-state :current-room room-symbol)
          (require 'scumm-render)
          (scumm-render-room room-symbol)
          ;; Call on-enter hook if present
          (when-let ((on-enter (plist-get room :on-enter)))
            (funcall on-enter)))
      (error "Room %s not found" room-symbol))))

;;; ========== MESSAGE SYSTEM ==========

(defvar scumm-message-callback nil
  "Callback function for displaying messages.")

(defun scumm-message (text)
  "Display message TEXT to player."
  (if scumm-message-callback
      (funcall scumm-message-callback text)
    (message "%s" text)))

;;; ========== GAME CONTROL ==========

(defvar scumm-running nil
  "Whether a game is currently running.")

(defun scumm-start-game (start-room)
  "Start game at START-ROOM."
  (scumm-init-game)
  (setq scumm-running t)
  (require 'scumm-render)
  (scumm-setup-buffer)
  (scumm-goto-room start-room))

(defun scumm-quit-game ()
  "Quit current game."
  (interactive)
  (when (yes-or-no-p "Really quit game? ")
    (setq scumm-running nil)
    (when (get-buffer "*SCUMM*")
      (kill-buffer "*SCUMM*"))
    (message "Game ended.")))

(provide 'scumm-core)
;;; scumm-core.el ends here

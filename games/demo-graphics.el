;;; demo-graphics.el --- Demo game with optional graphics support -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Anton Feldmann
;; License: GPL-3.0-or-later

;;; Commentary:

;; A demo game that supports optional Monkey Island-style graphics.
;; To use graphics:
;;   (scumm-enable-graphics)
;;   (scumm-set-assets-path "assets/")
;;   (load "games/demo-graphics.el")
;;   (scumm-start-demo-graphics)
;;
;; To play text-only (default):
;;   M-x scumm-start-demo-graphics

;;; Code:

(require 'scumm)

;;; ========== ROOMS ==========

(defroom kitchen
  :name "Kitchen"
  :description "A warm, inviting kitchen with white walls. Dishes are piled
in the sink, and the refrigerator hums quietly in the corner. A doorway
leads to the hallway."
  :objects (fridge sink stove)
  :exits ((north . hallway))
  :on-enter (lambda ()
              (scumm-message "You enter the kitchen. It smells like fresh bread.")))

(defroom hallway
  :name "Hallway"
  :description "A narrow hallway with hardwood floors. Doors lead to the
kitchen to the south and the living room to the north."
  :exits ((north . living-room)
          (south . kitchen))
  :on-enter (lambda ()
              (scumm-message "You're in the hallway.")))

(defroom living-room
  :name "Living Room"
  :description "A comfortable living room with a fireplace. An old leather
couch sits against one wall, and bookshelves line another. A window shows
the street outside."
  :objects (couch bookshelf window)
  :exits ((south . hallway))
  :on-enter (lambda ()
              (scumm-message "A cozy living room awaits you.")))

;;; ========== OBJECTS ==========

(defobject fridge
  :name "refrigerator"
  :room kitchen
  :description "A white refrigerator, humming softly."
  :verbs '((open . fridge-open)
           (use . fridge-open)))

(defobject sink
  :name "sink"
  :room kitchen
  :description "A ceramic sink filled with dirty dishes."
  :verbs '((examine . (lambda () (scumm-message "Pretty grimy.")))))

(defobject stove
  :name "stove"
  :room kitchen
  :description "An old gas stove."
  :verbs '((examine . (lambda () (scumm-message "Looks like it could use cleaning.")))))

(defobject couch
  :name "couch"
  :room living-room
  :description "A worn but comfortable leather couch."
  :verbs '((examine . (lambda () (scumm-message "It's very comfortable.")))))

(defobject bookshelf
  :name "bookshelf"
  :room living-room
  :description "Floor-to-ceiling bookshelves filled with classic novels."
  :verbs '((examine . (lambda () (scumm-message "Lots of interesting books.")))))

(defobject window
  :name "window"
  :room living-room
  :description "A window looking out onto the street."
  :verbs '((look . (lambda () (scumm-message "Through the window you see the busy street.")))
           (examine . (lambda () (scumm-message "A large bay window with a good view.")))))

;;; ========== FUNCTIONS ==========

(defun fridge-open ()
  "Open the refrigerator."
  (scumm-message "You open the refrigerator. It's mostly empty."))

;;;###autoload
(defun scumm-start-demo-graphics ()
  "Start the demo game with optional graphics support."
  (interactive)
  (scumm-init 'kitchen)
  (scumm-message "Welcome to the Demo House!
Type 'l' to look, 'e' to examine, 'o' to open, 'i' for inventory, 'g' to go.
Type 'q' to quit."))

(provide 'demo-graphics)
;;; demo-graphics.el ends here

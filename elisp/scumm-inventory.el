;;; scumm-inventory.el --- Inventory management -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Anton Feldmann
;; License: GPL-3.0-or-later

;; Author: Anton Feldmann
;; Keywords: games

;;; Commentary:

;; Inventory management is handled in scumm-core.el.
;; This file provides additional convenience functions.

;;; Code:

(require 'scumm-core)
(require 'scumm-world)

;;; ========== INVENTORY OPERATIONS ==========

(defun scumm-take-object (object)
  "Take OBJECT from current room and add to inventory."
  (let ((room (scumm-get-current-room)))
    (when (scumm-object-in-room? object room)
      (scumm-remove-object-from-room object room)
      (scumm-add-to-inventory object)
      (scumm-message (format "Taken: %s"
                            (or (plist-get (scumm-get-object object) :name)
                                (symbol-name object)))))))

(defun scumm-drop-object (object)
  "Drop OBJECT from inventory into current room."
  (when (scumm-in-inventory? object)
    (let ((room (scumm-get-current-room)))
      (scumm-remove-from-inventory object)
      (scumm-add-object-to-room object room)
      (scumm-message (format "Dropped: %s"
                            (or (plist-get (scumm-get-object object) :name)
                                (symbol-name object)))))))

(provide 'scumm-inventory)
;;; scumm-inventory.el ends here

;;; scumm-dialog.el --- Dialog system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Anton Feldmann
;; License: GPL-3.0-or-later

;; Author: Anton Feldmann
;; Keywords: games

;;; Commentary:

;; Dialog tree system for talking to actors.

;;; Code:

(require 'scumm-core)

;;; ========== DIALOG SYSTEM ==========

(defun scumm-start-dialog (dialog-symbol)
  "Start dialog DIALOG-SYMBOL."
  (let ((dialogs (scumm-get-state :dialogs))
        (dialog-tree (gethash dialog-symbol (scumm-get-state :dialogs))))
    (if dialog-tree
        (scumm-dialog-loop dialog-tree)
      (scumm-message "No dialog available."))))

(defun scumm-dialog-loop (choices)
  "Run dialog loop with CHOICES."
  (let* ((prompts (mapcar #'car choices))
         (choice (completing-read "Say: " prompts nil t))
         (response-pair (assoc choice choices))
         (response (cdr response-pair)))
    (cond
     ;; End dialog
     ((eq response :end)
      (scumm-message "Goodbye."))
     
     ;; Function response (for state changes)
     ((functionp response)
      (funcall response)
      (scumm-dialog-loop choices))
     
     ;; String response
     ((stringp response)
      (scumm-message response)
      (scumm-dialog-loop choices))
     
     ;; Symbol response (goto another dialog)
     ((symbolp response)
      (scumm-start-dialog response))
     
     ;; Default
     (t
      (scumm-message "...")))))

(provide 'scumm-dialog)
;;; scumm-dialog.el ends here

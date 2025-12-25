;;; scumm-save.el --- Save/Load game state -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Anton Feldmann
;; License: GPL-3.0-or-later

;; Author: Anton Feldmann
;; Keywords: games

;;; Commentary:

;; Save and load game state to/from files.

;;; Code:

(require 'scumm-core)

;;; ========== SAVE/LOAD ==========

(defvar scumm-save-directory
  (expand-file-name "scumm-saves" user-emacs-directory)
  "Directory for SCUMM save files.")

(defun scumm-ensure-save-directory ()
  "Ensure save directory exists."
  (unless (file-exists-p scumm-save-directory)
    (make-directory scumm-save-directory t)))

(defun scumm-save-game (filename)
  "Save game state to FILENAME."
  (interactive
   (list (read-file-name "Save game: "
                        scumm-save-directory
                        nil nil "save.sav")))
  (scumm-ensure-save-directory)
  (with-temp-file filename
    (prin1 scumm-game-state (current-buffer)))
  (scumm-message (format "Game saved to %s" filename)))

(defun scumm-load-game (filename)
  "Load game state from FILENAME."
  (interactive
   (list (read-file-name "Load game: "
                        scumm-save-directory
                        nil t)))
  (if (file-exists-p filename)
      (progn
        (with-temp-buffer
          (insert-file-contents filename)
          (setq scumm-game-state (read (current-buffer))))
        (scumm-message "Game loaded.")
        (require 'scumm-render)
        (scumm-render-room (scumm-get-current-room)))
    (scumm-message "Save file not found.")))

(provide 'scumm-save)
;;; scumm-save.el ends here

;;; scumm.el --- SCUMM-like adventure game engine for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Anton Feldmann

;; Author: Anton Feldmann <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games
;; URL: https://github.com/afeldman/emacs-scumm
;; License: GPL-3.0-or-later

;;; Commentary:

;; A SCUMM-like adventure game engine for Emacs.
;; 
;; Features:
;; - Script-driven game definition
;; - Verb-based interaction (look, open, use, talk, etc.)
;; - Dialog system
;; - Inventory management
;; - Save/Load support
;; - Extensible for multiple games
;;
;; Usage:
;;   (require 'scumm)
;;   (load "path/to/game.el")
;;   (scumm-start-game 'starting-room)

;;; Code:

;; Load modules from elisp directory
(let ((scumm-dir (expand-file-name "elisp"
                                   (file-name-directory
                                    (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path scumm-dir))

(require 'scumm-core)
(require 'scumm-world)
(require 'scumm-script)
(require 'scumm-render)
(require 'scumm-input)
(require 'scumm-dialog)
(require 'scumm-inventory)
(require 'scumm-save)

;;; ========== PUBLIC API ==========

;;;###autoload
(defun scumm-start-demo ()
  "Start the demo game."
  (interactive)
  (load (expand-file-name "games/demo.el"
                         (file-name-directory
                          (or load-file-name buffer-file-name))))
  (scumm-start-game 'kitchen))

;;;###autoload
(defun scumm-start-game (room-name)
  "Start a game in the given ROOM-NAME."
  (scumm-init room-name))

(provide 'scumm)
;;; scumm.el ends here

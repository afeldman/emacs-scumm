;;; scumm-render.el --- Buffer rendering and UI -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Anton Feldmann
;; License: GPL-3.0-or-later

;; Author: Anton Feldmann
;; Keywords: games

;;; Commentary:

;; Renders game state to the *SCUMM* buffer.

;;; Code:

(require 'scumm-core)
(require 'scumm-world)

;;; ========== BUFFER MANAGEMENT ==========

(defvar scumm-buffer-name "*SCUMM*"
  "Name of the game buffer.")

(defun scumm-setup-buffer ()
  "Create and setup the game buffer."
  (let ((buf (get-buffer-create scumm-buffer-name)))
    (with-current-buffer buf
      (scumm-mode)
      (setq buffer-read-only t))
    (switch-to-buffer buf)))

(defun scumm-clear-buffer ()
  "Clear the game buffer."
  (with-current-buffer scumm-buffer-name
    (let ((inhibit-read-only t))
      (erase-buffer))))

;;; ========== RENDERING ==========

(defface scumm-room-title
  '((t :inherit font-lock-keyword-face :weight bold :height 1.3))
  "Face for room titles.")

(defface scumm-room-desc
  '((t :inherit default))
  "Face for room descriptions.")

(defface scumm-object-face
  '((t :inherit font-lock-variable-name-face))
  "Face for objects.")

(defface scumm-actor-face
  '((t :inherit font-lock-type-face))
  "Face for actors.")

(defface scumm-exit-face
  '((t :inherit font-lock-constant-face))
  "Face for exits.")

(defun scumm-insert (text &optional face)
  "Insert TEXT with optional FACE in game buffer."
  (with-current-buffer scumm-buffer-name
    (let ((inhibit-read-only t))
      (if face
          (insert (propertize text 'face face))
        (insert text)))))

(defun scumm-render-room (room-symbol)
  "Render ROOM-SYMBOL to buffer."
  (scumm-clear-buffer)
  (let* ((room (scumm-get-room room-symbol))
         (name (or (plist-get room :name) (symbol-name room-symbol)))
         (desc (plist-get room :description))
         (exits (plist-get room :exits))
         (objects (scumm-find-objects-in-current-room))
         (actors (scumm-find-actors-in-current-room)))
    
    ;; Room name
    (scumm-insert (format "%s\n" name) 'scumm-room-title)
    (scumm-insert (make-string (length name) ?=) 'scumm-room-title)
    (scumm-insert "\n\n")
    
    ;; Description
    (scumm-insert desc 'scumm-room-desc)
    (scumm-insert "\n\n")
    
    ;; Objects
    (when objects
      (scumm-insert "You see: ")
      (scumm-insert (mapconcat (lambda (obj)
                                 (let ((obj-def (scumm-get-object obj)))
                                   (or (plist-get obj-def :name)
                                       (symbol-name obj))))
                               objects ", ")
                    'scumm-object-face)
      (scumm-insert "\n"))
    
    ;; Actors
    (when actors
      (scumm-insert "Actors: ")
      (scumm-insert (mapconcat (lambda (actor)
                                 (let ((actor-def (scumm-get-actor actor)))
                                   (or (plist-get actor-def :name)
                                       (symbol-name actor))))
                               actors ", ")
                    'scumm-actor-face)
      (scumm-insert "\n"))
    
    ;; Exits
    (when exits
      (scumm-insert "\nExits: ")
      (scumm-insert (mapconcat (lambda (exit)
                                 (format "%s" (car exit)))
                               exits ", ")
                    'scumm-exit-face)
      (scumm-insert "\n"))
    
    (scumm-insert "\n")
    (scumm-insert "───────────────────────────────────────\n")
    (scumm-insert "Commands: look, examine, open, use, talk, inventory, quit\n")))

(defun scumm-render-message (text)
  "Render message TEXT in buffer."
  (scumm-insert (format "\n> %s\n" text)))

;; Set message callback
(setq scumm-message-callback #'scumm-render-message)

;;; ========== MODE DEFINITION ==========

(defvar scumm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'scumm-quit-game)
    (define-key map (kbd "l") #'scumm-cmd-look)
    (define-key map (kbd "i") #'scumm-cmd-inventory)
    (define-key map (kbd "e") #'scumm-cmd-examine)
    (define-key map (kbd "o") #'scumm-cmd-open)
    (define-key map (kbd "u") #'scumm-cmd-use)
    (define-key map (kbd "t") #'scumm-cmd-talk)
    (define-key map (kbd "g") #'scumm-cmd-go)
    map)
  "Keymap for SCUMM game mode.")

(define-derived-mode scumm-mode special-mode "SCUMM"
  "Major mode for playing SCUMM adventure games."
  :group 'scumm
  (setq buffer-read-only t))

(provide 'scumm-render)
;;; scumm-render.el ends here

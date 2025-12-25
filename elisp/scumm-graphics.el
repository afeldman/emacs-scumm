;;; scumm-graphics.el --- Optional graphics support for SCUMM -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Anton Feldmann
;; License: GPL-3.0-or-later

;;; Commentary:

;; Optional graphics support for SCUMM engine.
;; Enables split-screen layout with scene images (PNG/JPG) on the left
;; and game text/dialogs on the right, Monkey Island style.
;;
;; Usage:
;;   (scumm-enable-graphics)
;;   (scumm-set-scene-image 'kitchen "assets/kitchen.png")
;;   (scumm-set-assets-path "path/to/assets")

;;; Code:

;; ========== Configuration ==========

(defvar scumm-graphics-enabled nil
  "Whether graphics mode is enabled.")

(defvar scumm-assets-path nil
  "Path to assets directory containing images.")

(defvar scumm-scene-images (make-hash-table :test 'eq)
  "Hash table mapping room names to image file paths.")

(defvar scumm-graphics-window-split 0.4
  "Horizontal split ratio for graphics (0.0-1.0). Default 40% left for image.")

(defvar scumm-graphics-image-width 320
  "Width for displayed images in characters.")

(defvar scumm-graphics-image-height 200
  "Height for displayed images in lines.")

;; ========== Public API ==========

;;;###autoload
(defun scumm-enable-graphics ()
  "Enable graphics mode (split-screen with images)."
  (interactive)
  (setq scumm-graphics-enabled t))

;;;###autoload
(defun scumm-disable-graphics ()
  "Disable graphics mode, use text-only rendering."
  (interactive)
  (setq scumm-graphics-enabled nil))

;;;###autoload
(defun scumm-set-assets-path (path)
  "Set the path to the assets directory containing images.
PATH can be absolute or relative to the game directory."
  (setq scumm-assets-path (expand-file-name path)))

;;;###autoload
(defun scumm-set-scene-image (room-name image-path)
  "Associate an image with a room.
ROOM-NAME is the symbol name of the room.
IMAGE-PATH is relative to assets directory or absolute."
  (let ((full-path
         (if (file-name-absolute-p image-path)
             image-path
           (expand-file-name image-path scumm-assets-path))))
    (puthash room-name full-path scumm-scene-images)))

;;;###autoload
(defun scumm-get-scene-image (room-name)
  "Get the image path for a room, or nil if not set."
  (gethash room-name scumm-scene-images))

;;;###autoload
(defun scumm-set-graphics-split (ratio)
  "Set horizontal split ratio (0.0-1.0).
RATIO 0.4 means 40% left (images), 60% right (text)."
  (when (and (>= ratio 0.0) (<= ratio 1.0))
    (setq scumm-graphics-window-split ratio)))

;; ========== Layout Management ==========

(defun scumm-setup-graphics-layout ()
  "Setup split-screen layout for graphics mode.
Creates left window for images, right window for text."
  (when scumm-graphics-enabled
    (delete-other-windows)
    (let ((total-width (window-width)))
      (split-window-horizontally
       (truncate (* total-width scumm-graphics-window-split))))))

(defun scumm-render-scene-image (room-name)
  "Render the scene image for ROOM-NAME in the left window.
If image not found or graphics disabled, returns nil."
  (when scumm-graphics-enabled
    (let ((image-path (scumm-get-scene-image room-name)))
      (if (and image-path (file-exists-p image-path))
          (scumm--insert-image image-path)
        nil))))

(defun scumm--insert-image (image-path)
  "Insert image from IMAGE-PATH into current buffer.
Scales image to scumm-graphics-image-width/height."
  (condition-case err
      (let ((image (create-image image-path nil nil
                                 :width scumm-graphics-image-width
                                 :height scumm-graphics-image-height)))
        (insert-image image "")
        t)
    (error
     (message "Failed to load image: %s" image-path)
     nil)))

;; ========== Window Management ==========

(defun scumm-get-text-window ()
  "Get the text window (right side in graphics mode)."
  (if scumm-graphics-enabled
      (let ((windows (window-list)))
        (if (> (length windows) 1)
            (nth 1 windows)  ; Second window (right side)
          (selected-window)))
    (selected-window)))

(defun scumm-get-graphics-window ()
  "Get the graphics window (left side in graphics mode)."
  (if scumm-graphics-enabled
      (let ((windows (window-list)))
        (if (> (length windows) 1)
            (nth 0 windows)  ; First window (left side)
          nil))
    nil))

;; ========== Integration Hooks ==========

(defun scumm-graphics-setup-for-room (room-name)
  "Setup graphics for entering a room.
Call this when transitioning to a new room."
  (when scumm-graphics-enabled
    (let ((graphics-window (scumm-get-graphics-window)))
      (when graphics-window
        (with-selected-window graphics-window
          (erase-buffer)
          (scumm-render-scene-image room-name))))))

;; ========== Utility Functions ==========

(defun scumm-clear-scene-images ()
  "Clear all cached scene image associations."
  (clrhash scumm-scene-images))

(defun scumm-list-scene-images ()
  "Return list of (room-name . image-path) for all configured scenes."
  (let (result)
    (maphash (lambda (k v) (push (cons k v) result)) scumm-scene-images)
    result))

(provide 'scumm-graphics)
;;; scumm-graphics.el ends here

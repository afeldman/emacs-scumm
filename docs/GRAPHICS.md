# Graphics & Split-Screen Guide

Optional Monkey Island-style graphics support for SCUMM games.

## Overview

SCUMM supports optional graphics rendering:
- **Left panel**: Scene images (PNG/JPG)
- **Right panel**: Game text, dialogs, inventory
- **Bottom**: Inventory/stats (future)

Graphics are **completely optional** - games work fine with text-only rendering.

## Quick Start

### 1. Enable Graphics

```elisp
(scumm-enable-graphics)
```

### 2. Set Assets Path

```elisp
(scumm-set-assets-path "assets/")    ; relative to game directory
(scumm-set-assets-path "/absolute/path/assets")  ; absolute path
```

### 3. Register Room Images

```elisp
(scumm-set-scene-image 'kitchen "kitchen.png")
(scumm-set-scene-image 'hallway "hallway.png")
(scumm-set-scene-image 'living-room "living-room.png")
```

### 4. Start Game

```elisp
(scumm-start-demo-graphics)
```

## Complete Example

```elisp
(require 'scumm)

;; Enable graphics mode
(scumm-enable-graphics)

;; Set base path for images
(scumm-set-assets-path "~/games/my-adventure/assets")

;; Configure room images
(scumm-set-scene-image 'kitchen "kitchen.png")
(scumm-set-scene-image 'hallway "hallway.png")
(scumm-set-scene-image 'garden "garden.png")

;; Define rooms with graphics
(defroom kitchen
  :name "Kitchen"
  :description "A cozy kitchen with warm lighting."
  :objects (fridge table)
  :exits ((north . hallway)))

;; Start game
(scumm-start-game 'kitchen)
```

## API Reference

### Enabling/Disabling Graphics

```elisp
(scumm-enable-graphics)      ; Enable split-screen mode
(scumm-disable-graphics)     ; Disable, use text-only
```

### Assets Management

```elisp
;; Set base directory for all images
(scumm-set-assets-path "/path/to/assets")

;; Register image for a room
(scumm-set-scene-image 'room-name "image.png")

;; Get image path for a room
(scumm-get-scene-image 'room-name)  ; Returns path or nil

;; Clear all image associations
(scumm-clear-scene-images)

;; List all registered images
(scumm-list-scene-images)  ; Returns '((kitchen . "...") ...)
```

### Layout Configuration

```elisp
;; Set split ratio (0.0-1.0)
;; 0.4 = 40% image (left), 60% text (right)
(scumm-set-graphics-split 0.4)

;; Set image display size
(setq scumm-graphics-image-width 320)   ; pixels
(setq scumm-graphics-image-height 200)  ; pixels
```

## File Organization

```
my-adventure/
├── game.el              # Main game code
├── assets/
│   ├── kitchen.png
│   ├── hallway.png
│   ├── garden.png
│   └── ...
└── characters/
    └── ...
```

## Image Format

- **Supported formats**: PNG, JPG, GIF (image-type support)
- **Recommended size**: 320x200 to 640x480
- **Aspect ratio**: Any (will be scaled to fit)

## Behavior

### With Graphics Enabled

1. When entering a room, the left window shows the scene image
2. Right window shows room description, objects, and dialogs
3. If image not found, left window shows nothing (graceful fallback)
4. Text-only games still work - just don't set any images

### Without Graphics (Default)

- Standard full-width text rendering
- No special setup needed
- Fully compatible with games that don't use graphics

## Best Practices

### 1. Image Sizes
Keep consistent aspect ratios across all images:
```elisp
(setq scumm-graphics-image-width 320)
(setq scumm-graphics-image-height 240)
```

### 2. Fallback Gracefully
If image is missing, game still plays:
```elisp
(scumm-set-scene-image 'kitchen "kitchen.png")
;; If kitchen.png doesn't exist, nothing appears but game continues
```

### 3. Optional for Players
Let players toggle graphics:
```elisp
(defun toggle-graphics ()
  (interactive)
  (if (boundp 'scumm-graphics-enabled)
      (if scumm-graphics-enabled
          (scumm-disable-graphics)
        (scumm-enable-graphics))
    (scumm-enable-graphics)))
```

### 4. Performance
- Images are loaded on-demand when entering a room
- No memory overhead if graphics disabled
- Works on slow connections (local file load)

## Common Issues

### Image Not Showing

1. Check path is correct:
   ```elisp
   (scumm-set-assets-path "/full/path/to/assets")
   ```

2. Verify file exists:
   ```bash
   ls -la /full/path/to/assets/kitchen.png
   ```

3. Check graphics are enabled:
   ```elisp
   scumm-graphics-enabled  ; Should be t
   ```

### Text Overlapping Image

Adjust split ratio:
```elisp
(scumm-set-graphics-split 0.35)  ; Less space for images
```

### Image Too Small/Large

Adjust image dimensions:
```elisp
(setq scumm-graphics-image-width 256)    ; Smaller
(setq scumm-graphics-image-height 192)
```

## Future Enhancements

- [ ] Character portraits/actors as overlays
- [ ] Inventory display with item graphics
- [ ] Cursor graphics (pointer styles)
- [ ] Background music/sound
- [ ] Animated sequences
- [ ] Multiple viewport support

## See Also

- [API.md](API.md) - Complete API reference
- [GAME_DEVELOPMENT_GUIDE.md](GAME_DEVELOPMENT_GUIDE.md) - Game creation
- [README.md](../README.md) - Project overview

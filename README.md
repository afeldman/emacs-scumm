# Emacs SCUMM

A SCUMM-like adventure game engine for Emacs, written entirely in Emacs Lisp.

## Features

- 🎮 **Script-driven game definition** - Write games in pure Emacs Lisp using intuitive macros
- 🗣️ **Verb-based interaction** - Classic point-and-click verbs: look, open, use, talk, etc.
- 💬 **Dialog system** - Interactive conversations with NPCs
- 🎒 **Inventory management** - Pick up and use items
- 💾 **Save/Load** - Persistent game state
- 🎨 **Text-based rendering** - Beautiful terminal-style interface with faces
- 🔧 **Extensible** - Easy to create new games

## Installation

### Using straight.el

```elisp
(use-package scumm
  :straight (scumm :type git :host github :repo "afeldman/emacs-scumm"))
```

### Manual Installation

```bash
git clone https://github.com/afeldman/emacs-scumm.git
```

Then add to your `init.el`:

```elisp
(add-to-list 'load-path "/path/to/emacs-scumm")
(require 'scumm)
```

## Quick Start

Play the demo game:

```elisp
M-x scumm-start-demo
```

## Controls

| Key | Action |
|-----|--------|
| `l` | Look around |
| `e` | Examine object |
| `o` | Open object |
| `u` | Use object |
| `t` | Talk to actor |
| `i` | Show inventory |
| `g` | Go to another room |
| `q` | Quit game |

## Creating Your Own Game

### Define Rooms

```elisp
(defroom kitchen
  :name "Kitchen"
  :description "A messy kitchen with dirty dishes everywhere."
  :objects (fridge table)
  :exits ((north . hallway))
  :on-enter (lambda ()
              (scumm-message "You smell something funny...")))
```

### Define Objects

```elisp
(defobject fridge
  :name "refrigerator"
  :room kitchen
  :description "A white refrigerator."
  :verbs '((open . fridge-open)
           (use . fridge-use)))

(defun fridge-open ()
  "Open the fridge."
  (scumm-add-to-inventory 'milk)
  (scumm-message "You found milk!"))
```

### Define Actors & Dialogs

```elisp
(defactor pirate
  :name "Mysterious Pirate"
  :room tavern
  :dialog pirate-dialog)

(defdialog pirate-dialog
  ("Hello." . "Arr!")
  ("Who are you?" . "A mighty pirate!")
  ("Bye." . :end))
```

### Start Your Game

```elisp
(scumm-start-game 'kitchen)  ; Start at kitchen room
```

## Architecture

```
scumm-core.el       - Game state, event loop
scumm-world.el      - Rooms, objects, actors
scumm-script.el     - DSL macros (defroom, defobject, etc.)
scumm-render.el     - Buffer rendering, UI
scumm-input.el      - Verb commands
scumm-dialog.el     - Dialog trees
scumm-inventory.el  - Inventory management
scumm-save.el       - Save/load functionality
```

## Demo Game Walkthrough

1. Start game with `M-x scumm-start-demo`
2. Press `o` and select "refrigerator" to get milk
3. Press `g` and go `north` to hallway
4. Press `t` to talk to pirate
5. Ask about treasure
6. Press `u` and use the milk (give to pirate)
7. Go `north` to bedroom
8. Press `o` and open treasure chest
9. You win! 🎉

## API Reference

### Game Control

- `(scumm-start-game room)` - Start game at ROOM
- `(scumm-quit-game)` - Quit current game
- `(scumm-save-game filename)` - Save game state
- `(scumm-load-game filename)` - Load game state

### World Building

- `(defroom name ...)` - Define a room
- `(defobject name ...)` - Define an object
- `(defactor name ...)` - Define an actor
- `(defdialog name ...)` - Define a dialog tree

### Game State

- `(scumm-get-flag flag)` - Get game flag
- `(scumm-set-flag flag value)` - Set game flag
- `(scumm-add-to-inventory obj)` - Add object to inventory
- `(scumm-remove-from-inventory obj)` - Remove from inventory
- `(scumm-goto-room room)` - Move player to room
- `(scumm-message text)` - Display message to player

## Example Games

Check the `games/` directory for example games:

- `demo.el` - Simple demo showcasing all features

## Contributing

Contributions welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Write tests if applicable
4. Submit a pull request

## License

Copyright (C) 2025 Anton Feldmann

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

## Roadmap

- [ ] Image support (insert-image for rooms/objects)
- [ ] Sound effects
- [ ] Multiple inventory pages
- [ ] Conditional dialog branches
- [ ] Puzzle dependency system
- [ ] Multiple language support
- [ ] More example games

## Credits

Inspired by the classic SCUMM engine (Script Creation Utility for Maniac Mansion) from LucasArts.

## See Also

- [zork-emacs](https://github.com/afeldman/zork-emacs) - Zork I implementation in Emacs
- [emacs-zmachine](https://github.com/afeldman/emacs-zmachine) - ZIL game engine for Emacs

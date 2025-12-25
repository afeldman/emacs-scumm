# SCUMM Architecture

Technical architecture and design decisions for the Emacs SCUMM engine.

## Overview

The SCUMM engine is a modular Emacs Lisp framework for building text-based adventure games. It separates concerns into independent modules with clear interfaces.

## Module Architecture

### Layer Diagram

```
┌─────────────────────────────────────────────────┐
│          Game Scripts (demo.el, etc.)          │
├─────────────────────────────────────────────────┤
│  Script DSL (scumm-script.el)                  │
│  Macros: defroom, defobject, defactor, defdialog
├─────────────────────────────────────────────────┤
│  High-Level APIs                               │
│  ├─ scumm-input.el   (Verb Commands)          │
│  ├─ scumm-inventory.el (Inventory)            │
│  ├─ scumm-dialog.el   (Conversations)         │
│  └─ scumm-save.el     (Persistence)           │
├─────────────────────────────────────────────────┤
│  Core Systems                                   │
│  ├─ scumm-core.el   (State, Control)          │
│  ├─ scumm-world.el  (Rooms, Objects, Actors)  │
│  └─ scumm-render.el (UI, Buffer)              │
├─────────────────────────────────────────────────┤
│  Emacs Core: Buffers, Modes, I/O              │
└─────────────────────────────────────────────────┘
```

## Core Modules

### 1. scumm-core.el - Game State & Control

**Responsibility:** Central game state management and high-level control flow

**Key Concepts:**
- **Game State Plist**: Central source of truth
  ```
  (:current-room 'kitchen
   :inventory (key milk)
   :flags ((talked-to-npc . t) ...)
   :rooms #<hash-table>
   :objects #<hash-table>
   :actors ((pirate . (:name "Captain" :room tavern ...)) ...)
   :dialogs #<hash-table>)
  ```

**Key Functions:**
- `scumm-init-game()` - Initialize empty state
- `scumm-start-game(room)` - Start game at room
- `scumm-quit-game()` - Cleanup and exit
- `scumm-get-state(key)` / `scumm-set-state(key value)` - State access
- `scumm-message(text)` - Display text to player

**Design Decisions:**
- Uses plist for state (readable, serializable)
- Uses hash tables for room/object lookup (O(1) access)
- Uses alist for flags (simple key-value pairs)
- Callback-based message system (allows swapping renderers)

### 2. scumm-world.el - Game World Model

**Responsibility:** Define and manage rooms, objects, and actors

**Data Model:**

**Room Definition (plist):**
```elisp
(:name "Kitchen"
 :description "A messy kitchen..."
 :objects (fridge table)
 :exits ((north . hallway) (south . pantry))
 :on-enter <function>)
```

**Object Definition (plist):**
```elisp
(:name "refrigerator"
 :room kitchen
 :description "A white fridge..."
 :verbs ((open . handler-fn) (use . handler-fn))
 :state (:open nil :contains (milk cheese)))
```

**Actor Definition (plist):**
```elisp
(:name "Pirate"
 :room tavern
 :dialog pirate-dialog)
```

**Key Functions:**
- `scumm-define-room(symbol plist)` - Define room
- `scumm-define-object(symbol plist)` - Define object
- `scumm-define-actor(symbol plist)` - Define actor
- `scumm-get-room(symbol)` - Retrieve room def
- `scumm-get-object(symbol)` - Retrieve object def
- `scumm-find-*-in-current-room()` - Query functions

**Design Decisions:**
- Hash tables for rooms/objects (efficient lookup)
- Alist for actors (usually few actors)
- Objects can be in rooms OR inventory (room = nil)
- State stored within object definition (mutable)
- Verb handlers are closures (access game state)

### 3. scumm-render.el - User Interface

**Responsibility:** Display game state in Emacs buffer

**Core Concepts:**
- **Game Buffer**: `*SCUMM*` - Single persistent buffer for game display
- **Faces**: Custom faces for styling different elements
- **Read-only Buffer**: Prevents accidental edits

**Rendering Cycle:**
```
Room Updated
    ↓
scumm-clear-buffer() - Erase old content
    ↓
scumm-render-room() - Insert room data
    ↓
Display updated buffer
```

**Rendered Elements:**
```
Title (scumm-room-title face)
Description (scumm-room-desc face)
Objects (scumm-object-face)
Actors (scumm-actor-face)
Exits (scumm-exit-face)
Command hints
```

**Design Decisions:**
- Single buffer (cleaner UX)
- Text properties + faces (no overlays needed)
- Read-only mode (prevent accidental interaction)
- Keybindings via derived mode

### 4. scumm-input.el - Command Handling

**Responsibility:** Process player commands and dispatch verbs

**Command Flow:**
```
Player Input (e.g., "examine fridge")
    ↓
scumm-cmd-examine() triggered
    ↓
completing-read() - Choose object
    ↓
scumm-get-object-verb() - Find handler
    ↓
Handler function called
    ↓
scumm-message() - Display result
```

**Standard Verbs:**
- `look` - Display current room
- `examine` - Describe object
- `open` - Open object
- `use` - Use object
- `talk` - Converse with actor
- `inventory` - Show items
- `go` - Travel to room

**Design Decisions:**
- `completing-read` for object selection (user-friendly)
- Verb handlers are per-object (flexibility)
- Missing verbs gracefully degrade (message: "can't do that")

### 5. scumm-dialog.el - Conversation System

**Responsibility:** Present and manage dialog trees

**Dialog Structure:**
```elisp
(defdialog npc-dialog
  ("Hello." . "Greetings!")           ; String response
  ("Who are you?" . npc-intro)        ; Function response
  ("Join me?" . join-branch-dialog)   ; Another dialog
  ("Bye." . :end))                    ; End marker
```

**Dialog Loop:**
```
Display dialog options
    ↓
Player chooses option
    ↓
Execute response (string/function/goto-dialog)
    ↓
Loop back to dialog or end
```

**Design Decisions:**
- Simple list-based format (easy to understand)
- Multiple response types (flexible)
- Functions for state changes
- Symbols for branching to other dialogs

### 6. scumm-script.el - Scripting DSL

**Responsibility:** Convenient syntax for game developers

**Macros:**
- `(defroom name :props ...)` → `scumm-define-room`
- `(defobject name :props ...)` → `scumm-define-object`
- `(defactor name :props ...)` → `scumm-define-actor`
- `(defdialog name ...)` → Dialog tree storage

**Design Decision:** Macros expand to direct function calls (no runtime overhead)

### 7. scumm-inventory.el - Item Management

**Responsibility:** Inventory operations

**Functions:**
- `scumm-add-to-inventory(obj)`
- `scumm-remove-from-inventory(obj)`
- `scumm-in-inventory?(obj)`
- `scumm-take-object(obj)` - High-level
- `scumm-drop-object(obj)` - High-level

**Design Decision:** Inventory stored as simple list in game state

### 8. scumm-save.el - Persistence

**Responsibility:** Save and load game state

**Mechanism:**
```
Game State (plist)
    ↓
prin1 → Serializable Lisp
    ↓
Write to file
    ↓
[Load]
    ↓
read → Reconstruct state
    ↓
Restore game
```

**Design Decisions:**
- Uses `prin1` / `read` (simple, works for plists)
- Save directory: `~/.emacs.d/scumm-saves/`
- File format: `.sav` (arbitrary)

## State Management Strategy

### The Central State Plist

```elisp
scumm-game-state
├── :current-room (symbol)         ; Current location
├── :inventory (list)              ; Player items
├── :flags (alist)                 ; Game flags/variables
├── :rooms (hash-table)            ; Room definitions
├── :objects (hash-table)          ; Object definitions
├── :actors (alist)                ; Actor definitions
└── :dialogs (hash-table)          ; Dialog trees
```

### Mutability Pattern

**Mutable by Design:**
- Object state: `:state (:open nil :visited t)`
- Room objects list
- Inventory list
- Flags

**Immutable:**
- Room definitions (edited via `plist-put`)
- Object verb handlers (added via `scumm-verb-handler`)

### State Change Tracking

State changes are implicit (no event system):
```elisp
;; Change state
(scumm-set-flag 'door-open t)
(scumm-add-to-inventory 'key)

;; No need to notify observers
;; Callers check state explicitly
(if (scumm-get-flag 'door-open) ...)
```

## Control Flow

### Game Initialization

```
scumm-start-game('start-room)
    ├─ scumm-init-game()         ; Clear state
    ├─ scumm-setup-buffer()      ; Create UI
    └─ scumm-goto-room('start-room)
        ├─ scumm-render-room()   ; Display
        └─ Call :on-enter hook
```

### Turn Cycle (Player Input)

```
Player presses key
    ↓
scumm-mode keymap
    ↓
Command function (e.g., scumm-cmd-examine)
    ├─ Get user input (completing-read)
    ├─ Find handler
    └─ Call handler
        ├─ Modify game state
        ├─ Trigger side effects
        └─ scumm-message() for feedback
            └─ scumm-render-message() to buffer
```

## Extension Points

### Adding New Verbs

```elisp
(defun scumm-cmd-push ()
  "New verb: push object."
  (interactive)
  (scumm-do-verb 'push "Push"))

;; Add to keymap
(define-key scumm-mode-map (kbd "p") #'scumm-cmd-push)
```

### Custom Rendering

```elisp
(setq scumm-message-callback #'my-custom-renderer)

(defun my-custom-renderer (text)
  ;; Instead of inserting to buffer...
  ;; Could use overlays, images, etc.
  (scumm-insert text))
```

### New Module Integration

```elisp
;; New module: scumm-combat.el
(require 'scumm-core)

(defun scumm-attack (target)
  (let ((damage (random 10)))
    (scumm-message (format "You hit for %d damage!" damage))))

;; Add to game
(define-key scumm-mode-map (kbd "a") 
  (lambda () (interactive) (scumm-attack (completing-read ...))))
```

## Design Principles

### 1. Simplicity Over Features

- Use plain plists, not classes
- Simple completion over complex parsing
- Direct function calls, not event systems

### 2. Debuggability

- Game state inspectable with `scumm-game-state`
- No hidden state
- Clear function names

### 3. Extensibility Without Modification

- Verb handlers can be added at runtime
- New modules can inject keybindings
- Callbacks for customization

### 4. Idiomatic Emacs Lisp

- Use standard functions (alist, hash-table, plist)
- Macros for convenience, not abstraction
- Leverage Emacs conventions

## Performance Considerations

### Current Complexity

- Room lookup: O(1) (hash table)
- Object lookup: O(1) (hash table)
- Actor lookup: O(n) (alist, typically ~5 actors)
- Verb lookup: O(n) (alist, typically ~5 verbs/object)

### Scalability

**Current design handles:**
- ~100+ rooms
- ~500+ objects
- ~50+ actors
- Thousands of flag changes

**Optimizations if needed:**
- Cache frequently accessed objects
- Lazy-load rooms
- Use `plist-member` for faster flag access

## Serialization Format

### Save File Example

```elisp
;; Save file contains a single Lisp sexp
(:current-room kitchen
 :inventory (key milk)
 :flags ((entered-cave . t) (talked-to-npc . t))
 :rooms #<hash-table ... >
 ...)
```

### Limitations

- Hash tables serialize as `#<hash-table ...>`
- Functions cannot be saved (verb handlers are not serialized)

**Workaround:** Re-load game code before loading save file

## Testing Strategy

### Unit Testing Approach

```elisp
(defun test-room-transitions ()
  (scumm-init-game)
  (scumm-start-game 'start-room)
  (scumm-goto-room 'kitchen)
  (assert (eq (scumm-get-current-room) 'kitchen)))
```

### Integration Testing

Test complete game flows:
- Start → Collect items → Solve puzzle → End

## Security Considerations

**Not applicable** - Emacs SCUMM is for local single-player games.

However, be aware:
- Save files are plain Lisp (readable)
- State is unencrypted
- No permission system

## Future Enhancements

### Planned

- [ ] Timer system for timed events
- [ ] Combat/damage system
- [ ] More sophisticated dialog branching
- [ ] Image support

### Possible

- [ ] Network multiplayer
- [ ] Plugin system
- [ ] Graphical rendering
- [ ] Voice support

<!-- API.md: Vollständige API-Dokumentation -->

# SCUMM Engine API Reference

Vollständige Dokumentation aller Funktionen und Makros der SCUMM-Engine.

## Table of Contents

1. [Core API](#core-api)
2. [World Building API](#world-building-api)
3. [Input & Verbs API](#input--verbs-api)
4. [Dialog API](#dialog-api)
5. [Inventory API](#inventory-api)
6. [Save/Load API](#saveload-api)
7. [Rendering API](#rendering-api)

---

## Core API

### Game State Management

#### `(scumm-init-game)`

Initializes a new game state. Called automatically by `scumm-start-game`.

**Returns:** nil

**Example:**
```elisp
(scumm-init-game)
```

#### `(scumm-start-game room-symbol)`

Starts a new game at the specified room.

**Arguments:**
- `room-symbol` (symbol): Starting room

**Returns:** nil

**Side Effects:** 
- Initializes game state
- Creates game buffer
- Renders starting room
- Sets `scumm-running` to `t`

**Example:**
```elisp
(scumm-start-game 'kitchen)
```

#### `(scumm-quit-game)`

Quit the current game with confirmation.

**Returns:** nil

**Side Effects:** Sets `scumm-running` to nil, kills buffer

**Example:**
```elisp
(scumm-quit-game)  ;; Interactive command
```

### State Accessors

#### `(scumm-get-state key)`

Get a value from the game state plist.

**Arguments:**
- `key` (symbol): State key (`:current-room`, `:inventory`, `:flags`, etc.)

**Returns:** Value associated with key

**Example:**
```elisp
(scumm-get-state :current-room)  ;; => 'kitchen
(scumm-get-state :inventory)     ;; => '(milk key)
```

#### `(scumm-set-state key value)`

Set a game state value.

**Arguments:**
- `key` (symbol): State key
- `value` (any): New value

**Returns:** nil

**Example:**
```elisp
(scumm-set-state :current-room 'hallway)
```

### Flag Management

Flags are simple boolean markers for tracking game state.

#### `(scumm-set-flag flag &optional value)`

Set a game flag.

**Arguments:**
- `flag` (symbol): Flag name
- `value` (any): Value to set (default: `t`)

**Returns:** nil

**Example:**
```elisp
(scumm-set-flag 'talked-to-pirate)
(scumm-set-flag 'milk-taken t)
(scumm-set-flag 'fridge-open 'slightly)
```

#### `(scumm-get-flag flag)`

Get flag value.

**Arguments:**
- `flag` (symbol): Flag name

**Returns:** Flag value or nil if not set

**Example:**
```elisp
(if (scumm-get-flag 'talked-to-pirate)
    (scumm-message "I know about that."))
```

#### `(scumm-clear-flag flag)`

Delete a flag.

**Arguments:**
- `flag` (symbol): Flag name

**Returns:** nil

**Example:**
```elisp
(scumm-clear-flag 'temporary-flag)
```

### Room Navigation

#### `(scumm-get-current-room)`

Get the current room symbol.

**Returns:** Room symbol

**Example:**
```elisp
(eq (scumm-get-current-room) 'kitchen)  ;; => t
```

#### `(scumm-goto-room room-symbol)`

Move player to a different room.

**Arguments:**
- `room-symbol` (symbol): Destination room

**Returns:** nil

**Side Effects:**
- Updates current room
- Renders the new room
- Calls room's `:on-enter` hook if present

**Example:**
```elisp
(scumm-goto-room 'hallway)
```

### Messages

#### `(scumm-message text)`

Display a message to the player.

**Arguments:**
- `text` (string): Message text

**Returns:** nil

**Example:**
```elisp
(scumm-message "You found a key!")
(scumm-message (format "Health: %d" health))
```

---

## World Building API

### Rooms

#### `(scumm-define-room symbol plist)`

Define a room (typically via `defroom` macro).

**Arguments:**
- `symbol` (symbol): Room identifier
- `plist` (plist): Room properties

**Room Properties:**
- `:name` (string) - Display name
- `:description` (string) - Room description
- `:objects` (list of symbols) - Objects in room
- `:exits` (alist) - `((direction . room) ...)`
- `:on-enter` (function) - Hook called on entry

**Returns:** nil

**Example:**
```elisp
(scumm-define-room 'kitchen
  :name "Kitchen"
  :description "A messy kitchen."
  :objects '(fridge table)
  :exits '((north . hallway) (south . pantry))
  :on-enter (lambda () (scumm-message "Welcome!")))
```

#### `(scumm-get-room symbol)`

Get room definition.

**Arguments:**
- `symbol` (symbol): Room identifier

**Returns:** Room plist or nil

**Example:**
```elisp
(scumm-get-room 'kitchen)
```

#### `(scumm-room-objects room-symbol)`

Get list of objects in a room.

**Arguments:**
- `room-symbol` (symbol): Room identifier

**Returns:** List of object symbols

**Example:**
```elisp
(scumm-room-objects 'kitchen)  ;; => (fridge table)
```

### Objects

#### `(scumm-define-object symbol plist)`

Define an object (typically via `defobject` macro).

**Arguments:**
- `symbol` (symbol): Object identifier
- `plist` (plist): Object properties

**Object Properties:**
- `:name` (string) - Display name
- `:room` (symbol) - Current room or nil for inventory
- `:description` (string) - Examine text
- `:verbs` (alist) - `((verb-symbol . handler-function) ...)`
- `:state` (plist) - Custom state dictionary

**Returns:** nil

**Example:**
```elisp
(scumm-define-object 'fridge
  :name "refrigerator"
  :room 'kitchen
  :description "A white refrigerator."
  :verbs '((open . fridge-open-handler)
           (use . fridge-use-handler))
  :state '(:open nil :contents (milk cheese)))
```

#### `(scumm-get-object symbol)`

Get object definition.

**Arguments:**
- `symbol` (symbol): Object identifier

**Returns:** Object plist or nil

**Example:**
```elisp
(scumm-get-object 'fridge)
```

#### `(scumm-in-inventory? object)`

Check if object is in player inventory.

**Arguments:**
- `object` (symbol): Object identifier

**Returns:** t or nil

**Example:**
```elisp
(if (scumm-in-inventory? 'key)
    (scumm-message "You have the key!"))
```

#### `(scumm-object-in-room? object room)`

Check if object is in a specific room.

**Arguments:**
- `object` (symbol): Object identifier
- `room` (symbol): Room identifier

**Returns:** t or nil

**Example:**
```elisp
(scumm-object-in-room? 'fridge 'kitchen)  ;; => t
```

#### `(scumm-get-object-verb object verb)`

Get verb handler for an object.

**Arguments:**
- `object` (symbol): Object identifier
- `verb` (symbol): Verb identifier

**Returns:** Function or nil

**Example:**
```elisp
(scumm-get-object-verb 'fridge 'open)  ;; => #'fridge-open-handler
```

#### `(scumm-set-object-state object key value)`

Set object state.

**Arguments:**
- `object` (symbol): Object identifier
- `key` (symbol): State key
- `value` (any): Value

**Returns:** nil

**Example:**
```elisp
(scumm-set-object-state 'fridge :open t)
```

#### `(scumm-get-object-state object key)`

Get object state.

**Arguments:**
- `object` (symbol): Object identifier
- `key` (symbol): State key

**Returns:** State value or nil

**Example:**
```elisp
(scumm-get-object-state 'fridge :open)  ;; => t
```

### Actors

#### `(scumm-define-actor symbol plist)`

Define an actor/NPC.

**Arguments:**
- `symbol` (symbol): Actor identifier
- `plist` (plist): Actor properties

**Actor Properties:**
- `:name` (string) - Display name
- `:room` (symbol) - Current room
- `:dialog` (symbol) - Dialog tree identifier

**Returns:** nil

**Example:**
```elisp
(scumm-define-actor 'pirate
  :name "Captain Blackbeard"
  :room 'tavern
  :dialog 'pirate-dialog)
```

#### `(scumm-get-actor symbol)`

Get actor definition.

**Arguments:**
- `symbol` (symbol): Actor identifier

**Returns:** Actor plist or nil

**Example:**
```elisp
(scumm-get-actor 'pirate)
```

#### `(scumm-actor-in-room? actor)`

Check if actor is in current room.

**Arguments:**
- `actor` (symbol): Actor identifier

**Returns:** t or nil

**Example:**
```elisp
(when (scumm-actor-in-room? 'pirate)
  (scumm-message "The pirate is here!"))
```

---

## Input & Verbs API

### Verb Commands

Standard commands available to the player.

#### `(scumm-cmd-look)`

Interactive command: Look around current room.

**Binding:** `l` in scumm-mode

**Example:**
```elisp
(scumm-cmd-look)  ;; Re-renders current room
```

#### `(scumm-cmd-examine)`

Interactive command: Examine an object.

**Binding:** `e` in scumm-mode

**Prompts:** User to select object

**Example:**
```elisp
M-x scumm-cmd-examine
;; Prompts: "Examine: [fridge|table|...]"
```

#### `(scumm-cmd-open)`

Interactive command: Open an object.

**Binding:** `o` in scumm-mode

**Example:**
```elisp
M-x scumm-cmd-open
```

#### `(scumm-cmd-use)`

Interactive command: Use an object.

**Binding:** `u` in scumm-mode

**Example:**
```elisp
M-x scumm-cmd-use
```

#### `(scumm-cmd-talk)`

Interactive command: Talk to an actor.

**Binding:** `t` in scumm-mode

**Example:**
```elisp
M-x scumm-cmd-talk
```

#### `(scumm-cmd-inventory)`

Interactive command: Show player inventory.

**Binding:** `i` in scumm-mode

**Example:**
```elisp
M-x scumm-cmd-inventory
;; Output: "Inventory: key, milk carton, gold coin"
```

#### `(scumm-cmd-go)`

Interactive command: Go to another room.

**Binding:** `g` in scumm-mode

**Example:**
```elisp
M-x scumm-cmd-go
;; Prompts: "Go: [north|south|east|...]"
```

### Verb Helper

#### `(scumm-do-verb verb verb-name)`

Execute a verb on a selected object.

**Arguments:**
- `verb` (symbol): Verb identifier
- `verb-name` (string): Prompt text

**Returns:** nil

**Internal Use:** Called by `scumm-cmd-open`, `scumm-cmd-use`, etc.

**Example:**
```elisp
(defun my-custom-verb ()
  (scumm-do-verb 'kick "Kick"))
```

---

## Dialog API

#### `(defdialog name &rest choices)`

Define a dialog tree (macro).

**Arguments:**
- `name` (symbol): Dialog identifier
- `choices` (list): List of `(prompt . response)` pairs

**Response Types:**
- String: Display as NPC response, loop dialog
- `:end`: End dialog
- Symbol: Goto another dialog
- Function: Call function, loop dialog

**Returns:** nil

**Example:**
```elisp
(defdialog pirate-dialog
  ("Hello." . "Arr!")
  ("Who are you?" . "I be a pirate!")
  ("Do you have treasure?" . pirate-treasure-response)
  ("Bye." . :end))

(defun pirate-treasure-response ()
  (scumm-message "Aye, I do! But it be locked away...")
  ;; Dialog continues
  )
```

#### `(scumm-start-dialog dialog-symbol)`

Start a dialog conversation.

**Arguments:**
- `dialog-symbol` (symbol): Dialog identifier

**Returns:** nil

**Example:**
```elisp
(scumm-start-dialog 'pirate-dialog)
```

#### `(scumm-dialog-loop choices)`

Internal: Loop through dialog choices.

**Arguments:**
- `choices` (list): List of (prompt . response) pairs

**Returns:** nil

**Internal Use:** Used by `scumm-start-dialog`

---

## Inventory API

#### `(scumm-add-to-inventory object)`

Add an object to player inventory.

**Arguments:**
- `object` (symbol): Object identifier

**Returns:** nil

**Side Effects:** Updates inventory state

**Example:**
```elisp
(scumm-add-to-inventory 'key)
```

#### `(scumm-remove-from-inventory object)`

Remove an object from inventory.

**Arguments:**
- `object` (symbol): Object identifier

**Returns:** nil

**Example:**
```elisp
(scumm-remove-from-inventory 'key)
```

#### `(scumm-take-object object)`

Take an object from current room.

**Arguments:**
- `object` (symbol): Object identifier

**Returns:** nil

**Side Effects:**
- Removes from room
- Adds to inventory
- Displays message

**Example:**
```elisp
(scumm-take-object 'milk)
;; Output: "Taken: milk carton"
```

#### `(scumm-drop-object object)`

Drop an object from inventory into current room.

**Arguments:**
- `object` (symbol): Object identifier

**Returns:** nil

**Side Effects:**
- Removes from inventory
- Adds to room
- Displays message

**Example:**
```elisp
(scumm-drop-object 'key)
;; Output: "Dropped: key"
```

---

## Save/Load API

#### `(scumm-save-game filename)`

Save game state to file.

**Arguments:**
- `filename` (string): File path

**Returns:** nil

**Default Directory:** `~/.emacs.d/scumm-saves/`

**Example:**
```elisp
(scumm-save-game "~/my-saves/game1.sav")
;; Output: "Game saved to ~/my-saves/game1.sav"
```

#### `(scumm-load-game filename)`

Load game state from file.

**Arguments:**
- `filename` (string): File path

**Returns:** nil

**Side Effects:**
- Restores game state
- Renders current room

**Example:**
```elisp
(scumm-load-game "~/my-saves/game1.sav")
;; Output: "Game loaded."
```

---

## Rendering API

#### `(scumm-setup-buffer)`

Create and setup the game buffer.

**Returns:** nil

**Side Effects:** Creates `*SCUMM*` buffer, switches to it

**Called by:** `scumm-start-game`

**Example:**
```elisp
(scumm-setup-buffer)
```

#### `(scumm-render-room room-symbol)`

Render a room to the game buffer.

**Arguments:**
- `room-symbol` (symbol): Room identifier

**Returns:** nil

**Side Effects:** Updates `*SCUMM*` buffer display

**Example:**
```elisp
(scumm-render-room 'kitchen)
```

#### `(scumm-insert text &optional face)`

Insert text into game buffer.

**Arguments:**
- `text` (string): Text to insert
- `face` (symbol): Optional face name

**Returns:** nil

**Example:**
```elisp
(scumm-insert "You see a key." 'scumm-object-face)
```

---

## DSL Macros (Script.el)

### Room Definition Macro

#### `(defroom name &rest props)`

Define a room with convenient syntax.

**Usage:**
```elisp
(defroom kitchen
  :name "Kitchen"
  :description "A messy kitchen."
  :objects (fridge table)
  :exits ((north . hallway))
  :on-enter (lambda () (scumm-message "You're in the kitchen!")))
```

**Equivalent to:**
```elisp
(scumm-define-room 'kitchen
  :name "Kitchen"
  :description "A messy kitchen."
  ...)
```

### Object Definition Macro

#### `(defobject name &rest props)`

Define an object with convenient syntax.

**Usage:**
```elisp
(defobject fridge
  :name "refrigerator"
  :room kitchen
  :description "A white fridge."
  :verbs '((open . fridge-open)
           (close . fridge-close)))
```

### Actor Definition Macro

#### `(defactor name &rest props)`

Define an actor with convenient syntax.

**Usage:**
```elisp
(defactor pirate
  :name "Captain Blackbeard"
  :room tavern
  :dialog pirate-dialog)
```

---

## Utility Functions

#### `(scumm-find-objects-in-current-room)`

Get all objects in current room.

**Returns:** List of object symbols

**Example:**
```elisp
(scumm-find-objects-in-current-room)  ;; => (fridge table)
```

#### `(scumm-find-actors-in-current-room)`

Get all actors in current room.

**Returns:** List of actor symbols

**Example:**
```elisp
(scumm-find-actors-in-current-room)  ;; => (pirate)
```

#### `(scumm-add-object-to-room object room)`

Manually add object to a room.

**Arguments:**
- `object` (symbol): Object identifier
- `room` (symbol): Room identifier

**Returns:** nil

**Example:**
```elisp
(scumm-add-object-to-room 'key 'hallway)
```

#### `(scumm-remove-object-from-room object room)`

Manually remove object from a room.

**Arguments:**
- `object` (symbol): Object identifier
- `room` (symbol): Room identifier

**Returns:** nil

**Example:**
```elisp
(scumm-remove-object-from-room 'key 'hallway)
```

---

## Global Variables

#### `scumm-game-state`

The central game state plist. Contains:
- `:current-room` - Current room symbol
- `:inventory` - List of items
- `:flags` - Alist of game flags
- `:rooms` - Hash table of room definitions
- `:objects` - Hash table of object definitions
- `:dialogs` - Hash table of dialog definitions
- `:actors` - Alist of actor definitions

#### `scumm-running`

Boolean indicating if a game is currently active.

#### `scumm-message-callback`

Function called to display messages. Set to `scumm-render-message` by default.

#### `scumm-buffer-name`

Name of the game buffer (default: `"*SCUMM*"`).

#### `scumm-save-directory`

Directory for save files (default: `~/.emacs.d/scumm-saves/`).

---

## Keybindings

| Key | Command | Function |
|-----|---------|----------|
| `l` | `scumm-cmd-look` | Look around |
| `e` | `scumm-cmd-examine` | Examine object |
| `o` | `scumm-cmd-open` | Open object |
| `u` | `scumm-cmd-use` | Use object |
| `t` | `scumm-cmd-talk` | Talk to actor |
| `i` | `scumm-cmd-inventory` | Show inventory |
| `g` | `scumm-cmd-go` | Go to room |
| `q` | `scumm-quit-game` | Quit game |

---

## Common Patterns

### Creating Object Handlers

```elisp
(defun fridge-open ()
  "Open the fridge."
  (if (scumm-get-object-state 'fridge :open)
      (scumm-message "It's already open.")
    (scumm-set-object-state 'fridge :open t)
    (scumm-add-to-inventory 'milk)
    (scumm-message "You open the fridge and find milk!")))

(defobject fridge
  :name "refrigerator"
  :room kitchen
  :verbs '((open . fridge-open)))
```

### Conditional Dialogs

```elisp
(defun has-sword-response ()
  (if (scumm-in-inventory? 'sword)
      (scumm-message "Nice sword you got there!")
    (scumm-message "You should get yourself a sword.")))

(defdialog knight-dialog
  ("Hello." . "Hail, traveler!")
  ("What about weapons?" . has-sword-response)
  ("Bye." . :end))
```

### Room Entry Hooks

```elisp
(defroom haunted-cave
  :name "Haunted Cave"
  :description "A spooky cave with strange noises."
  :on-enter (lambda ()
              (unless (scumm-get-flag 'entered-cave-before)
                (scumm-set-flag 'entered-cave-before)
                (scumm-message "Eerie sounds echo through the cave..."))))
```

---

## Error Handling

Most functions silently fail or use game messages instead of throwing errors. For robust games, add checks:

```elisp
(when (scumm-get-room 'undefined-room)
  (error "Room not defined!"))

(if (scumm-in-inventory? 'required-item)
    (scumm-message "You have what you need.")
  (scumm-message "You're missing something."))
```

---

## Performance Tips

1. **Lazy Define**: Define rooms/objects only when needed
2. **Reuse Handlers**: Use common handler functions
3. **Flag Usage**: Use flags instead of object state when possible
4. **Message Batching**: Group related messages

```elisp
;; Good
(defun open-door ()
  (scumm-set-object-state 'door :open t)
  (scumm-message "Door opens.\nYou enter."))

;; Less efficient
(defun open-door ()
  (scumm-set-object-state 'door :open t)
  (scumm-message "Door opens.")
  (scumm-message "You enter."))
```

# Getting Started with Emacs SCUMM

A step-by-step guide to get up and running with the SCUMM engine in 10 minutes.

## Installation

### Step 1: Clone the Repository

```bash
cd ~/Projects/priv
git clone https://github.com/afeldman/emacs-scumm.git
cd emacs-scumm
```

### Step 2: Add to Your Emacs Config

Add to `~/.emacs.d/init.el`:

```elisp
(add-to-list 'load-path "~/Projects/priv/emacs-scumm")
(require 'scumm)
```

### Step 3: Test Installation

Open Emacs and run:

```
M-x scumm-start-demo
```

You should see:

```
Kitchen
======

A messy kitchen with dirty dishes everywhere. There's a fridge in the corner...
```

**Success!** ✅ The engine is working.

---

## Playing the Demo Game

### Basic Controls

| Key | Action |
|-----|--------|
| `l` | Look around |
| `e` | Examine object |
| `o` | Open object |
| `u` | Use object |
| `t` | Talk to actor |
| `i` | Check inventory |
| `g` | Go to another room |
| `q` | Quit game |

### Demo Walkthrough

1. **Start in Kitchen**
   - Press `l` to look around
   - You see: refrigerator, dirty-dishes

2. **Open the Fridge**
   - Press `o` (open)
   - Select: "refrigerator"
   - You take milk!
   - Press `i` to confirm in inventory

3. **Go to Hallway**
   - Press `g` (go)
   - Select: "north"
   - Meet a mysterious pirate

4. **Talk to Pirate**
   - Press `t` (talk)
   - The pirate asks for a drink
   - Select: "Do you know about treasure?"

5. **Give Milk to Pirate**
   - Press `u` (use)
   - Select: "milk carton"
   - The pirate reveals the treasure location!

6. **Go to Bedroom**
   - Press `g`
   - Select: "north"

7. **Open Treasure Chest**
   - Press `o`
   - Select: "treasure chest"
   - You find a gold coin!
   
**🏆 You win!**

---

## Creating Your First Game

### Quick Start Template

Create a file `my-first-game.el`:

```elisp
;;; my-first-game.el --- My First Adventure -*- lexical-binding: t; -*-

(require 'scumm-script)
(require 'scumm-inventory)

;;; ========== ROOMS ==========

(defroom starting-room
  :name "My Room"
  :description "You're in a simple room. There's a door to the east."
  :objects (desk lamp)
  :exits ((east . outside)))

(defroom outside
  :name "Outside"
  :description "You're outside! Freedom!"
  :objects nil
  :exits ((west . starting-room)))

;;; ========== OBJECTS ==========

(defobject desk
  :name "wooden desk"
  :room starting-room
  :description "A sturdy wooden desk."
  :verbs '((examine . (lambda () (scumm-message "Nothing special.")))))

(defobject lamp
  :name "desk lamp"
  :room starting-room
  :description "A brass desk lamp."
  :verbs '((take . take-lamp)))

(defun take-lamp ()
  (scumm-take-object 'lamp))

;;; ========== START GAME ==========

(provide 'my-first-game)
;;; my-first-game.el ends here
```

### Load and Play Your Game

```elisp
(load "~/path/to/my-first-game.el")
(scumm-start-game 'starting-room)
```

**That's it!** You now have a playable game.

---

## Common Tasks

### Add a New Room

```elisp
(defroom kitchen
  :name "Kitchen"
  :description "A cozy kitchen with a stove."
  :objects (stove sink)
  :exits ((out . hallway)))
```

### Add an Object You Can Take

```elisp
(defobject golden-key
  :name "golden key"
  :room treasure-room
  :description "A shiny golden key."
  :verbs '((take . take-key)))

(defun take-key ()
  (scumm-take-object 'golden-key))
```

### Add an NPC with Dialog

```elisp
(defactor wise-sage
  :name "Wise Sage"
  :room tower
  :dialog sage-dialog)

(defdialog sage-dialog
  ("Hello." . "Greetings, traveler.")
  ("Can you help?" . "Perhaps, what do you seek?")
  ("Bye." . :end))
```

### Use an Item with an Object

```elisp
(defobject locked-door
  :name "locked door"
  :room hallway
  :verbs '((use . use-door)))

(defun use-door ()
  (if (scumm-in-inventory? 'golden-key)
      (progn
        (scumm-remove-from-inventory 'golden-key)
        (scumm-message "The key fits! Door unlocked!")
        (scumm-set-flag 'door-unlocked)
        ;; Door is now passable
        )
    (scumm-message "It's locked. You need a key.")))
```

### Track Game Progress with Flags

```elisp
;; Set a flag
(scumm-set-flag 'talked-to-sage)

;; Check a flag
(if (scumm-get-flag 'talked-to-sage)
    (scumm-message "The sage already spoke to you.")
  (scumm-message "The sage has never seen you before."))

;; Clear a flag
(scumm-clear-flag 'talked-to-sage)
```

---

## Understanding Game Structure

### Typical Game Flow

```
1. Load engine: (require 'scumm)
2. Define world: (defroom ...) (defobject ...) (defdialog ...)
3. Start game:   (scumm-start-game 'starting-room)
4. Player acts:  Keys trigger commands (l, e, o, u, t, g)
5. Game updates: Objects, flags, room state change
6. Player wins:  Reach goal condition
```

### Three Core Concepts

**1. Rooms** - Locations
```elisp
(defroom kitchen :name "Kitchen" :description "..." :exits (...))
```

**2. Objects** - Things you interact with
```elisp
(defobject fridge :name "fridge" :room kitchen :verbs (...))
```

**3. Actors** - NPCs you talk to
```elisp
(defactor pirate :name "Pirate" :room tavern :dialog pirate-dialog)
```

---

## Tips & Tricks

### Tip 1: Test Each Room

After defining a room, test that you can enter it:

```elisp
;; After defining both rooms
(scumm-start-game 'room-a)
(scumm-goto-room 'room-b)  ; Should display room-b
```

### Tip 2: Use Messages for Feedback

Always tell the player what's happening:

```elisp
(defun open-chest ()
  (scumm-message "You slowly open the chest...")
  (scumm-add-to-inventory 'gold-coin)
  (scumm-message "Inside is a gold coin!"))
```

### Tip 3: Combine Objects and Flags

Create puzzles by requiring multiple items:

```elisp
(defun solve-puzzle ()
  (if (and (scumm-in-inventory? 'torch)
           (scumm-in-inventory? 'flint))
      (progn
        (scumm-message "You strike flint with torch...")
        (scumm-set-flag 'fire-lit)
        (scumm-message "A fire roars to life!"))
    (scumm-message "You need fire-making materials.")))
```

### Tip 4: Dialog with Multiple Branches

Handle different player responses:

```elisp
(defdialog merchant-dialog
  ("I want to buy something." . show-shop)
  ("How much for the sword?" . (lambda ()
                                (scumm-message "500 gold coins.")))
  ("Too expensive!" . (lambda ()
                       (scumm-message "Take it or leave it.")))
  ("Bye." . :end))
```

### Tip 5: Use Conditional Descriptions

Change text based on game state:

```elisp
(defobject treasure-chest
  :name "treasure chest"
  :room cave
  :description (if (scumm-get-flag 'chest-opened)
                  "An empty treasure chest."
                "A locked treasure chest.")
  :verbs '((open . open-chest)))
```

---

## Troubleshooting

### Problem: "I don't understand that." message

**Cause:** Command not recognized

**Solution:** Check keybindings in scumm-mode. Press `?` to see available commands.

### Problem: Object not found

**Cause:** Object name doesn't match

**Solution:** 
1. Check object definition: `:name "correct-name"`
2. Check object is in current room: `:room current-room`
3. Press `l` to list objects in room

### Problem: Can't find room

**Cause:** Room isn't defined

**Solution:**
```elisp
;; Check if room exists
(scumm-get-room 'my-room)  ;; Should return non-nil
```

### Problem: Inventory won't update

**Cause:** `scumm-take-object` not called properly

**Solution:**
```elisp
;; Wrong
(scumm-message "You took it!")

;; Right
(scumm-take-object 'item)  ;; Handles message internally
```

### Problem: Dialog not triggering

**Cause:** Actor not in room

**Solution:**
```elisp
;; Check actor is in room
(scumm-actor-in-room? 'my-actor)  ;; Should be t

;; Check actor dialog is defined
(defdialog my-actor-dialog ...)
```

---

## Next Steps

1. **Read the API Reference**: See [API.md](API.md) for all available functions
2. **Study the Demo Game**: Check [games/demo.el](games/demo.el) for examples
3. **Read the Developer Guide**: See [GAME_DEVELOPMENT_GUIDE.md](GAME_DEVELOPMENT_GUIDE.md) for advanced patterns
4. **Create Your Game**: Build something awesome!

---

## Example: Simple Mystery Game

Here's a tiny complete game to get ideas from:

```elisp
;;; mystery-game.el --- A tiny mystery -*- lexical-binding: t; -*-

(require 'scumm-script)
(require 'scumm-inventory)

;; Rooms
(defroom foyer
  :name "Foyer"
  :description "You're in a foyer. There's a door to the east."
  :objects (clue-photo)
  :exits ((east . study)))

(defroom study
  :name "Study"
  :description "A dusty study with bookshelves."
  :objects (locked-drawer)
  :exits ((west . foyer)))

;; Objects
(defobject clue-photo
  :name "mysterious photo"
  :room foyer
  :description "A photo of someone you don't recognize."
  :verbs '((take . take-photo)))

(defun take-photo ()
  (scumm-take-object 'clue-photo))

(defobject locked-drawer
  :name "wooden drawer"
  :room study
  :description "A locked drawer."
  :verbs '((open . try-drawer)))

(defun try-drawer ()
  (if (scumm-in-inventory? 'clue-photo)
      (progn
        (scumm-message "The photo is a key to the drawer!")
        (scumm-add-to-inventory 'letter)
        (scumm-message "You found a mysterious letter. Game won!"))
    (scumm-message "The drawer is locked.")))

;; Play
(scumm-start-game 'foyer)
```

Run it:
```elisp
(load "mystery-game.el")
;; Find photo, go east, open drawer with photo, WIN!
```

---

## Getting Help

- Check [API.md](API.md) for function documentation
- Read [GAME_DEVELOPMENT_GUIDE.md](GAME_DEVELOPMENT_GUIDE.md) for patterns
- Look at [games/demo.el](games/demo.el) for examples
- Review [ARCHITECTURE.md](ARCHITECTURE.md) for how things work

**Have fun creating!** 🎮

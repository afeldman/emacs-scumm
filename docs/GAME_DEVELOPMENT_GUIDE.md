# SCUMM Game Development Guide

A comprehensive guide to creating adventure games with the Emacs SCUMM engine.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Game Structure](#game-structure)
3. [Creating Rooms](#creating-rooms)
4. [Creating Objects](#creating-objects)
5. [Creating Actors & Dialogs](#creating-actors--dialogs)
6. [Advanced Patterns](#advanced-patterns)
7. [Testing Your Game](#testing-your-game)
8. [Debugging](#debugging)

---

## Getting Started

### Setup

1. Create a new directory for your game:
```bash
mkdir my-adventure
cd my-adventure
```

2. Create a new game file:
```elisp
;;; my-game.el --- My Adventure Game -*- lexical-binding: t; -*-

(require 'scumm-script)
(require 'scumm-inventory)

;; Your game code here...

(provide 'my-game)
;;; my-game.el ends here
```

3. Load and start your game:
```elisp
(add-to-list 'load-path "/path/to/emacs-scumm")
(require 'scumm)
(load "path/to/my-game.el")
(scumm-start-game 'starting-room)
```

### Minimal Game Example

```elisp
(require 'scumm-script)

;; Define a simple room
(defroom start-room
  :name "Starting Room"
  :description "A simple room with a door."
  :objects (locked-door)
  :exits ((out . outside)))

;; Define an object
(defobject locked-door
  :name "wooden door"
  :room start-room
  :description "A locked wooden door."
  :verbs '((open . try-open-door)))

;; Object handler
(defun try-open-door ()
  (scumm-message "The door is locked."))

;; Define exit room
(defroom outside
  :name "Outside"
  :description "You're outside! Freedom!"
  :exits ((in . start-room)))

;; Start the game
(scumm-start-game 'start-room)
```

---

## Game Structure

### Recommended File Organization

For larger games, split into multiple files:

```
my-adventure/
├── my-game.el          # Main file, loads everything
├── locations.el        # Room definitions
├── objects.el          # Object definitions
├── npcs.el            # Actor definitions
├── dialogs.el         # Dialog trees
├── handlers.el        # Event handlers
└── README.md
```

### Example Main File

```elisp
;;; my-game.el --- My Adventure Game -*- lexical-binding: t; -*-

(require 'scumm-script)
(require 'scumm-inventory)

;; Load game components
(load "locations.el")
(load "objects.el")
(load "npcs.el")
(load "dialogs.el")
(load "handlers.el")

;; Game initialization
(scumm-message "Welcome to My Adventure!")
(scumm-start-game 'starting-location)

(provide 'my-game)
;;; my-game.el ends here
```

---

## Creating Rooms

### Basic Room

```elisp
(defroom kitchen
  :name "Kitchen"
  :description "A cozy kitchen with wooden cabinets and a stove."
  :objects (fridge stove)
  :exits ((out . hallway)))
```

### Room with Entry Hook

Entry hooks run when the player enters the room:

```elisp
(defroom secret-cave
  :name "Secret Cave"
  :description "A mysterious cave glowing with blue light."
  :on-enter (lambda ()
              ;; Show message only first time
              (unless (scumm-get-flag 'discovered-cave)
                (scumm-set-flag 'discovered-cave)
                (scumm-message "You found a secret cave!"))))
```

### Conditional Room Descriptions

```elisp
(defroom house-door
  :name "Front Door"
  :description (if (scumm-get-flag 'door-unlocked)
                  "The door is open."
                "The door is closed and locked.")
  :exits (if (scumm-get-flag 'door-unlocked)
            '((in . house-interior))
          '()))
```

### Room with Multiple Exits

```elisp
(defroom crossroads
  :name "Crossroads"
  :description "A clearing where four paths diverge."
  :objects nil
  :exits ((north . forest)
          (south . village)
          (east . mountain)
          (west . river)))
```

---

## Creating Objects

### Static Object

```elisp
(defobject painting
  :name "oil painting"
  :room art-gallery
  :description "A beautiful landscape painting of mountains."
  :verbs '((examine . (lambda ()
                       (scumm-message "It's a stunning piece of work.")))))
```

### Takeable Object

```elisp
(defobject gold-key
  :name "golden key"
  :room treasure-room
  :description "An ornate golden key."
  :verbs '((take . take-key)
           (use . use-key)))

(defun take-key ()
  (scumm-take-object 'gold-key))

(defun use-key ()
  (scumm-message "You need something to unlock with it."))
```

### Interactive Object with State

```elisp
(defobject chest
  :name "wooden chest"
  :room treasure-room
  :description (lambda ()
                (if (scumm-get-object-state 'chest :open)
                    "An open chest full of treasure."
                  "A locked wooden chest."))
  :state (:open nil :contains (gold-coin ruby))
  :verbs '((open . open-chest)
           (close . close-chest)
           (examine . examine-chest)))

(defun open-chest ()
  (if (scumm-get-object-state 'chest :open)
      (scumm-message "It's already open.")
    (if (scumm-in-inventory? 'chest-key)
        (progn
          (scumm-set-object-state 'chest :open t)
          (scumm-add-to-inventory 'gold-coin)
          (scumm-add-to-inventory 'ruby)
          (scumm-message "You unlock the chest with the key!\nYou find gold and a ruby!"))
      (scumm-message "The chest is locked. You need a key."))))

(defun close-chest ()
  (if (scumm-get-object-state 'chest :open)
      (progn
        (scumm-set-object-state 'chest :open nil)
        (scumm-message "You close the chest."))
    (scumm-message "It's already closed.")))

(defun examine-chest ()
  (let ((desc (if (scumm-get-object-state 'chest :open)
                  "An open chest full of treasure."
                "A locked wooden chest.")))
    (scumm-message desc)))
```

### Object with Complex Logic

```elisp
(defobject vending-machine
  :name "vending machine"
  :room arcade
  :description "An old coin-operated vending machine."
  :state (:has-item t)
  :verbs '((use . use-machine)
           (insert . insert-coin)))

(defun use-machine ()
  (scumm-message "You press the buttons on the vending machine."))

(defun insert-coin ()
  (if (not (scumm-in-inventory? 'coin))
      (scumm-message "You don't have a coin.")
    (if (scumm-get-object-state 'vending-machine :has-item)
        (progn
          (scumm-remove-from-inventory 'coin)
          (scumm-add-to-inventory 'soda)
          (scumm-set-object-state 'vending-machine :has-item nil)
          (scumm-message "You insert the coin.\nClunk! A soda drops down."))
      (scumm-message "The machine is empty."))))
```

---

## Creating Actors & Dialogs

### Simple Actor with Dialog

```elisp
(defactor merchant
  :name "Old Merchant"
  :room market-square
  :dialog merchant-dialog)

(defdialog merchant-dialog
  ("Hello!" . "Howdy, stranger!")
  ("What do you sell?" . "Just curiosities and wonders.")
  ("Bye." . :end))
```

### Dialog with Conditional Responses

```elisp
(defactor guard
  :name "City Guard"
  :room city-gates
  :dialog guard-dialog)

(defdialog guard-dialog
  ("Can I enter?" . guard-permission)
  ("Who are you?" . "I guard these gates.")
  ("Bye." . :end))

(defun guard-permission ()
  (if (scumm-in-inventory? 'official-pass)
      (progn
        (scumm-set-flag 'has-permission)
        (scumm-message "Ah, you have a pass. Welcome!"))
    (scumm-message "Sorry, you need a pass to enter.")))
```

### Multi-Branch Dialog Tree

```elisp
(defdialog wizard-dialog
  ("Hello, wise one." . "Greetings, traveler.")
  ("Can you help me?" . wizard-help-branch)
  ("Tell me about the curse." . "The curse came from the dark tower...")
  ("Bye." . :end))

(defun wizard-help-branch ()
  (scumm-message "I can teach you magic... for a price.")
  ;; Could trigger another dialog
  (scumm-start-dialog 'wizard-price-dialog))

(defdialog wizard-price-dialog
  ("How much?" . "1000 gold coins.")
  ("Never mind." . :end)
  ("I'll pay!" . payment-accepted))

(defun payment-accepted ()
  (if (scumm-get-flag 'has-1000-gold)
      (progn
        (scumm-set-flag 'learned-magic)
        (scumm-message "Excellent! I teach you the spell of fire!"))
    (scumm-message "You don't have enough gold!")))
```

### Actor with State

```elisp
(defactor drunk-sailor
  :name "Drunk Sailor"
  :room tavern
  :dialog sailor-dialog)

(defdialog sailor-dialog
  ("Ahoy!" . sailor-response))

(defun sailor-response ()
  (let ((drinks (scumm-get-flag 'sailor-drinks-given)))
    (cond
     ((< (or drinks 0) 2)
      (scumm-message "*hiccup* Blow me down!")
      (scumm-set-flag 'sailor-drinks-given (+ (or drinks 0) 1)))
     ((eq drinks 2)
      (scumm-message "Zzzzz..." (scumm-set-flag 'sailor-unconscious))))))
```

---

## Advanced Patterns

### Item Combinations

```elisp
(defobject cauldron
  :name "bubbling cauldron"
  :room witch-hut
  :verbs '((use . use-cauldron)))

(defun use-cauldron ()
  (cond
   ((and (scumm-in-inventory? 'moonflower)
         (scumm-in-inventory? 'dragon-scale))
    (scumm-remove-from-inventory 'moonflower)
    (scumm-remove-from-inventory 'dragon-scale)
    (scumm-add-to-inventory 'magic-potion)
    (scumm-message "The cauldron bubbles and produces a magic potion!"))
   (t
    (scumm-message "You need moonflower and dragon scale to make the potion."))))
```

### Puzzle Chains

```elisp
(defobject rusty-lever
  :name "rusty lever"
  :room control-room
  :verbs '((use . pull-lever)))

(defun pull-lever ()
  (if (scumm-get-flag 'lever-pulled)
      (scumm-message "The lever doesn't move anymore.")
    (scumm-set-flag 'lever-pulled)
    (scumm-message "CREAK! You pull the lever.")
    (scumm-message "In the distance, you hear a rumble...")
    
    ;; Make new room accessible
    (scumm-set-flag 'secret-door-open)
    (when (eq (scumm-get-current-room) 'control-room)
      (scumm-goto-room 'control-room)))) ; Re-render to show new exit
```

### Time-Based Triggers

```elisp
(defroom reactor-core
  :name "Reactor Core"
  :description "The reactor is humming dangerously."
  :on-enter (lambda ()
              (unless (scumm-get-flag 'reactor-cooled)
                (scumm-message "WARNING: Reactor overheating!")
                ;; Could trigger game-over after certain conditions
                (scumm-set-flag 'turns-until-meltdown 5))))
```

### Dynamic Inventory Use

```elisp
(defobject mystery-door
  :name "mysterious door"
  :room hallway
  :verbs '((use . use-door-with-inventory)))

(defun use-door-with-inventory ()
  (let ((inv (scumm-get-state :inventory)))
    (cond
     ((memq 'gold-key inv)
      (scumm-message "The golden key fits perfectly!")
      (scumm-goto-room 'treasury))
     ((memq 'silver-key inv)
      (scumm-message "Silver key is close, but not quite."))
     (t
      (scumm-message "The door is locked. You need a key.")))))
```

### Object Movement Between Rooms

```elisp
(defobject wandering-cat
  :name "orange cat"
  :room starting-room
  :verbs '((pet . pet-cat)))

;; Called periodically (would need a game loop)
(defun move-cat ()
  (let ((current (plist-get (scumm-get-object 'wandering-cat) :room))
        (next-room (if (eq current 'kitchen) 'hallway 'kitchen)))
    (scumm-remove-object-from-room 'wandering-cat current)
    (scumm-add-object-to-room 'wandering-cat next-room)
    (scumm-message (format "The cat wanders off to the %s."
                          (plist-get (scumm-get-room next-room) :name)))))

(defun pet-cat ()
  (scumm-message "*purr* The cat is happy!"))
```

---

## Testing Your Game

### Manual Testing Checklist

- [ ] All rooms are accessible
- [ ] All objects can be examined
- [ ] All verbs work as intended
- [ ] Dialog flows correctly
- [ ] Inventory works
- [ ] Save/Load works
- [ ] Win condition is reachable
- [ ] No dead ends

### Automated Testing

```elisp
(defun test-game-basic ()
  "Run basic smoke tests on the game."
  (interactive)
  
  ;; Start game
  (scumm-start-game 'start-location)
  
  ;; Test room transitions
  (scumm-goto-room 'kitchen)
  (assert (eq (scumm-get-current-room) 'kitchen))
  
  ;; Test objects
  (assert (scumm-get-object 'fridge))
  (assert (scumm-object-in-room? 'fridge 'kitchen))
  
  ;; Test inventory
  (scumm-add-to-inventory 'key)
  (assert (scumm-in-inventory? 'key))
  (scumm-remove-from-inventory 'key)
  (assert (not (scumm-in-inventory? 'key)))
  
  ;; Test flags
  (scumm-set-flag 'test-flag)
  (assert (scumm-get-flag 'test-flag))
  
  (message "All tests passed!"))
```

---

## Debugging

### Printing Game State

```elisp
(defun debug-game-state ()
  "Print current game state to buffer."
  (interactive)
  (pop-to-buffer "*SCUMM Debug*")
  (erase-buffer)
  (insert (format "Current Room: %s\n" (scumm-get-current-room)))
  (insert (format "Inventory: %s\n" (scumm-get-state :inventory)))
  (insert (format "Flags: %s\n" (scumm-get-state :flags)))
  (insert "\nObjects in current room:\n")
  (dolist (obj (scumm-find-objects-in-current-room))
    (insert (format "  - %s\n" obj))))
```

### Interactive Debugging Commands

```elisp
;; Teleport to any room
(defun debug-goto (room)
  (interactive "SRoom: ")
  (scumm-goto-room (intern room)))

;; Give yourself an item
(defun debug-give (item)
  (interactive "SItem: ")
  (scumm-add-to-inventory (intern item)))

;; Set a flag
(defun debug-set-flag (flag)
  (interactive "SFlag: ")
  (scumm-set-flag (intern flag)))

;; Clear inventory
(defun debug-clear-inventory ()
  (interactive)
  (scumm-set-state :inventory nil)
  (scumm-message "Inventory cleared."))
```

### Error Checking

```elisp
;; Verify all rooms exist
(defun check-room-references ()
  (let ((rooms (scumm-get-state :rooms)))
    (maphash (lambda (room-sym room-def)
               (let ((exits (plist-get room-def :exits)))
                 (dolist (exit exits)
                   (unless (gethash (cdr exit) rooms)
                     (warn "Room %s references undefined exit %s"
                           room-sym (cdr exit))))))
             rooms)))

;; Verify object references
(defun check-object-rooms ()
  (maphash (lambda (obj-sym obj-def)
             (let ((room (plist-get obj-def :room)))
               (when (and room (not (gethash room (scumm-get-state :rooms))))
                 (warn "Object %s references undefined room %s"
                       obj-sym room))))
           (scumm-get-state :objects)))
```

---

## Performance Considerations

### Lazy Loading

```elisp
;; Only define rooms/objects when game starts
(defun init-game-content ()
  (defroom kitchen ...)
  (defroom hallway ...)
  ;; etc.
  )

(scumm-start-game 'initial-room)  ; Then call init-game-content before
```

### Efficient State Management

```elisp
;; Instead of many small flags, use a structured flag
(scumm-set-flag 'room-state '(:visited t :examined-objects (table chair)))
(scumm-get-flag 'room-state)  ;; => '(:visited t :examined-objects ...)

;; Instead of duplicate descriptions, use functions
(defobject variable-object
  :name "changing thing"
  :description (lambda ()
                 (if (scumm-get-flag 'magic-activated)
                     "It glows with magic light."
                   "It looks ordinary.")))
```

---

## Example: Complete Small Game

See [games/demo.el](/games/demo.el) for a complete example game with:
- Multiple rooms
- Interactive objects
- NPCs with dialogs
- Inventory management
- Puzzle solving

To run:
```elisp
(load "games/demo.el")
(scumm-start-demo)
```

;;; demo.el --- Demo game for SCUMM engine -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Anton Feldmann

;;; Commentary:

;; A simple demo game showcasing the SCUMM engine features.

;;; Code:

(require 'scumm-script)
(require 'scumm-inventory)

;;; ========== ROOMS ==========

(defroom kitchen
  :name "Kitchen"
  :description "A messy kitchen with dirty dishes everywhere. There's a fridge in the corner and a doorway to the north."
  :objects (fridge dirty-dishes)
  :exits ((north . hallway))
  :on-enter (lambda ()
              (unless (scumm-get-flag 'been-in-kitchen)
                (scumm-set-flag 'been-in-kitchen)
                (scumm-message "You smell something funny..."))))

(defroom hallway
  :name "Hallway"
  :description "A dark hallway with peeling wallpaper. Doors lead north to a bedroom and south to the kitchen. A mysterious pirate stands here."
  :objects nil
  :exits ((north . bedroom)
          (south . kitchen)))

(defroom bedroom
  :name "Bedroom"
  :description "A small bedroom with a single bed and a window overlooking the sea."
  :objects (treasure-chest)
  :exits ((south . hallway))
  :on-enter (lambda ()
              (when (and (scumm-get-flag 'talked-to-pirate)
                        (not (scumm-get-flag 'seen-chest)))
                (scumm-set-flag 'seen-chest)
                (scumm-message "The pirate mentioned something about a chest..."))))

;;; ========== OBJECTS ==========

(defobject fridge
  :name "refrigerator"
  :room kitchen
  :description "A white refrigerator covered in magnets."
  :state (:open nil)
  :verbs '((open . fridge-open)
           (use . fridge-use)))

(defun fridge-open ()
  "Open the fridge."
  (if (scumm-get-object-state 'fridge :open)
      (scumm-message "The fridge is already open.")
    (scumm-set-object-state 'fridge :open t)
    (scumm-add-to-inventory 'milk)
    (scumm-message "You open the fridge and find a carton of milk. You take it.")))

(defun fridge-use ()
  "Use fridge."
  (scumm-message "You should probably OPEN it first."))

(defobject dirty-dishes
  :name "dirty dishes"
  :room kitchen
  :description "A pile of dirty dishes with suspicious green mold."
  :verbs '((use . dishes-use)))

(defun dishes-use ()
  "Use dishes."
  (scumm-message "Ew, no thanks. You're not touching those."))

(defobject milk
  :name "milk carton"
  :room nil  ; starts in fridge, added to inventory when opened
  :description "A carton of milk. Best before... oh, that was 3 months ago."
  :verbs '((use . milk-use)))

(defun milk-use ()
  "Use milk."
  (if (scumm-actor-in-room? 'pirate)
      (progn
        (scumm-remove-from-inventory 'milk)
        (scumm-set-flag 'gave-milk-to-pirate)
        (scumm-message "You give the expired milk to the pirate. He drinks it and looks happy!")
        (scumm-message "The pirate tells you about a treasure chest in the bedroom."))
    (scumm-message "You don't want to drink expired milk.")))

(defobject treasure-chest
  :name "treasure chest"
  :room bedroom
  :description "An old wooden chest with a rusty lock."
  :state (:open nil)
  :verbs '((open . chest-open)))

(defun chest-open ()
  "Open treasure chest."
  (if (scumm-get-flag 'gave-milk-to-pirate)
      (if (scumm-get-object-state 'treasure-chest :open)
          (scumm-message "The chest is already open.")
        (scumm-set-object-state 'treasure-chest :open t)
        (scumm-add-to-inventory 'gold-coin)
        (scumm-message "You open the chest and find a shiny gold coin! You win!"))
    (scumm-message "The chest is locked. You need to find a way to open it.")))

(defobject gold-coin
  :name "gold coin"
  :room nil
  :description "A shiny gold doubloon!"
  :verbs nil)

;;; ========== ACTORS ==========

(defactor pirate
  :name "Mysterious Pirate"
  :room hallway
  :dialog pirate-dialog)

;;; ========== DIALOGS ==========

(defdialog pirate-dialog
  ("Hello." . "Arr! Welcome to me ship... er, house!")
  ("Who are you?" . "I be Captain Blackbeard! Well, his great-great-grandson.")
  ("Do you know anything about treasure?" . pirate-treasure-response)
  ("Bye." . :end))

(defun pirate-treasure-response ()
  "Pirate's response about treasure."
  (if (scumm-get-flag 'gave-milk-to-pirate)
      (scumm-message "Arr! Thanks for the milk, matey! Check the bedroom for me treasure chest!")
    (progn
      (scumm-set-flag 'talked-to-pirate)
      (scumm-message "Arr! I might know somethin'... but I be mighty thirsty. Bring me somethin' to drink!"))))

;;; ========== INITIALIZATION ==========

(scumm-message "Demo game loaded. Type 'g' to go, 'l' to look, 'e' to examine, 'o' to open, 'u' to use, 't' to talk, 'i' for inventory.")

(provide 'demo)
;;; demo.el ends here

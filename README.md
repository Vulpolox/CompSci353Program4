# CompSci353Program4

# Instructions on How to Play Game

### 1. [Download](https://drive.google.com/drive/folders/10djgK9eD6HKZJDYwvFVGhxvo42yZiPNk?usp=sharing) the executable and run it (Windows only for now)

## OR

### 1. Clone the repo

### 2. Open Game.rkt in DrRacket

### 3. Run the file

# Source Code Shortcuts

[Format.rkt](https://github.com/Vulpolox/CompSci353Program4/blob/main/Format.rkt)

[UI.rkt](https://github.com/Vulpolox/CompSci353Program4/blob/main/UI.rkt)

[GameState.rkt](https://github.com/Vulpolox/CompSci353Program4/blob/main/GameState.rkt)

[Minigames.rkt](https://github.com/Vulpolox/CompSci353Program4/blob/main/Minigames.rkt)

[Menus.rkt](https://github.com/Vulpolox/CompSci353Program4/blob/main/Menus.rkt)

[Game.rkt](https://github.com/Vulpolox/CompSci353Program4/blob/main/Game.rkt)

# Game Overview

## Setting

The game takes place in a sprawling cave system.  The player has to collect coins Cookie Clicker-style to progress

## Locations

* Main Area -- the central hub in the cave system.  Paths branch out in the four cardinal directions
* North Path -- A small area of the cave that has a number guessing game
* South Path -- A massive area of the cave with a logic puzzle, a memorization game, and some random events
  - Secret Passage -- is found by having a certain item while doing a certain thing.  Contains a clue
* West Path -- A room at the end of a hallway with a monster to fight and a dark labyrinth to explore (also might contain another logic puzzle)
  - Treasure Room -- Is found at the end of the labyrinth
  - Deep Treasure Room -- Is found at the true end of the labyrinth
* East Path -- A subterranean casino that has several games to play including blackjack and a slot machine
* Victory Path -- the final area of the game (may contain a logic puzzle)

## Inventory Items

* Upgrade Module V1 -- found in North Path
* Upgrade Module V2 -- found in South Path
* Upgrade Module V3 -- found in West Path
* Upgrade Module V4 -- found in East Path
* Upgrade Module R -- found in South Path
* Upgrade Module R2 -- found in Deep Treasure Room
* Key -- found in North Path
* Mimic Key -- found in South Path and East Path
* Exit Key -- found in Victory Path
* Lantern -- found in Treasure Room
* Sword -- found in Secret Passage
* Coins -- found and used in every area

## Goal State

The player can complete the game by collecting 20,000,000 coins, using those coins to unlock the\
Victory Path, obtaining the Exit Key, and leaving the cave

## Sources Used

* Logic puzzles were based off of "Mimic Logic" by Nihohe Soft ([https://store.steampowered.com/app/2455920/Mimic_Logic/](https://store.steampowered.com/app/2455920/Mimic_Logic/))
* I used generative AI to create random intersting facts and funny dialogue that are displayed at differnt points in the game

## Base Program Requirements

### Movement

Movement is implemented in multiple ways throughout the game.  For the majority of the game, the player uses\
relative direction (i.e. "Enter the passage"), but there is a sizeable chunk of the game where player movement\
is constrained to the cardinal directions

### Inventory

The inventory system has all required features.  In the main six areas, the player has the option to view their\
inventory by choosing "Show Info" from the menu.  The player also has the option to drop and pick up items by choosing\
the "Drop items" option from the menu.  Dropped items and items revealed using search persist\
even when leaving and re-entering areas

### Reporting the Player's Location

This can be accomplished by choosing "Show info" from the menus of the six main areas.

### List All Items in the Player's Inventory

This can be accomplished by choosing "Show info" from the menus of the six main areas.

### Describe the Current Area

A detailed description of the area is given when the player uses the search command for\
each of the main areas.  A more brief description can be viewed at any time by\
choosing the "Show info" option

### Search the Current Area

A one-time option to search the current area is available for each of the main areas.\
This can be done by choosing the "Check your surroundings" option from the menu.\
This command reveals items and activities for the player to do.

### Help

The "Show info" option from the menus accomplishes 2/3 of the requirements for the help command\
(gives player location and displays all items in inventory).  The third requirement is satisfied by\
the structure of the game itself.  Everything in the game is handled by menus, so all possible commands\
are visible to the player at all times.

## Optional Features (Extra Credit)

### Random Events

In the South Path area, the player has the option to explore in the dark.  This can reward the player with coins,\
take away the player's coins, and is even used as the method to obtain one of the Upgrade Modules

### Muliple Endings

There is a good ending (escaping the cave) and a bad ending (failing a late-game logic puzzle)


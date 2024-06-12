# Eight-off Solitaire Game

This Haskell program simulates games of Eight-off solitaire, aiming to achieve decent scores through automated gameplay strategies.

## Author

- **Samiha Fansur**

## Last Modified

- **December 9, 2021**

## Overview

The `Solitaire` module contains data types and functions to represent and manipulate solitaire games, specifically the Eight-off variation. The program automatically plays the game using a series of pre-defined moves and strategies aimed at maximizing the game score.

## Features

- **Data Types:** Defines cards, decks, and boards using Haskell data types.
- **Game Initialization:** Provides functions to deal cards into the initial board setup for both Spider and Eight-off solitaire variations.
- **Gameplay Logic:** Includes functions to detect possible moves, make strategic decisions, and automatically play through a game.
- **Utilities:** Functions to shuffle decks, deal cards, and calculate scores based on the current board state.

## Modules

- **Main Types:** `Suit`, `Pip`, `Card`, `Deck`, `Board`
- **Utilities:** Shuffling and dealing cards, flipping cards, and calculating remaining stock deals.
- **Gameplay Functions:** Functions to automate gameplay, move cards according to rules, handle card foundations, columns, and reserves.

## Usage

1. **Setting Up the Environment:**
   Ensure you have a Haskell compiler like GHC installed.

2. **Compiling the Code:**
   Use the following GHC command to compile the module:
   ```bash
   ghc --make Solitaire.hs

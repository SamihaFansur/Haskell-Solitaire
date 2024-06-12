{-
  A haskell program that plays an Eight-off solitaire game and produces decent scores.
  Written by:Samiha Fansur (aca20sf)
  Last modified on: 09/12/2021
-}
module Solitaire where

 import Data.List
 import Data.Maybe
 import System.Random

 --data types
 data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Show, Enum)
 data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine |Ten | Jack | Queen | King deriving (Eq, Ord, Enum, Show)
 
 data Card = FaceUp Pip Suit | FaceDown Pip Suit deriving Eq
 instance Show Card where
   show (FaceUp pip suit) = "(" ++ show pip ++ ", " ++ show suit ++ ")"
   show (FaceDown _ _) = "<unknown>"


 type Deck = [Card]

 type Stock = Deck

 type Foundations = [Card]
 type Columns = [Deck]
 type Reserves = [Card]

 data Board = EOBoard Foundations Columns Reserves | SBoard Foundations Columns Stock deriving Eq
 instance Show Board where
  show board@(EOBoard foundations columns reserves) =
    "\nEOBoard\nFoundations  " ++  (show foundations) ++  "\n\nColumns" ++ (colStrings columns) ++ "\n\nReserve      "++ (show reserves) ++ "\n"
  show board@(SBoard foundations columns stock) =
    "\nSBoard\nFoundations  " ++  (show foundations) ++  "\n\nColumns" ++ (colStrings columns) ++ "\n\nStock      "++ (show (calcStockDeals stock)) ++ " Deals remaining\n"

  --colStrings
 colStrings :: Columns -> String 
 colStrings columns = foldr (++) "" ["\n             "++(show col) |col<-columns]

 --constants & utilities
 --Sample boards
 boardA :: Board
 boardA = 
      EOBoard
      []
      [
        [ (FaceUp Ace Clubs), (FaceUp Seven Diamonds), (FaceUp Ace Hearts), (FaceUp Queen Hearts), (FaceUp King Spades), (FaceUp Four Spades) ], 
        [ (FaceUp Five Diamonds), (FaceUp Queen Spades), (FaceUp Three Diamonds), (FaceUp Five Spades), (FaceUp Six Spades), (FaceUp Seven Hearts) ], 
        [ (FaceUp King Hearts), (FaceUp Ten Diamonds), (FaceUp Seven Spades), (FaceUp Queen Diamonds), (FaceUp Five Hearts), (FaceUp Eight Diamonds) ], 
        [ (FaceUp Jack Spades), (FaceUp Six Hearts), (FaceUp Seven Clubs), (FaceUp Eight Spades), (FaceUp Ten Clubs), (FaceUp Queen Clubs) ], 
        [ (FaceUp Ace Spades), (FaceUp Eight Clubs), (FaceUp Ace Diamonds), (FaceUp King Diamonds), (FaceUp Jack Hearts), (FaceUp Four Clubs) ], 
        [ (FaceUp Two Diamonds), (FaceUp Three Hearts), (FaceUp Three Clubs), (FaceUp Ten Hearts), (FaceUp Six Diamonds), (FaceUp Jack Spades) ], 
        [ (FaceUp Ten Spades), (FaceUp Three Spades), (FaceUp Nine Hearts), (FaceUp Nine Clubs), (FaceUp Four Diamonds), (FaceUp Nine Clubs) ], 
        [ (FaceUp Eight Hearts), (FaceUp King Spades), (FaceUp Nine Diamonds), (FaceUp Four Hearts), (FaceUp Two Spades), (FaceUp Two Clubs) ]
      ]
      [(FaceUp Two Hearts), (FaceUp Six Clubs), (FaceUp Five Clubs), (FaceUp Jack Diamonds)]

 boardB :: Board
 boardB = 
      SBoard
      [(FaceUp King Hearts), (FaceUp Queen Hearts), (FaceUp Jack Hearts), (FaceUp Ten Hearts), (FaceUp Nine Hearts), (FaceUp Eight Hearts), (FaceUp Seven Hearts), (FaceUp Six Hearts), (FaceUp Five Hearts), (FaceUp Four Hearts), (FaceUp Three Hearts), (FaceUp Two Hearts), (FaceUp Ace Hearts)]
      [
        [(FaceUp Eight Diamonds), (FaceUp Nine Hearts)],
        [(FaceUp Two Diamonds)],
        [(FaceUp Ace Spades), (FaceUp Two Spades), (FaceUp Three Spades), (FaceUp Four Spades), (FaceUp Five Spades), (FaceUp Six Clubs), (FaceUp Seven Clubs), (FaceUp Eight Clubs), (FaceUp Nine Clubs), (FaceUp Ten Diamonds), (FaceUp Jack Diamonds), (FaceUp Queen Diamonds), (FaceUp King Diamonds), (FaceDown Six Hearts), (FaceDown King Clubs)],
        [(FaceUp Seven Clubs), (FaceUp Eight Diamonds), (FaceUp Nine Diamonds), (FaceUp Ten Diamonds), (FaceUp Jack Diamonds), (FaceUp Queen Diamonds), (FaceUp King Diamonds), (FaceUp Nine Clubs), (FaceUp Ten Hearts), (FaceUp Jack Spades)],
        [(FaceUp Ace Hearts), (FaceUp Two Hearts), (FaceUp Three Hearts), (FaceUp Four Hearts), (FaceUp Five Hearts), (FaceUp Six Diamonds), (FaceUp Seven Diamonds), (FaceUp Queen Clubs), (FaceUp King Hearts)],
        [(FaceUp Two Diamonds), (FaceUp Three Diamonds), (FaceUp Four Diamonds)],
        [(FaceUp Jack Clubs), (FaceUp Queen Clubs),(FaceUp King Clubs),(FaceUp Two Spades),(FaceUp Three Spades), (FaceUp Four Diamonds), (FaceUp Five Diamonds), (FaceUp Six Diamonds), (FaceUp Seven Hearts), (FaceUp Eight Clubs), (FaceUp Nine Spades), (FaceUp Ten Clubs), (FaceUp Ace Clubs), (FaceUp Two Clubs), (FaceUp Three Clubs), (FaceUp Four Clubs), (FaceUp Five Spades)],
        [(FaceUp Seven Spades), (FaceUp Eight Spades), (FaceUp Nine Spades), (FaceUp Ten Spades), (FaceUp Jack Spades),(FaceUp Queen Spades), (FaceUp King Spades), (FaceDown Ace Diamonds), (FaceDown Ten Clubs), (FaceDown King Spades)],
        [(FaceUp Jack Hearts), (FaceUp Queen Hearts)],
        [(FaceUp Ace Clubs),(FaceUp Two Clubs)]
      ]
      [
        (FaceDown Eight Hearts), (FaceDown Ace Diamonds), (FaceDown Ten Spades), (FaceDown Three Clubs), (FaceDown Four Spades), (FaceDown Five Clubs), (FaceDown Six Spades), (FaceDown Queen Spades), (FaceDown Three Diamonds), (FaceDown Eight Spades), (FaceDown Four Clubs), (FaceDown Jack Spades), (FaceDown Five Clubs), (FaceDown Six Spades), (FaceDown Nine Diamonds), (FaceDown Seven Spades), (FaceDown Five Diamonds), (FaceDown Ace Spades), (FaceDown Six Clubs), (FaceDown Seven Diamonds)
      ]
      

 ---------------------------------------------------------------SPIDER SOLITAIRE----------------------------------------------------
 --a list of all 104 cards
 sPack :: Deck
 sPack = allCardsInSuit Clubs ++ allCardsInSuit Diamonds ++ allCardsInSuit Hearts ++ allCardsInSuit Spades ++
         allCardsInSuit Clubs ++ allCardsInSuit Diamonds ++ allCardsInSuit Hearts ++ allCardsInSuit Spades
  where
    --list of all cards in each suit
    allCardsInSuit :: Suit -> [Card]
    allCardsInSuit suit = [(FaceDown pip suit) | pip <- listOfPip] 
      where
      listOfPip = [Ace ..]

  --given a seed, deck is shuffled
 sShuffle :: Int -> Deck
 sShuffle seed =
    let shuffledDeck = sortBy (\ (_,w1)(_,w2)  -> (compare w1 w2))(zip sPack rndNums)
        rndNums = take 104 (randoms (mkStdGen seed)::[Int])
    in map fst shuffledDeck

 flipACard :: Card -> Card
 flipACard (FaceUp pip suit) = (FaceDown pip suit)
 flipACard (FaceDown pip suit) = (FaceUp pip suit)

 --deals opening board. uses SEED to return the same board everytime
 sDeal :: Int -> Board
 sDeal seed = let shuffledCards = sShuffle seed
                  foundations = []
                  columns = [(flipACard(head shuffledCards) : take 5 shuffledCards), 
                             (flipACard(head (drop 6 shuffledCards)) : take 5 (drop 7 shuffledCards) ),
                             (flipACard(head (drop 12 shuffledCards)) : take 5 (drop 13 shuffledCards) ),
                             (flipACard(head (drop 18 shuffledCards)) : take 5 (drop 19 shuffledCards) ),
                            
                             (flipACard(head (drop 24 shuffledCards)) : take 4 (drop 25 shuffledCards) ),
                             (flipACard(head (drop 29 shuffledCards)) : take 4 (drop 30 shuffledCards) ),
                             (flipACard(head (drop 34 shuffledCards)) : take 4 (drop 35 shuffledCards) ),
                             (flipACard(head (drop 39 shuffledCards)) : take 4 (drop 40 shuffledCards) ),
                             (flipACard(head (drop 44 shuffledCards)) : take 4 (drop 45 shuffledCards) ),
                             (flipACard(head (drop 49 shuffledCards)) : take 4 (drop 50 shuffledCards) )]
                  stock = drop 54 shuffledCards
               in (SBoard foundations columns stock) 

 calcStockDeals :: Stock -> Int
 calcStockDeals stock = (length stock) `div` 10

 --------------------------------------------------------------EIGHTOFF SOLITAIRE---------------------------------------------------

 --a list of all 52 cards
 pack :: Deck
 pack = allCardsInSuit Clubs ++ allCardsInSuit Diamonds ++ allCardsInSuit Hearts ++ allCardsInSuit Spades
  where 
    --list of all cards in each suit
    allCardsInSuit :: Suit -> [Card]
    allCardsInSuit suit = [(FaceUp pip suit) | pip <- listOfPip] 
        where
        listOfPip = [Ace ..]

 --gives the successor of a card
 sCard :: Card -> Card
 sCard (FaceUp pip suit)
   | pip == King = (FaceUp Ace suit)
   | otherwise = (FaceUp (succ pip) suit)

 --gives the predessor of a card
 pCard :: Card -> Card
 pCard (FaceUp pip suit)
   | pip == Ace = (FaceUp King suit)
   | otherwise = (FaceUp (pred pip) suit)

 --checks if a card is an Ace
 isAce :: Card -> Bool
 isAce (FaceUp pip _) = pip == Ace

 --checks if a card is a King
 isKing :: Card -> Bool
 isKing (FaceUp pip _) = pip == King

 --given a seed, deck is shuffled
 shuffle :: Int -> Deck
 shuffle seed =
    let shuffledDeck = sortBy (\ (_,w1)(_,w2)  -> (compare w1 w2))(zip pack rndNums)
        rndNums = take 52 (randoms (mkStdGen seed)::[Int])
    in map fst shuffledDeck

 --creates columns using the cards remaing in a deck
 createEachColumn :: Deck -> [Deck]
 createEachColumn [] = []
 createEachColumn remainingCardsDeck = rhs : createEachColumn lhs
                     where
                     (rhs, lhs) = splitAt 6 remainingCardsDeck

 --deals opening board. uses SEED to return the same board everytime
 eODeal :: Int -> Board
 eODeal seed = let shuffledCards = shuffle seed
                   foundations = []
                   columns = createEachColumn ( drop 4 shuffledCards)
                   reserves = take 4 shuffledCards
               in (EOBoard foundations columns reserves) 

 --returns the column head
 getTopCardFromNonEmptyColumns :: Board -> Deck
 getTopCardFromNonEmptyColumns (EOBoard foundations columns reserves) = map head (filter (/=[]) columns)
 
 --moves aces from reserves and column heads to foundations
 acesFromReservesAndColToFoundations :: Board -> Board
 acesFromReservesAndColToFoundations board@(EOBoard foundations columns reserves) = (EOBoard newFoundations newColumns newReserves)
    where 
        newFoundations = foundations ++ 
                        (filter isAce reserves) ++ 
                        (filter isAce (getTopCardFromNonEmptyColumns board))
        newColumns = filter (/=[]) (map (\column -> if (isAce (head column)) then (tail column) else column) columns)
        newReserves = filter (not.isAce) reserves

 --moves other moveable cards from reserves and column heads to foundations, given that the predecessor of the card is in a foundation
 cardsFromReservesAndTopColCardToFoundations ::Board -> Board
 cardsFromReservesAndTopColCardToFoundations board@(EOBoard foundations columns reserves) = (EOBoard newFoundations newColumns newReserves)
    where
      newFoundations = concat[(map (\cardInFoundation-> (if ((sCard cardInFoundation) `elem` x) && (isKing cardInFoundation == False) then (sCard cardInFoundation) else cardInFoundation)) foundations) | x <- [reserves ++ (getTopCardFromNonEmptyColumns board)]]   
      newColumns = filter (/=[]) (map (\column -> if ((pCard (head column)) `elem` foundations) then (tail column) else column)columns)
      newReserves = filter (\cardInReserve-> (not((pCard cardInReserve) `elem` foundations ))) reserves
 
 --moves all moveable cards to foundations
 toFoundations :: Board -> Board
 toFoundations board@(EOBoard foundations columns reserves)
    | board /= (acesFromReservesAndColToFoundations board) = toFoundations (acesFromReservesAndColToFoundations board)
    | board /= (cardsFromReservesAndTopColCardToFoundations board) = toFoundations (cardsFromReservesAndTopColCardToFoundations board)
    | otherwise = board

---------------------------------------------------autoplay functions------------------------------------------------------------
 
 --removes a card from a column
 removeCardFromCol :: Board -> Card -> Board
 removeCardFromCol (EOBoard foundations columns reserves) cardToRemove = (EOBoard foundations newColumns reserves)
    where newColumns = map (\c -> filter (/=cardToRemove) c) columns
  
 --returns card from column head if it is a king
 getKingFromColTop :: Columns -> Card
 getKingFromColTop ((topCardFromFirstCol:topCol):otherCols)
    | (length (topCardFromFirstCol:topCol)) > 1 && (isKing topCardFromFirstCol) = topCardFromFirstCol
    | otherwise = getKingFromColTop (filter (/=[]) otherCols) 

 --if an empty column exists move king from the head of a column to that empty column
 moveKingFromColTopToEmptyCol :: Board -> Board
 moveKingFromColTopToEmptyCol board@(EOBoard foundations columns reserves) = (EOBoard foundations newColumns reserves)
     where
         colWithRemovedKing@(EOBoard foundationsN columnsN reservesN) = removeCardFromCol board (getKingFromColTop columns) 
         newColumns = columnsN ++ [[getKingFromColTop columns]]
 
 --returns column head if card under column head is a king
 getKingFromUnderColTopCard :: Columns -> Card
 getKingFromUnderColTopCard (topCol:otherCols)
    | length topCol == 1 = getKingFromUnderColTopCard (filter (/=[])otherCols) 
    | (length topCol > 2) && isKing (topCol !! 1) = topCol !! 0
    | otherwise = getKingFromUnderColTopCard (filter (/=[]) otherCols)
 
 --deletes card from a list (i.e. column or reserve)
 removeCardFromList :: Card -> Deck -> Deck
 removeCardFromList _ [] = []
 removeCardFromList cardToRemove (firstCardOfListToRemoveCardFrom:listToRomoveCardFrom)
     |cardToRemove == firstCardOfListToRemoveCardFrom = removeCardFromList cardToRemove listToRomoveCardFrom
     |otherwise = firstCardOfListToRemoveCardFrom : removeCardFromList cardToRemove listToRomoveCardFrom

 --removes card from reserves
 removeCardFromReserve :: Board -> Card -> Board
 removeCardFromReserve (EOBoard foundations columns reserves) cardToRemove = (EOBoard foundations columns newReserves)
     where newReserves = removeCardFromList cardToRemove reserves

 --returns the king card from reserves
 getKingFromReserve :: Reserves -> Card
 getKingFromReserve (topReserve:otherReserves)
    | isKing topReserve = topReserve
    | otherwise = getKingFromReserve otherReserves

 --moves king card from reserves into an empty column
 moveKingFromReserveToEmptyCol :: Board -> Board
 moveKingFromReserveToEmptyCol board@(EOBoard foundations columns reserves) = (EOBoard foundations newColumns newReserves)
     where
       newColumns = columns ++ [[(getKingFromReserve reserves)]]
       reserveWithRemovedKing@(EOBoard foundationsN columnsN reservesN) = removeCardFromReserve board (getKingFromReserve reserves) 
       newReserves = reservesN

 --checks whether the predecessor of a card exists before it can be moved, unless card is an ace
 canCardMove :: Card -> Deck -> Bool
 canCardMove _ [] = False
 canCardMove (FaceUp Ace _) _ = True
 canCardMove cardToMove (topCardInFoundation:otherFoundationsCards)
    | pCard cardToMove == topCardInFoundation = True
    | otherwise = canCardMove cardToMove otherFoundationsCards

 --moves card to column head
 moveCardToColTop :: Columns -> Card -> Columns
 moveCardToColTop colToMoveTo cardToMove = map (\column -> (if ((head column) == cardToMove) then (pCard cardToMove : column) else column)) colToMoveTo
 
 --gets successor of column head
 getSuccOfTopCard :: Board -> Columns -> Card
 getSuccOfTopCard board@(EOBoard foundations columns reserves) (topCol:otherCols) 
   | length topCol == 1 = getSuccOfTopCard board otherCols
   | isKing (topCol !! 1) = getSuccOfTopCard  board otherCols
   | sCard(topCol !! 1) `elem` (getTopCardFromNonEmptyColumns board) = (topCol !! 0)
   | otherwise = getSuccOfTopCard board otherCols 

 --returns card from a column with just 1 card which is not a king
 oneCardCol :: Board -> Columns -> Card
 oneCardCol board@(EOBoard foundations columns reserves) (topCol:otherCols)
    | (isKing (topCol !! 0) == False) && (length topCol == 1) = topCol !! 0
    | otherwise = oneCardCol board otherCols

 --moves column head to reserve if card under column head is a king
 moveKingFromUnderTopCardInColToReserve :: Board -> Board
 moveKingFromUnderTopCardInColToReserve board@(EOBoard foundations columns reserves) = (EOBoard foundations newColumns newReserves)
     where
         colWithRemovedKing@(EOBoard foundationsN columnsN reservesN) = removeCardFromCol board cardBeingMoved 
         newColumns = columnsN
         cardBeingMoved = getKingFromUnderColTopCard columns
         newReserves = reserves ++ [cardBeingMoved]

 --moves first card to reserve if card under column head is a successor of another column head
 moveCardUnderColTopIfSucc :: Board -> Board
 moveCardUnderColTopIfSucc board@(EOBoard foundations columns reserves) = (EOBoard foundations newColumns newReserves)
     where
         succCard = getSuccOfTopCard board columns
         tempNewColumns@(EOBoard foundationsN columnsN reservesN) = removeCardFromCol board succCard
         newColumns = columnsN
         newReserves = reserves ++ [succCard]

 --if card is not a king and is the only card in a column, move it to reserves
 moveCardFromOneCardColToReserve :: Board -> Board
 moveCardFromOneCardColToReserve board@(EOBoard foundations columns reserves) = (EOBoard foundations newColumns newReserves)
     where
         cardBeingRemoved = oneCardCol board columns
         newColumns = [cols | cols <- columnsN, cols/=[]]
         removeOneCardCol@(EOBoard foundationsN columnsN reservesN) = removeCardFromCol board cardBeingRemoved
         newReserves = reserves ++ [cardBeingRemoved]

 --returns list of successors of cards in foundations
 deckFCardsSucc :: Deck -> Deck -> Deck
 deckFCardsSucc _ [] = []
 deckFCardsSucc [] _ = []
 deckFCardsSucc deckFoundations (topCard:otherCards)
     | (topCard:otherCards) == [] = filter (not.isAce) otherCards
     | canCardMove topCard deckFoundations = filter (not.isAce) (topCard: deckFCardsSucc deckFoundations otherCards)
     | otherwise = deckFCardsSucc deckFoundations otherCards

 --------------------------------find move sub functions-----------------------------------------
 --move column head to reserve if card under column head is a succ of card in foundation
 movingCardUnderTopCardInColToF :: Board -> [Board]
 movingCardUnderTopCardInColToF eoboard@(EOBoard foundations columns reserve) =
   [ if(length reserve <= (fst(x))) && (canCardNMove foundations columns (snd(x))) then (moveCardNFromColTopToReserve eoboard (snd(x))) else (EOBoard [][][]) | x <- [(7,2),(6,3),(5,4),(4,5),(3,6),(2,7),(1,8)]]
      where
        canCardNMove _ [] _ = False
        canCardNMove foundation (topCol:otherCols) n
          | length topCol <= cardIndex = canCardNMove foundation (filter (/=[])otherCols)  n
          | canCardMove (topCol !! cardIndex) foundation = True
          | otherwise = canCardNMove foundation (filter (/=[])otherCols)  n
          where
            cardIndex = n - 1

        moveCardNFromColTopToReserve eoboard@(EOBoard foundations columns reserve) n = (EOBoard foundations newColumns newReserves)
          where
            tempNewColumns@(EOBoard foundationsN columnsN reservesN) = removeCardFromCol eoboard (getCardN foundations columns n)
            newColumns = columnsN
            newReserves = reserve ++ [getCardN foundations columns n]
            getCardN foundation (topCol:otherCols) n
              | length topCol <= cardIndex = getCardN foundation (filter (/=[])otherCols) n
              | canCardMove (topCol !! cardIndex) foundation = (topCol !! 0)
              | otherwise = getCardN foundation (filter (/=[])otherCols) n
              where
                cardIndex = n - 1

 --applies toFoundations to board is not already applied
 isToFBoardSameAsBoard :: Board -> [Board]
 isToFBoardSameAsBoard eoboard@(EOBoard foundations columns reserve)
  | eoboard /= toFoundations eoboard = [toFoundations eoboard]
  | otherwise = [(EOBoard [][][])]

 --move king from column head or reserve to an empty column
 moveKingToEmptyColumn :: Board -> [Board]
 moveKingToEmptyColumn eoboard@(EOBoard foundations columns reserve)
  | (doesReserveHaveKing reserve) && ((length columns) <= 7) =  [moveKingFromReserveToEmptyCol eoboard]
  | (isTopCardInColKing columns) && ((length columns) <= 7) = [moveKingFromColTopToEmptyCol eoboard] 
  | otherwise = [(EOBoard [][][])]
  where
    doesReserveHaveKing [] = False
    doesReserveHaveKing (headreserve:restreserve)
        | isKing headreserve = True
        | otherwise = doesReserveHaveKing restreserve

    isTopCardInColKing [] = False
    isTopCardInColKing (topCol:otherCols)
        | ((length topCol) > 1) && (isKing (head topCol))  = True
        | otherwise = isTopCardInColKing (filter (/=[]) otherCols)

 --move column head to reserve if card under column head is a king or a succ of another column head
 moveTopCardToReserve :: Board -> [Board]
 moveTopCardToReserve eoboard@(EOBoard foundations columns reserve)
  | ((length columns) <= 7) && ((length reserve) <= 7) && (isCardUnderTopCardKing columns) = [moveKingFromUnderTopCardInColToReserve eoboard]
  | (isCardUnderTopCardSucc columns eoboard) && ((length reserve) <= 7) = [moveCardUnderColTopIfSucc eoboard] 
  | otherwise = [(EOBoard [][][])]
  where
    isCardUnderTopCardKing [] = False
    isCardUnderTopCardKing (topCol:otherCols) 
        | length topCol == 1 = isCardUnderTopCardKing (filter (/=[])otherCols)
        | (length topCol > 2) && isKing(topCol !! 1) = True
        | otherwise = isCardUnderTopCardKing (filter (/=[]) otherCols)

    isCardUnderTopCardSucc [] _ = False
    isCardUnderTopCardSucc (topCol:otherCols) eoboard@(EOBoard foundations columns reserve)
        | length topCol == 1 || isKing (topCol !! 1) = isCardUnderTopCardSucc otherCols eoboard
        | elem (sCard(topCol !! 1)) (getTopCardFromNonEmptyColumns eoboard) =  True
        | otherwise = isCardUnderTopCardSucc otherCols eoboard

 --move succ of column head to appropriate column if reserves contain a succ of a column head or another column head is the succ card
 moveTopCardSucc :: Board -> [Board]
 moveTopCardSucc eoboard@(EOBoard foundations columns reserve)
  | length (deckFCardsSucc (getTopCardFromNonEmptyColumns eoboard) (getTopCardFromNonEmptyColumns eoboard)) /= 0 = [moveSuccCardsInCols eoboard]
  | length (deckFCardsSucc reserve (getTopCardFromNonEmptyColumns eoboard)) /= 0 = [movePredFromReserveToColTop eoboard]
  | otherwise = [(EOBoard [][][])]
  where
    moveSuccCardsInCols eoboard@(EOBoard foundations columns reserve) = (EOBoard foundations newColumns reserve)
     where
      tempNewColumns@(EOBoard foundationsN columnsN reservesN) = removeCardFromCol eoboard (pCard(head(deckFCardsSucc (getTopCardFromNonEmptyColumns eoboard) (getTopCardFromNonEmptyColumns eoboard))))
      newColumns = moveCardToColTop (filter (/=[]) columnsN)
                      (( head (deckFCardsSucc (getTopCardFromNonEmptyColumns eoboard) (getTopCardFromNonEmptyColumns eoboard) ) )) 

    movePredFromReserveToColTop eoboard@(EOBoard foundations columns reserve)  = (EOBoard foundations newColumns newReserves)
     where
      newColumns = moveCardToColTop columns cardToMove
      tempNewReserves@(EOBoard foundationsN columnsN reserveN) = removeCardFromReserve eoboard (pCard cardToMove)
      newReserves = reserveN
      cardToMove = head(deckFCardsSucc reserve (getTopCardFromNonEmptyColumns eoboard))

 --moves card to reserve if it is the only card in a column and not a king
 nonKingCardFromColOfOneToReserve :: Board -> [Board]
 nonKingCardFromColOfOneToReserve eoboard@(EOBoard foundations columns reserve)
  | (isColOneCardCol columns eoboard) && ((length reserve) <= 7) = [moveCardFromOneCardColToReserve eoboard]
  | otherwise = [(EOBoard [][][])]
  where
    isColOneCardCol [] _ = False
    isColOneCardCol ((topCol):otherCols) eoboard@(EOBoard foundations columns reserve) 
      | ((isKing(topCol !! 0)) == False) && (length topCol == 1) = True
      | otherwise =  isColOneCardCol otherCols eoboard

 --returns a list of boards after making all legal moves. list ordered from best to worst starting at the top
 findMoves :: Board -> [Board]
 findMoves eoboard@(EOBoard foundations columns reserve) = 
   filter (\board -> board /= (EOBoard [][][])) --filters out empty boards
                                              ( movingCardUnderTopCardInColToF eoboard ++ isToFBoardSameAsBoard eoboard
                                                ++ moveKingToEmptyColumn eoboard ++ moveTopCardToReserve eoboard 
                                                ++ moveTopCardSucc eoboard ++ nonKingCardFromColOfOneToReserve eoboard)

 --chooses the best move after checking that no more moves can be made in findMoves, and then takes the best board from findMoves(top most board)                                                 
 chooseMove :: Board -> Maybe Board
 chooseMove eoboard@(EOBoard foundations columns reserve)
   | length (findMoves eoboard) >= 1 = Just (head (findMoves eoboard))
   | otherwise = Nothing 

 --calculates score of a board depending on number of cards in foundations
 score :: Board -> Int 
 score (EOBoard foundations columns reserve) =  52- (foldr (+) 0 (map length columns)) - (length reserve)

 --plays a whole game of solitaire and returns the score of the final board
 playSolitaire :: Board -> Int
 playSolitaire eoboard@(EOBoard foundations columns reserve)
  | isJust (chooseMove eoboard) = playSolitaire (helperFunc (chooseMove eoboard))
  | otherwise = score eoboard
  where
    helperFunc (Just x) = x 

 --analyseEO will play n number of games specified by the 2nd param of the function. Returns a tuple of values (number of wins, average score)
 analyseEO :: Int -> Int -> (Int,Float)
 analyseEO startSeed noOfGames = (noOfWins, avgScore)
   where 
     noOfWins = length (filter (\n -> (n == 52)) scores)
     avgScore = realToFrac (sum scores) / genericLength scores
     scores = map (\seed -> (playSolitaire (eODeal seed))) [(startSeed)..(startSeed + (noOfGames - 1))]
 
 --returns true or false based on if all the cards are in foundations by checking if all columns and reserves are empty
 haveWon :: Board -> Bool
 haveWon (EOBoard foundations columns reserve)
  | columns == [] && reserve == [] = True
  | otherwise = False


 {- Paste the contents of this file, including this comment, into your source file, below all
    of your code. You can change the indentation to align with your own, but other than this,
    ONLY make changes as instructed in the comments.
 -}
 -- Constants that YOU must set:
 studentName = "Samiha Fansur"
 studentNumber = "200188580"
 studentUsername = "aca20sf"

 initialBoardDefined = boardA {- replace XXX with the name of the constant that you defined
                              in step 3 of part 1 -}
 secondBoardDefined = boardB  {- replace YYY with the constant defined in step 5 of part 1,
                             or if you have chosen to demonstrate play in a different game
                             of solitaire for part 2, a suitable contstant that will show
                             your play to good effect for that game -}

 {- Beyond this point, the ONLY change you should make is to change the comments so that the
    work you have completed is tested. DO NOT change anything other than comments (and indentation
    if needed). The comments in the template file are set up so that only the constant eight-off
    board from part 1 and the toFoundations function from part 1 are tested. You will probably
    want more than this tested.

    CHECK with Emma or one of the demonstrators if you are unsure how to change this.

    If you mess this up, your code will not compile, which will lead to being awarded 0 marks
    for functionality and style.
 -}

 main :: IO()
 main =
   do
     putStrLn $ "Output for " ++ studentName ++ " (" ++ studentNumber ++ ", " ++ studentUsername ++ ")"

     putStrLn "***The eight-off initial board constant from part 1:"
     print initialBoardDefined

     let board = toFoundations initialBoardDefined
     putStrLn "***The result of calling toFoundations on that board:"
     print board

     {- Move the start comment marker below to the appropriate position.
       If you have completed ALL the tasks for the assignment, you can
       remove the comments from the main function entirely.
       DO NOT try to submit/run non-functional code - you will receive 0 marks
       for ALL your code if you do, even if *some* of your code is correct.
     -}


     let boards = findMoves board      -- show that findMoves is working
     putStrLn "***The possible next moves after that:"
     print boards

     let chosen = chooseMove board     -- show that chooseMove is working
     putStrLn "***The chosen move from that set:"
     print chosen

     putStrLn "***Now showing a full game"     -- display a full game
     score <- displayGame initialBoardDefined 0
     putStrLn $ "Score: " ++ score
     putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire initialBoardDefined)


     putStrLn "\n\n\n************\nNow looking at the alternative game:"

     putStrLn "***The spider initial board constant from part 1 (or equivalent if playing a different game of solitaire):"
     print secondBoardDefined          -- show the suitable constant. For spider solitaire this
                                       -- is not an initial game, but a point from which the game
                                       -- can be won

     {- start comment marker - move this if appropriate
     putStrLn "***Now showing a full game for alternative solitaire"
     score <- displayGame secondBoardDefined 0 -- see what happens when we play that game (assumes chooseMove
                                                -- works correctly)
     putStrLn $ "Score: " ++ score
     putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire secondBoardDefined)

     -}

 {- displayGame takes a Board and move number (should initially be 0) and
    displays the game step-by-step (board-by-board). The result *should* be
    the same as performing playSolitaire on the initial board, if it has been
    implemented correctly.
    DO NOT CHANGE THIS CODE other than aligning indentation with your own.
 -}
 displayGame :: Board -> Int ->IO String
 displayGame board n =
   if haveWon board
     then return "A WIN"
     else
       do
         putStr ("Move " ++ show n ++ ": " ++ show board)
         let maybeBoard = chooseMove board
         if isJust maybeBoard then
           do
             let (Just newBoard) = maybeBoard
             displayGame newBoard (n+1)
         else
           do
             let score = show (playSolitaire board)
             return score
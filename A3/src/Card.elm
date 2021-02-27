-----------------------
-- Samuil Daniela
-- 16.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Card exposing (Card(..), Face(..), Suit(..), cardValue, viewCard, cardToString, deck)

import Html exposing (..)
import Html.Attributes exposing (style)

{-
  Replace with your definitions from assignment 1
-}
type Face = Ace |  Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Suit = Clubs | Diamonds | Hearts | Spades
type Card = Card Face Suit

faceToString : Face -> String
faceToString face =
   case face of
   Ace -> "The face is Ace"
   Two -> "The face is Two"
   Three -> "The face is Three"
   Four -> "The face is Four"
   Five -> "The face is Five"
   Six -> "The face is Six"
   Seven -> "The face is Seven"
   Eight -> "The face is Eight"
   Nine -> "The face is Nine"
   Ten -> "The face is Ten"
   Jack -> "The face is Jack"
   Queen -> "The face is Queen"
   King -> "The face is King"


suitToString: Suit -> String
suitToString suit =
   case suit of
   Clubs -> "The suit is Clubs"
   Diamonds -> "The suit is Diamonds"
   Spades -> "The suit is Spades"
   Hearts -> "The suit is Hearts"

cardToString: Card -> String
cardToString (Card f s) =
    faceToString (f) ++ " of " ++ suitToString (s)


cardValue : Card -> List Int
cardValue card=
    let
        cardValueHelper l=
            case card of
                Card Two _ -> [2]
                Card Three _ -> [3]
                Card Four _ -> [4]
                Card Five _ -> [5]
                Card Six _ -> [6]
                Card Seven _ -> [7]
                Card Eight _ -> [8]
                Card Nine _ -> [9]
                Card Ace _ -> List.append [1,11] l
                Card _ _ -> [10]
    in
        cardValueHelper []

deck : List Card
deck=
    let
        deckHelper l i=
            case i of
                0 -> Card Ace Clubs :: deckHelper l (i+1)
                1 -> Card Ace Diamonds :: deckHelper l (i+1)
                2 -> Card Ace Hearts :: deckHelper l (i+1)
                3 -> Card Ace Spades :: deckHelper l (i+1)
                4 -> Card Two Clubs :: deckHelper l (i+1)
                5 -> Card Two Diamonds :: deckHelper l (i+1)
                6 -> Card Two Hearts :: deckHelper l (i+1)
                7 -> Card Two Spades :: deckHelper l (i+1)
                8 -> Card Three Clubs :: deckHelper l (i+1)
                9 -> Card Three Diamonds :: deckHelper l (i+1)
                10 -> Card Three Hearts :: deckHelper l (i+1)
                11 -> Card Three Spades :: deckHelper l (i+1)
                12 -> Card Four Clubs :: deckHelper l (i+1)
                13 -> Card Four Diamonds :: deckHelper l (i+1)
                14 -> Card Four Hearts :: deckHelper l (i+1)
                15 -> Card Four Spades :: deckHelper l (i+1)
                16 -> Card Five Clubs :: deckHelper l (i+1)
                17 -> Card Five Diamonds :: deckHelper l (i+1)
                18 -> Card Five Hearts :: deckHelper l (i+1)
                19 -> Card Five Spades :: deckHelper l (i+1)
                20 -> Card Six Clubs :: deckHelper l (i+1)
                21 -> Card Six Diamonds :: deckHelper l (i+1)
                22 -> Card Six Hearts :: deckHelper l (i+1)
                23 -> Card Six Spades :: deckHelper l (i+1)
                24 -> Card Seven Clubs :: deckHelper l (i+1)
                25 -> Card Seven Diamonds :: deckHelper l (i+1)
                26 -> Card Seven Hearts :: deckHelper l (i+1)
                27 -> Card Seven Spades :: deckHelper l (i+1)
                28 -> Card Eight Clubs :: deckHelper l (i+1)
                29 -> Card Eight Diamonds :: deckHelper l (i+1)
                30 -> Card Eight Hearts :: deckHelper l (i+1)
                31 -> Card Eight Spades :: deckHelper l (i+1)
                32 -> Card Nine Clubs :: deckHelper l (i+1)
                33 -> Card Nine Diamonds :: deckHelper l (i+1)
                34 -> Card Nine Hearts :: deckHelper l (i+1)
                35 -> Card Nine Spades :: deckHelper l (i+1)
                36 -> Card Ten Clubs :: deckHelper l (i+1)
                37 -> Card Ten Diamonds :: deckHelper l (i+1)
                38 -> Card Ten Hearts :: deckHelper l (i+1)
                39 -> Card Ten Spades :: deckHelper l (i+1)
                40 -> Card Jack Clubs :: deckHelper l (i+1)
                41 -> Card Jack Diamonds :: deckHelper l (i+1)
                42 -> Card Jack Hearts :: deckHelper l (i+1)
                43 -> Card Jack Spades :: deckHelper l (i+1)
                44 -> Card Queen Clubs :: deckHelper l (i+1)
                45 -> Card Queen Diamonds :: deckHelper l (i+1)
                46 -> Card Queen Hearts :: deckHelper l (i+1)
                47 -> Card Queen Spades :: deckHelper l (i+1)
                48 -> Card King Clubs :: deckHelper l (i+1)
                49 -> Card King Diamonds :: deckHelper l (i+1)
                50 -> Card King Hearts :: deckHelper l (i+1)
                51 -> Card King Spades :: deckHelper l (i+1)
                _ -> []
    in
        deckHelper [] 0

{-
  Modify this function (if needed) to work with your `Card` definition
-}
cardToUnicode : Card -> String
cardToUnicode (Card face suit) =
   case face of
     Ace -> case suit of
       Spades ->"🂡"
       Hearts -> "🂱"
       Clubs ->  "🃑"
       Diamonds -> "🃁"
     Two -> case suit of
       Spades ->"🂢"
       Hearts -> "🂲"
       Clubs ->  "🃒"
       Diamonds -> "🃂"
     Three -> case suit of
       Spades ->"🂣"
       Hearts -> "🂳"
       Clubs ->  "🃓"
       Diamonds ->"🃃"
     Four -> case suit of
       Spades ->"🂤"
       Hearts -> "🂴"
       Clubs ->  "🃔"
       Diamonds -> "🃄"
     Five -> case suit of
       Spades ->"🂥"
       Hearts -> "🂵"
       Clubs ->  "🃕"
       Diamonds -> "🃅"
     Six -> case suit of
       Spades ->"🂦"
       Hearts -> "🂶"
       Clubs ->  "🃖"
       Diamonds -> "🃆"
     Seven -> case suit of
       Spades ->"🂧"
       Hearts -> "🂷"
       Clubs ->  "🃗"
       Diamonds -> "🃇"
     Eight -> case suit of
       Spades -> "🂨"
       Hearts ->  "🂸"
       Clubs ->   "🃘"
       Diamonds ->  "🃈"
     Nine -> case suit of
       Spades -> "🂩"
       Hearts ->  "🂹"
       Clubs ->   "🃙"
       Diamonds ->  "🃉"
     Ten -> case suit of
       Spades ->"🂪"
       Hearts -> "🂺"
       Clubs ->  "🃚"
       Diamonds -> "🃊"
     Jack -> case suit of
       Spades ->"🂫"
       Hearts -> "🂻"
       Clubs ->  "🃛"
       Diamonds -> "🃋"
     Queen -> case suit of
       Spades ->"🂭"
       Hearts -> "🂽"
       Clubs ->  "🃝"
       Diamonds -> "🃍"
     King -> case suit of
       Spades -> "🂮"
       Hearts -> "🂾"
       Clubs ->  "🃞"
       Diamonds -> "🃎"


{-
  Modify this function (if needed) to work with your `Card` definition
-}
viewCard : Card -> Html msg
viewCard card =
   let
     (Card face suit) = card
     faceName = faceToString face
     suitName = suitToString suit
     suitColor s =
       case s of
         Diamonds -> "red"
         Spades -> "black"
         Hearts -> "red"
         Clubs -> "black"
     unicode = cardToUnicode card
   in
     div [style "display" "inline-block"] [
       div [style "font-size" "12em", style "color" (suitColor suit)] [text unicode],
       div [style "font-size" "0.8em"]  [text (cardToString card)]
     ]
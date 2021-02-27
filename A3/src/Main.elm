-----------------------
-- Samuil Daniela
-- 16.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Main exposing (main, calculateScore)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Random
import Debug

import Card exposing (..)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { hand: List Card,
    deck: List Card,
    showDeck: Bool
  }

startingModel : Model
startingModel =
    Model [] Card.deck True

init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , Cmd.none
  )


type Msg
  = Draw
  | NewCard Card
  | ToogleDeck

-- I added the cardToUnicode here too because for some reason it would not be recognized as imported from the Card model
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

faceToString : Face -> String
faceToString face =
   case face of
   Ace -> "Ace"
   Two -> "Two"
   Three -> "Three"
   Four -> "Four"
   Five -> "Five"
   Six -> "Six"
   Seven -> "Seven"
   Eight -> "Eight"
   Nine -> "Nine"
   Ten -> "Ten"
   Jack -> "Jack"
   Queen -> "Queen"
   King -> "King"

showCardsBW: List Card -> String
showCardsBW list=
    case list of
        [] -> ""
        x1::xs1 -> cardToUnicode x1 ++ showCardsBW xs1


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draw ->
      ( model
      , drawCard model
      )
    
    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewCard newCard ->
      ( {model | hand = model.hand ++ [newCard], deck = List.filter (\x -> x /= newCard) (model.deck) }
      , Cmd.none
      )

    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToogleDeck ->
      (
       if model.showDeck == True then { model | showDeck = False} else {model | showDeck = True}
      , Cmd.none
      )

drawCard : Model -> Cmd Msg
drawCard model =
  case model.deck of
    (first::rest) -> Random.generate NewCard (Random.uniform first rest)
    _ -> Cmd.none

{-
  1. Get the value of each card (use `cardValue`)
  2. Generate a list of all possible scores
  3. Return the score closest to 21 (less than 21!), if one exists, else the smallest score over 21
  ```elm
  calculateScore [Card King Hearts] == 10
  calculateScore [Card Two Hearts] == 2
  calculateScore [Card Two Hearts, Card King Spades] == 12
  calculateScore [Card Ace Hearts, Card King Spades] == 21
  calculateScore [Card Ace Hearts, Card Five Hearts, Card Seven Spades] == 13
  calculateScore [Card King Hearts, Card Five Hearts, Card Seven Spades] == 22
  calculateScore [Card King Hearts, Card Ten Clubs, Card Ace Spades] == 21
  calculateScore [Card Ace Spades, Card Ace Clubs, Card Ten Clubs, Card King Clubs] == 22
  ```
-}

helperFunction: List Int -> Int
helperFunction list=
    let
        filterOutBiggerThan21 list1 =
            List.filter (\x -> x <= 21) list1
    in
        case List.reverse (List.sort(filterOutBiggerThan21 list)) of
            [] -> 0
            x::xs -> x

calculateScore : List Card -> Int
calculateScore cards =
        let
            cardValuehelper list=
                case (cardValue list) of
                    [] -> 0
                    x::xs -> x

            sumWithoutAces list=
                case list of
                     [] -> 0
                     x::xs -> cardValuehelper x + sumWithoutAces xs
        in
            case cards of
                [] -> 0
                x::xs ->if (cardToString x) == "Ace of Clubs" || (cardToString x) == "Ace of Diamonds" || (cardToString x) == "Ace of Hearts" || (cardToString x) == "Ace of Spades" then
                        let
                            howManyAces: List Card -> Int
                            howManyAces list =
                             List.length ( List.filter (\x1 -> (cardToString x1) == "Ace of Clubs" || (cardToString x1) == "Ace of Diamonds" || (cardToString x1) == "Ace of Hearts" || (cardToString x1) == "Ace of Spades") list )

                            createListWithAllValues1: List Card -> List Int
                            createListWithAllValues1 list=
                                [11 + sumWithoutAces (List.filter (\x1 -> (cardToString x1) /= "Ace of Clubs" && (cardToString x1) /= "Ace of Diamonds" && (cardToString x1) /= "Ace of Hearts" && (cardToString x1) /= "Ace of Spades") list )]
                                ++ [1 + sumWithoutAces (List.filter (\x1 -> (cardToString x1) /= "Ace of Clubs" && (cardToString x1) /= "Ace of Diamonds" && (cardToString x1) /= "Ace of Hearts" && (cardToString x1) /= "Ace of Spades") list )]

                            createListWithAllValues2: List Card -> List Int
                            createListWithAllValues2 list =
                                [11 + 1 + sumWithoutAces (List.filter (\x1 -> (cardToString x1) /= "Ace of Clubs" && (cardToString x1) /= "Ace of Diamonds" && (cardToString x1) /= "Ace of Hearts" && (cardToString x1) /= "Ace of Spades") list )]
                                 ++ [1 + 1 + sumWithoutAces (List.filter (\x1 -> (cardToString x1) /= "Ace of Clubs" && (cardToString x1) /= "Ace of Diamonds" && (cardToString x1) /= "Ace of Hearts" && (cardToString x1) /= "Ace of Spades") list )]
                                 ++ [11 + 11 + sumWithoutAces (List.filter (\x1 -> (cardToString x1) /= "Ace of Clubs" && (cardToString x1) /= "Ace of Diamonds" && (cardToString x1) /= "Ace of Hearts" && (cardToString x1) /= "Ace of Spades") list )]

                            createListWithAllValues3: List Card -> List Int
                            createListWithAllValues3 list =
                                [11 + 1 + 1 + sumWithoutAces (List.filter (\x1 -> (cardToString x1) /= "Ace of Clubs" && (cardToString x1) /= "Ace of Diamonds" && (cardToString x1) /= "Ace of Hearts" && (cardToString x1) /= "Ace of Spades") list )]
                                ++ [11 + 11 + 1 + sumWithoutAces (List.filter (\x1 -> (cardToString x1) /= "Ace of Clubs" && (cardToString x1) /= "Ace of Diamonds" && (cardToString x1) /= "Ace of Hearts" && (cardToString x1) /= "Ace of Spades") list )]
                                ++ [11 + 11 + 11 + sumWithoutAces (List.filter (\x1 -> (cardToString x1) /= "Ace of Clubs" && (cardToString x1) /= "Ace of Diamonds" && (cardToString x1) /= "Ace of Hearts" && (cardToString x1) /= "Ace of Spades") list )]
                                ++ [1 + 1 + 1 + sumWithoutAces (List.filter (\x1 -> (cardToString x1) /= "Ace of Clubs" && (cardToString x1) /= "Ace of Diamonds" && (cardToString x1) /= "Ace of Hearts" && (cardToString x1) /= "Ace of Spades") list )]

                            createListWithAllValues4: List Card -> List Int
                            createListWithAllValues4 list =
                                [11 + 1 + 1 + 1 + sumWithoutAces (List.filter (\x1 -> (cardToString x1) /= "Ace of Clubs" && (cardToString x1) /= "Ace of Diamonds" && (cardToString x1) /= "Ace of Hearts" && (cardToString x1) /= "Ace of Spades") list )]
                                ++ [11 + 11 + 11 + 1 + sumWithoutAces (List.filter (\x1 -> (cardToString x1) /= "Ace of Clubs" && (cardToString x1) /= "Ace of Diamonds" && (cardToString x1) /= "Ace of Hearts" && (cardToString x1) /= "Ace of Spades") list )]
                                ++ [11 + 11 + 1 + 1 + sumWithoutAces (List.filter (\x1 -> (cardToString x1) /= "Ace of Clubs" && (cardToString x1) /= "Ace of Diamonds" && (cardToString x1) /= "Ace of Hearts" && (cardToString x1) /= "Ace of Spades") list )]
                                ++ [11 + 11 + 11 + 11 + sumWithoutAces (List.filter (\x1 -> (cardToString x1) /= "Ace of Clubs" && (cardToString x1) /= "Ace of Diamonds" && (cardToString x1) /= "Ace of Hearts" && (cardToString x1) /= "Ace of Spades") list )]
                                ++ [1 + 1 + 1 + 1 + sumWithoutAces (List.filter (\x1 -> (cardToString x1) /= "Ace of Clubs" && (cardToString x1) /= "Ace of Diamonds" && (cardToString x1) /= "Ace of Hearts" && (cardToString x1) /= "Ace of Spades") list )]

                        in
                            case (howManyAces cards) of
                                1 -> helperFunction (createListWithAllValues1 cards)
                                2 -> helperFunction (createListWithAllValues2 cards)
                                3 -> helperFunction (createListWithAllValues3 cards)
                                _ -> helperFunction (createListWithAllValues4 cards)

                        else cardValuehelper x + calculateScore xs

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

{-
  Use the `viewCard` function for showing the player's hand and the `cardToUnicode` function for the remaining deck.
-}

view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"

    winOrLose: Int -> String
    winOrLose nr=
        if nr==21 then "You won, congratulations!" else if nr>21 then "You lost, try again" else ""

    showCards: Bool -> List Card -> Html Msg
    showCards b list=
        if b==True then showCardsColor list else div [] [ h1 [] [text ""] ]

    showCardsColor: List Card -> Html Msg
    showCardsColor list=
        div [] (List.map Card.viewCard list)

  in
    div []
      [ div [] [ h1 [] [text appName] ]
       , button [ onClick ToogleDeck ] [ text "toggle" ]
       , button [ onClick Draw ] [ text "draw" ]
       , div [] [ h1 [] [text "Here is the deck:"] ]
       --, div [] [ h1 [] [text (showCards model.showDeck model.deck)] ]
       , showCards model.showDeck model.deck
       , div [] [ h1 [] [text "Here is the hand:"] ]
       --, div [] [ h1 [] [text (showCards True model.hand)] ]
       , showCardsColor model.hand
       , div [] [ h1 [] [text (String.fromInt (calculateScore model.hand))] ]
       , div [] [ h1 [] [text (winOrLose (calculateScore model.hand))] ]
      ]
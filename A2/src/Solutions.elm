-----------------------
-- Samuil Daniela Teodora
-- 28.10.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Solutions exposing (..)

-- deck : List Card
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

-- cardValue : Card -> List Int
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

smallestK : Int -> List comparable -> List comparable
smallestK k l=
    let
        partition : comparable -> List comparable -> (List comparable, List comparable)
        partition pivot list =
            (List.filter (\x -> x < pivot) list, List.filter (\x -> x >= pivot) list)

        quicksort : List comparable -> List comparable
        quicksort list1 =
            case list1 of
            [] -> []
            x::xs -> let
                        (less, greater) = partition x xs
                    in
                (quicksort less) ++ [x] ++ (quicksort greater)

        smallestKHelper i=
            if List.isEmpty l then []
            else (List.take i (quicksort l) )
    in
        smallestKHelper k

balanced : String -> Bool
balanced s=
    let
        listString = String.toList s
        balancedHelper stack list=
            case list of
                [] -> if List.isEmpty stack then True else False
                x::xs -> if x == '(' then balancedHelper (x :: stack) xs
                         else if x == ')' then if List.isEmpty stack then False
                                               else balancedHelper (List.drop 1 stack) xs
                              else balancedHelper stack xs
    in
        balancedHelper [] listString

coinChange : Int -> List Int -> Int
coinChange sum coins=
    let
        coinChangeHelper s c =
            if sum == 0 then 1
                else if sum < 0 then 0
                     else if List.length coins == 0 then 0
                          else
                          let
                            helper s1 c1 =
                                case c1 of
                                [] -> 1
                                x::xs -> helper s1 (List.take ((List.length c1) - 1) c1 ) + helper ( s1 - x ) c1
                          in
                            helper s c
    in coinChangeHelper sum coins
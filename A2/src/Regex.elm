-----------------------
-- Samuil Daniela Teodora
-- 28.10.2020
-----------------------

module Regex exposing (..)

type RegexPattern
  = Literal Char
  | Many RegexPattern
  | OneOf RegexPattern RegexPattern
  | Seq RegexPattern RegexPattern

{-
  The `Ok` variant represents the matched input and the rest of the unmatched input
  The `Err` variant represents the original input
-}
type alias RegexResult = Result (List Char) (List Char, List Char)

{-
  Returns the `Ok` variant if the literal character matches the first character of the string.
  If the string is empty or the characters don't match the `Err` variant should be returned.
  ```elm
  matchLit 'a' ['a', 'b', 'b'] == Ok (['a'], ['b', 'b'])
  matchLit 'c' ['a', 'b', 'b'] == Err ['a', 'b', 'b']
  matchLit 'c' [] == Err []
  ```
-}
matchLit : Char -> List Char -> RegexResult
matchLit ch l=
    case l of
        [] -> Err []
        x::xs -> if ch == x then Ok ([x],xs) else Err l


{-
  Matches `pat1` and then `pat2`. Returns `Ok` only if both succeed.
  ```elm
  matchSeq (Literal 'a') (Literal 'b') ['a', 'b', 'c'] == Ok (['a', 'b'], ['c'])
  matchSeq (Literal 'a') (Literal 'b') ['a', 'x', 'c'] == Err (['a', 'x', 'c'])
  matchSeq (Seq (Literal 'a') (Literal 'b')) (Literal 'c') ['a', 'b', 'c', 'd'] == Ok (['a', 'b', 'c'], ['d'])
  ```
-}
matchSeq : RegexPattern -> RegexPattern -> List Char -> RegexResult
matchSeq pat1 pat2 input =
        case (pat1,pat2) of
        (Literal a, Literal b) -> let
                                      firstMatch = matchLit a input
                                      secondMatch = matchLit b (List.drop 1 input)
                                   in
                                      case input of
                                      [] -> Err []
                                      x::xs -> case (firstMatch,secondMatch) of
                                               (Ok _ , Ok _) -> Ok (a::[b], List.drop 2 input)
                                               (_,_) -> Err input

        (Seq c d, Seq e f) -> let
                                    (Literal ch1) = c
                                    (Literal ch2) = d
                                    (Literal ch3) = e
                                    (Literal ch4) = f

                                    firstMatchSeq1 = matchLit ch1 input
                                    firstMatchSeq2 = matchLit ch2 (List.drop 1 input)
                                    secondMatchSeq1 = matchLit ch3 (List.drop 2 input)
                                    secondMatchSeq2 = matchLit ch4 (List.drop 3 input)
                             in
                                    case input of
                                    [] -> Err []
                                    x::xs -> case (firstMatchSeq1,firstMatchSeq2) of
                                             (Ok _ , Ok _) -> case (secondMatchSeq1,secondMatchSeq2) of
                                                              (Ok _, Ok _) -> Ok (ch1::[ch2,ch3,ch4],List.drop 4 input)
                                                              (_,_) -> Err input
                                             (_,_) -> Err input
        (Seq g h, Literal i) -> let

                                    (Literal ch5) = g
                                    (Literal ch6) = h

                                    firstMatchSeq3 = matchLit ch5 input
                                    firstMatchSeq4 = matchLit ch6 (List.drop 1 input)
                                    secondMatch1 = matchLit i (List.drop 2 input)
                                 in
                                    case input of
                                    [] -> Err []
                                    x::xs -> case (firstMatchSeq3,firstMatchSeq4, secondMatch1) of
                                             (Ok _ , Ok _, Ok _) -> Ok (ch5::[ch6,i],List.drop 3 input)
                                             (_,_,_) -> Err input
        (Literal j, Seq q l) -> let

                                    (Literal ch7) = q
                                    (Literal ch8) = l

                                    firstMatch1 = matchLit j input
                                    secondMatchSeq3 = matchLit ch7 (List.drop 1 input)
                                    secondMatchSeq4 = matchLit ch8 (List.drop 2 input)

                                in
                                    case input of
                                    [] -> Err []
                                    x::xs -> case (firstMatch1, secondMatchSeq3, secondMatchSeq4) of
                                             (Ok _ , Ok _, Ok _) -> Ok (j::[ch7,ch8],List.drop 3 input)
                                             (_,_,_) -> Err input
        (_,_) -> Err input

{-
  Matches the pattern `pattern` zero or many times. Always returns the `Ok` variant.
  ```elm
  matchMany (Literal 'a') ['a', 'a', 'a'] == Ok (['a', 'a', 'a'], [])
  matchMany (Literal 'b') ['a', 'a', 'a'] == Ok ([], ['a', 'a', 'a'])
  matchMany (Literal 'b') ['b', 'b', 'a'] == Ok (['b', 'b'], ['a'])
  matchMany (Seq (Literal 'b') (Literal 'a')) ['b', 'a', 'b', 'a', 'c'] == Ok (['b', 'a', 'b', 'a'], ['c'])
  matchMany (Seq (Literal 'b') (Literal 'a')) ['b', 'a', 'c', 'a', 'c'] == Ok (['b', 'a'], ['c', 'a', 'c'])
  ```
-}
matchMany : RegexPattern -> List Char -> RegexResult
matchMany pattern input =
    let
        list: RegexPattern -> List Char -> List Char
        list p i=
            case p of
            Literal a              -> let
                                            firstMatch = matchLit a i
                                      in
                                            case i of
                                            [] -> []
                                            x::xs -> case firstMatch of
                                                     (Ok _ ) -> List.append [x] (list pattern ( List.drop 1 i ))
                                                     (_) -> []

            (Seq b c)             -> let
                                            (Literal ch1) = b
                                            (Literal ch2) = c

                                            firstMatchSeq1 = matchLit ch1 i
                                            firstMatchSeq2 = matchLit ch2 (List.drop 1 i)

                                     in
                                            case i of
                                            [] -> []
                                            x::xs -> case (firstMatchSeq1,firstMatchSeq2) of
                                                     (Ok _ , Ok _) -> List.append [ch1,ch2] (list pattern ( List.drop 2 i ))
                                                     (_,_) -> []

            (_) -> i
    in
        case pattern of
        Literal a              -> let
                                      firstMatch = matchLit a input
                                  in
                                      case input of
                                      [] -> Err []
                                      x::xs -> case firstMatch of
                                               (Ok _ ) -> Ok (list pattern input, List.drop 1 input)
                                               (_) -> Ok ([],input)

        (Seq b c)         -> let
                                    (Literal ch1) = b
                                    (Literal ch2) = c

                                    firstMatchSeq1 = matchLit ch1 input
                                    firstMatchSeq2 = matchLit ch2 (List.drop 1 input)

                             in
                                    case input of
                                    [] -> Err []
                                    x::xs -> case (firstMatchSeq1,firstMatchSeq2) of
                                             (Ok _ , Ok _) -> Ok (list pattern input,List.drop 2 input)
                                             (_,_) -> Ok ([],input)

        (_) -> Err input


{-
  Tries to match one of `pat1` and `pat2`, in this order. If `pat1` matches, its result is returned, else
  `pat2` is tried.
  ```elm
  matchOneOf (Literal 'a') (Literal 'b') ['a', 'a', 'a'] == Ok (['a'], ['a', 'a'])
  matchOneOf (Literal 'b') (Literal 'a') ['a', 'a', 'a'] == Ok (['a'], ['a', 'a'])
  matchOneOf (Seq (Literal 'a') (Literal 'b')) (Seq (Literal 'c') (Literal 'd'))) ['c', 'd', 'a'] (Ok (['c', 'd'], ['a']))
  ```
-}
matchOneOf : RegexPattern -> RegexPattern -> List Char -> RegexResult
matchOneOf pat1 pat2 input =
        case (pat1,pat2) of
        (Literal a, Literal b) -> let
                                      firstMatch = matchLit a input
                                      secondMatch = matchLit b (List.drop 1 input)
                                   in
                                      case input of
                                      [] -> Err []
                                      x::xs -> case (firstMatch,secondMatch) of
                                               (Ok _ , _) -> Ok (a::[b], List.drop 2 input)
                                               (_ , Ok _) -> Ok (a::[b], List.drop 2 input)
                                               (_,_) -> Err input

        (Seq c d, Seq e f) -> let
                                    (Literal ch1) = c
                                    (Literal ch2) = d
                                    (Literal ch3) = e
                                    (Literal ch4) = f

                                    firstMatchSeq1 = matchLit ch1 input
                                    firstMatchSeq2 = matchLit ch2 (List.drop 1 input)
                                    secondMatchSeq1 = matchLit ch3 (List.drop 2 input)
                                    secondMatchSeq2 = matchLit ch4 (List.drop 3 input)
                             in
                                    case input of
                                    [] -> Err []
                                    x::xs -> case (firstMatchSeq1,secondMatchSeq1) of
                                             (Ok _ , _) -> case (firstMatchSeq2,secondMatchSeq2) of
                                                              (Ok _,_) -> Ok (ch1::[ch2,ch3,ch4],List.drop 4 input)
                                                              (_,_) -> Err input
                                             (_ , Ok _) -> case (firstMatchSeq2,secondMatchSeq2) of
                                                           (_,Ok _) -> Ok (ch1::[ch2,ch3,ch4],List.drop 4 input)
                                                           (_,_) -> Err input
                                             (_,_) -> Err input
        (Seq g h, Literal i) -> let

                                    (Literal ch5) = g
                                    (Literal ch6) = h

                                    firstMatchSeq3 = matchLit ch5 input
                                    firstMatchSeq4 = matchLit ch6 (List.drop 1 input)
                                    secondMatch1 = matchLit i (List.drop 2 input)
                                 in
                                    case input of
                                    [] -> Err []
                                    x::xs -> case (firstMatchSeq3,firstMatchSeq4, secondMatch1) of
                                             (Ok _ , _, _) -> Ok (ch5::[ch6,i],List.drop 3 input)
                                             (_ , Ok _, _) -> Ok (ch5::[ch6,i],List.drop 3 input)
                                             (_ , _, Ok _) -> Ok (ch5::[ch6,i],List.drop 3 input)
                                             (_,_,_) -> Err input
        (Literal j, Seq q l) -> let

                                    (Literal ch7) = q
                                    (Literal ch8) = l

                                    firstMatch1 = matchLit j input
                                    secondMatchSeq3 = matchLit ch7 (List.drop 1 input)
                                    secondMatchSeq4 = matchLit ch8 (List.drop 2 input)

                                in
                                    case input of
                                    [] -> Err []
                                    x::xs -> case (firstMatch1, secondMatchSeq3, secondMatchSeq4) of
                                             (Ok _ , _, _) -> Ok (j::[ch7,ch8],List.drop 3 input)
                                             (_ , Ok _, _) -> Ok (j::[ch7,ch8],List.drop 3 input)
                                             (_ , _, Ok _) -> Ok (j::[ch7,ch8],List.drop 3 input)
                                             (_,_,_) -> Err input
        (_,_) -> Err input"

match : RegexPattern -> List Char -> RegexResult
match pattern input =
  case pattern of
    Literal char -> matchLit char input
    Many pat -> matchMany pat input
    OneOf pat1 pat2 -> matchOneOf pat1 pat2 input
    Seq pat1 pat2 -> matchSeq pat1 pat2 input
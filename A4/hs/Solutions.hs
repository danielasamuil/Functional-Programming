-----------------------
-- Daniela Samuil
-- 30.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

import Data.List as L

strWords :: String -> [String]
strWords s =
   case s of
      "" -> []
      _ -> (takeWhile (/=' ') s) : strWords ( dropWhile (==' ') $ dropWhile (/=' ') s )

piglatinize :: String -> String
piglatinize s =
   let
      pigLatin :: String -> String
      pigLatin s = 
         if (take 1 s) == "a" || (take 1 s) == "e" || (take 1 s) == "i" || (take 1 s) == "o" || (take 1 s) == "u" || (take 1 s) == "y" then s ++ "-hay"
         else (drop 1 s) ++ "-" ++ (take 1 s) ++ "ay"         
   in
      case (length (strWords s)) of 
         0 -> ""
         1 -> (pigLatin $ head (strWords s))
         _ -> (pigLatin $ head (strWords s)) ++ " " ++ (piglatinize $ drop ((length $ head (strWords s)) + 1) s )

iter :: (a -> a) -> a -> [a]
iter f a = a : (iter f $ f a)

apprPi :: Double
apprPi = 
  let
    nextPi :: Double -> Double
    nextPi number = number + (2.0 * cos (number / 2.0)) / ((2.0 * sin (number / 2.0)) - 1.0)

    formTuple :: Int -> (Double,Double)
    formTuple i = ((iter nextPi 0) !! i ,(iter nextPi 0) !! (i+1))

    findEquals :: Int -> Double
    findEquals i = if fst (formTuple i) == snd (formTuple i) then fst (formTuple i)
                   else findEquals (i+1)
  in
    findEquals 0


-- Implement:

-- the update function for option 1

update :: (Eq k) => (v -> v) -> v -> k -> [(k, v)] -> [(k, v)]
update _ _ _ _ = error "Implement this function"

-- OR

-- the uniques, countOccurrences and countWords functions for option 2

uniques :: (Eq a) => [a] -> [a]
uniques l =
   let
      seeIfInList :: (Eq a) => a -> [a] -> [a]
      seeIfInList el list = if (elem el list) then [] else el : []
   in
     case l of
        [] -> []
        x:xs -> seeIfInList x xs ++ uniques xs

countOccurrences :: (Eq a) => a -> [a] -> Int
countOccurrences el list = case list of
                              [] -> 0
                              x:xs -> if el == x then 1 + countOccurrences el xs 
                                      else countOccurrences el xs

countWords :: String -> [(String, Int)]
countWords s =
   let
      formTuple :: String -> String -> (String, Int)
      formTuple oneElement string = (oneElement, countOccurrences oneElement (strWords string))

      makeStringFromList :: [String] -> String
      makeStringFromList l = case l of
                                [] -> ""
                                x:xs -> x ++ " " ++ makeStringFromList xs
   in
      case uniques (strWords s) of
         [] -> []
         x:xs -> (formTuple x s) : ( countWords $ makeStringFromList $ filter (/= x) (strWords s))

-- Implement topWords using the functions implemented above.

topWords :: Int -> String -> [(String, Int)]
topWords nr s =
   let
      makeIntegerList :: [(String, Int)] -> [Int]
      makeIntegerList list = case list of
                                [] -> []
                                x:xs -> (snd x) : makeIntegerList xs

      getWordWithNrOccurrences :: Int -> [String] -> [(String, Int)] -> [(String, Int)]
      getWordWithNrOccurrences i alreadyFound list = case list of
                                                        [] -> []
                                                        x:xs -> if (snd x) == i then case alreadyFound of
                                                                                     [] -> [(head ( (fst x) : alreadyFound), i)]
                                                                                     x1:xs1 -> if x1==(fst x) then getWordWithNrOccurrences i alreadyFound xs
                                                                                               else getWordWithNrOccurrences i xs1 xs
                                                                else getWordWithNrOccurrences i alreadyFound xs

      updateList :: Int -> [String] -> Int -> String-> [(String,Int)]
      updateList i alreadyFound nr s =  case nr of
                              0 -> []
                              _ -> getWordWithNrOccurrences (head $ (drop i (sort $ makeIntegerList $ countWords s))) alreadyFound (countWords s) ++ updateList (i-1) alreadyFound (nr-1) s
   in
      updateList ((length $ countWords s) -1) [] nr s
     
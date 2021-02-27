-----------------------
-- Daniela Samuil
-- 20.12.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Main where

import Control.Applicative

passwords :: [String]
passwords = sequenceA [ ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'], ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'], ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'], ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'], ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'], ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'], ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'], ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']]

data User = User {email :: Email, password :: Password} deriving (Show, Eq)

data Password = Password String deriving (Show, Eq)

data Email = Email {username :: String, domain:: String} deriving (Show, Eq)

validatePassword :: String -> Maybe Password
validatePassword s = case (length s) of
                        0 -> Nothing
                        1 -> Nothing
                        2 -> Nothing
                        3 -> Nothing
                        4 -> Nothing
                        5 -> Nothing
                        6 -> Nothing
                        7 -> Nothing
                        _ -> if ('0' `elem` s) || ('1' `elem` s) || ('2' `elem` s) || ('3' `elem` s) || ('4' `elem` s) || ('5' `elem` s) || ('6' `elem` s) || ('7' `elem` s) || ('8' `elem` s) || ('9' `elem` s) then
                                if ('a' `elem` s) || ('b' `elem` s) || ('c' `elem` s) || ('d' `elem` s) || ('e' `elem` s) || ('f' `elem` s) || ('g' `elem` s) || ('h' `elem` s) || ('i' `elem` s) || ('j' `elem` s) || ('k' `elem` s) || ('l' `elem` s) || ('m' `elem` s) || ('n' `elem` s) || ('o' `elem` s) || ('p' `elem` s) || ('q' `elem` s) || ('r' `elem` s) || ('s' `elem` s) || ('t' `elem` s) || ('u' `elem` s) || ('v' `elem` s) || ('w' `elem` s) || ('x' `elem` s) || ('y' `elem` s) || ('z' `elem` s) then
                                   if ('A' `elem` s) || ('B' `elem` s) || ('C' `elem` s) || ('D' `elem` s) || ('E' `elem` s) || ('F' `elem` s) || ('G' `elem` s) || ('H' `elem` s) || ('I' `elem` s) || ('J' `elem` s) || ('K' `elem` s) || ('L' `elem` s) || ('M' `elem` s) || ('N' `elem` s) || ('O' `elem` s) || ('P' `elem` s) || ('Q' `elem` s) || ('R' `elem` s) || ('S' `elem` s) || ('T' `elem` s) || ('U' `elem` s) || ('V' `elem` s) || ('W' `elem` s) || ('X' `elem` s) || ('Y' `elem` s) || ('Z' `elem` s) then
                                      Just (Password s)
                                   else Nothing
                                else Nothing
                             else Nothing

validateEmail :: String -> Maybe Email
validateEmail s = let
                     email :: String -> String -> Email
                     email username1 domain1 = Email username1 domain1
                  in
                     case (length s) of
                     0 -> Nothing
                     1 -> Nothing
                     2 -> Nothing
                     3 -> Nothing
                     4 -> Nothing
                     5 -> Nothing
                     6 -> Nothing
                     7 -> Nothing
                     8 -> Nothing
                     _ -> if ('@' `elem` s) then
                             if (length (fst (break (=='@') s)) > 2) then
                                if ('.' `elem` (snd (break (=='@') s))) then
                                   if (length (fst (break (=='.') (snd (break (=='@') s)))) > 1) then
                                      if (drop ((length s) -4) s == ".com") then
                                         if ('@' `elem` (drop 1(snd (break (=='@') s)))) then Nothing else
                                            Just (email (fst (break (=='@') s)) (drop 1 (snd (break (=='@') s))))
                                      else Nothing
                                   else Nothing
                                else Nothing
                             else Nothing
                          else Nothing

validateUser :: String -> String -> Maybe User
validateUser email password = let
                                 user :: Email -> Password -> User
                                 user email1 password1 = User email1 password1
 
                                 email1 :: String -> String -> Email
                                 email1 username1 domain1 = Email username1 domain1								 
                              in
                                 case (validateEmail email) of
                                    Nothing -> Nothing
                                    Just _ -> case (validatePassword password) of
                                                 Nothing -> Nothing
                                                 Just _ -> Just (user (fromMaybe (email1 "" "") (validateEmail email)) (fromMaybe (Password "") (validatePassword password)))

main :: IO ()
main = do
putStrLn "Please enter your email:"
email <- getLine
putStrLn "Please enter your password:"
password <- getLine
case (validateUser email password) of
   Nothing -> putStrLn "Ups, email or password are incorect"
   Just _ -> putStrLn "Welcome"
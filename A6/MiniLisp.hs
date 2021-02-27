module MiniLisp where

import Parser

data LispAtom = Number Int | Symbol String deriving (Eq, Show)
data LispValue = List [LispValue] | Atom LispAtom deriving (Eq, Show)

ws :: Parser String
ws = (many (char ' ')) `orElse` (many (char '\n'))

between :: Parser a -> Parser b -> Parser c -> Parser c
between pHd pTl p = pMap fst (pThen pHd ( p `andThen` pTl )) 

ident :: Parser String
ident = pMap (\(first,second) -> [first] ++ second) ((lower `orElse` upper) `andThen` many (lower `orElse` upper `orElse` digit `orElse` (satisfies (=='?')))) 

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p = many (p `orElse` (pMap snd (sep `andThen` p)))

lisp :: Parser LispValue
lisp = error "TODO"

lispAtom :: Parser LispAtom
lispAtom = (pMap (\x -> Number x) number) `orElse` (pMap (\x -> Symbol x) (some (lower `orElse` upper)))

lispList :: Parser [LispValue] 
lispList = error "TODO"
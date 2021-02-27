-----------------------
-- Samuil Daniela Teodora
-- 28.10.2020
-----------------------

module FunSet exposing (..)

type alias FunSet = Int -> Bool

contains : FunSet -> Int -> Bool
contains set elem = set elem

singletonSet : Int -> FunSet
singletonSet elem = \inputElem -> elem == inputElem

{-
Conveniece function to create a set of elements.
```elm
setOf [1, 2, 3] == union (union (singletonSet 1) (singletonSet 2)) (singletonSet 3))
setOf [1, 2, 3] == fold union [(singletonSet 1), (singletonSet 2), (singletonSet 3)]
```
-}
setOf : List Int -> FunSet
setOf elems = \x -> List.any (\y -> y == x) elems

{-
Returns the union of 2 sets.
```elm
(union (singletonSet 1) (singletonSet 2)) 1 == True
(union (singletonSet 1) (singletonSet 2)) 2 == True
(union (setOf [1, 3, 4]) (setOf [1, 2])) 2 == True
(union (setOf [1, 3, 4]) (setOf [1, 2])) 5 == False
```
-}
union : FunSet -> FunSet -> FunSet
union a b = (\x -> contains a x || contains b x)


{-
Returns the intersection of 2 sets.
```elm
(intersect (setOf [1, 2]) (setOf [1, 3])) 1 == True
(intersect (setOf [1, 2]) (setOf [1, 3])) 2 == False
```
-}
intersect : FunSet -> FunSet -> FunSet
intersect a b = (\x -> contains a x && contains b x)

{-
Returns the difference of 2 sets.
```elm
(diff (setOf [1, 2]) (setOf [1, 3])) 1 == False
(diff (setOf [1, 2]) (setOf [1, 3])) 2 == True
```
-}
diff : FunSet -> FunSet -> FunSet
diff a b = (\x -> contains a x && not (contains b x))

{-
Returns a new set, with `function` applied to each of element. 
You can assume that elements are integres in the range [-1000, 1000].
```elm
(map (\x -> x + 1) (setOf [1, 2]) 1 == False
(map (\x -> x + 1) (setOf [1, 2]) 2 == True
(map (\x -> x + 1) (setOf [1, 2]) 3 == True
```
-}
map: ( Int -> Int ) -> FunSet -> FunSet
map function set =
    let
        mapHelper i list=
            if i<=1000
            then
            if (contains set i)
            then i::list else mapHelper (i+1) list
            else list

        mapHelper2: (Int->Int) -> List Int -> List Int
        mapHelper2 funct l= List.map funct l
    in
       setOf (mapHelper2 function (mapHelper -1001 []))

{-
Takes a list of sets and returns a new set, which is build by applying a fold using `operation` function.
```elm
(fold union [(singletonSet 1), (singletonSet 2), (singletonSet 3)]) 1 == True
(fold intersection [setOf [1], setOf [2]]) 1 == False
(fold intersection [setOf [1], setOf [2]]) 2 == False
(fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 1 == False
(fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 2 == False
(fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 3 == True
```
-}
fold: ( FunSet -> FunSet -> FunSet ) -> List FunSet -> FunSet
fold operation sets =
       case sets of
       [] -> singletonSet -1001
       x::xs -> List.foldl operation x xs
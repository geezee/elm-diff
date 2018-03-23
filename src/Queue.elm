module Queue exposing (..)

import Set exposing (Set)

type alias Value = (Int, Int)
type alias Queue = Set Value

empty : Queue
empty = Set.empty

push : Value -> Queue -> Queue
push value queue =
    Set.insert value queue

pushIf : Bool -> (() -> Value) -> Queue -> Queue
pushIf cond lazyValue queue =
    if cond then push (lazyValue ()) queue else queue

size : Queue -> Int
size = Set.size

isEmpty : Queue -> Bool
isEmpty q = Set.isEmpty q

fold : (Value -> b -> b) -> b -> Queue -> b
fold f b q = List.foldr f b (Set.toList q)

map : (Value -> a) -> Queue -> List a
map m q = List.map m (Set.toList q)

setAppend : Set comparable -> Set comparable -> Set comparable
setAppend set1 set2 =
    if ((Set.size set1) > (Set.size set2)) then
        setAppend set2 set1
    else
        List.foldr Set.insert set2 (Set.toList set1)

flatten : List Queue -> Queue
flatten lst = List.foldr setAppend Set.empty lst

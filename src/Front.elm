module Front exposing (..)

import Set exposing (Set)
import Dict exposing (Dict)

type alias Value = (Int, Int)

type alias Front =
    { entries : Set Value
    , parentMap : Dict Value Value }


empty : Front
empty = Front Set.empty Dict.empty


contains : Value -> Front -> Bool
contains value front =
    (Set.member value front.entries)


push : Value -> Front -> Front
push value front =
    if (contains value front) then front
    else Front (Set.insert value front.entries) front.parentMap


pushIf : Bool -> (() -> Value) -> Value -> Front -> Front
pushIf cond lazyValue parent front =
    let value = lazyValue () in
    if cond then push value front |> setParent value parent else front


setParent : Value -> Value -> Front -> Front
setParent value parent front =
    Front front.entries (Dict.insert value parent front.parentMap)


getParent : Value -> Front -> Maybe Value
getParent value front =
    Dict.get value front.parentMap


size : Front -> Int
size front =
    Set.size front.entries


isEmpty : Front -> Bool
isEmpty front =
    Set.isEmpty front.entries


mapList : (Value -> a) -> Front -> List a
mapList func front =
    List.map func (front.entries |> Set.toList)


-- BEWARE merge is not symmetric, there is a priority for the first front
-- on the choice of the parents
merge : Front -> Front -> Front
merge front1 front2 =
    let parentInsert = \key -> \value -> \front -> Dict.insert key value front in
    Front (Set.union front1.entries front2.entries)
          (Dict.merge
              parentInsert
              (\k -> \lv -> \rv -> \front -> Dict.insert k lv front)
              parentInsert
              front1.parentMap
              front2.parentMap
              Dict.empty)


flatten : List Front -> Front
flatten fronts =
    List.foldr merge empty fronts

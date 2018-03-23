module Diff exposing (..)

import Queue exposing (..)


commonPrefix : String -> String -> String
commonPrefix str1 str2 =
    case (str1, str2) of
        ("", _) -> ""
        (_, "") -> ""
        _ -> let (x, xs) = String.uncons str1 |> Maybe.withDefault ('%', "") in
             let (y, ys) = String.uncons str2 |> Maybe.withDefault ('%', "") in
             if x == y then (String.fromChar x) ++ (commonPrefix xs ys) else ""


followDiagonal : String -> String -> Value -> Value
followDiagonal str1 str2 value =
    let (i, j) = value in
    let prefix = commonPrefix (String.dropLeft i str1) (String.dropLeft j str2) in
    let len = String.length prefix in
    if (len == 0) then value else (i + len, j + len)


insideStrBounds : String -> String -> Value -> Bool
insideStrBounds str1 str2 value =
    let (i, j) = value in
    (i < String.length str1) && (j < String.length str2)


pushValueAlong : String -> String -> Value -> Int -> (Queue -> Queue)
pushValueAlong src trgt value axis =
    let (i, j) = value in
    let neighbor = (i + 1 * (1 - axis), j + 1 * axis) in
    pushIf (insideStrBounds src trgt neighbor)
           (\() -> followDiagonal src trgt neighbor)


neighbors : String -> String -> Value -> Queue
neighbors src trgt value =
    if ((String.isEmpty src) || (String.isEmpty trgt)) then
        empty
    else
        let (x,xs) = String.uncons src |> Maybe.withDefault (' ', "") in
        let (y,ys) = String.uncons trgt |> Maybe.withDefault (' ', "") in
        let add = pushValueAlong src trgt value in
        if (x == y) then
            empty |> push (followDiagonal src trgt value)
        else
            empty |> add 0 |> add 1



advanceStep : String -> String -> Queue -> Queue
advanceStep src trgt queue =
    map (neighbors src trgt) queue |> flatten

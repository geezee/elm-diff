module Diff exposing (..)

import Front exposing (..)


commonPrefix : String -> String -> String
commonPrefix str1 str2 =
    case (str1, str2) of
        ("", _) -> ""
        (_, "") -> ""
        _ -> let (x, xs) = String.uncons str1 |> Maybe.withDefault ('%', "") in
             let (y, ys) = String.uncons str2 |> Maybe.withDefault ('%', "") in
             if x == y then (String.fromChar x) ++ (commonPrefix xs ys) else ""


followDiagonal : String -> String -> Value -> Value
followDiagonal str1 str2 start =
    let (i, j) = start in
    let prefix = commonPrefix (String.dropLeft i str1) (String.dropLeft j str2) in
    let len = String.length prefix in
    if (len == 0) then start else (i + len, j + len)


insideStrBounds : String -> String -> Value -> Bool
insideStrBounds str1 str2 value =
    let (i, j) = value in
       (i < String.length str1) && (j < String.length str2)
    || (i == String.length str1) && (j == String.length str2)


pushValueAlong : String -> String -> Value -> Int -> Front -> Front
pushValueAlong src trgt value axis front =
    let (i, j) = value in
    let neighbor = (i + 1 * (1 - axis), j + 1 * axis) in
    pushIf (insideStrBounds src trgt neighbor)
           (\() -> followDiagonal src trgt neighbor)
           value
           front


neighbors : String -> String -> Value -> Front
neighbors src trgt value =
    if ((String.isEmpty src) || (String.isEmpty trgt)) then
        empty
    else
        let (x,xs) = String.uncons src |> Maybe.withDefault (' ', "") in
        let (y,ys) = String.uncons trgt |> Maybe.withDefault (' ', "") in
        let add = pushValueAlong src trgt value in
        if (x == y) then
            let diagonal = followDiagonal src trgt value in
            empty |> push diagonal |> setParent diagonal value
        else
            empty |> add 0 |> add 1



reachedEnd : Front -> String -> String -> Bool
reachedEnd front str1 str2 =
    contains (String.length str1, String.length str2) front   


advanceStep : String -> String -> Front -> Front
advanceStep src trgt front =
    let new = mapList (neighbors src trgt) front |> flatten in
    Front new.entries (merge front new |> .parentMap)


getEndValue : String -> String -> Value
getEndValue str1 str2 =
    (String.length str1, String.length str2)


getPath : Value -> Front -> List Value
getPath endValue front =
    case getParent endValue front of
        Nothing -> [ endValue ]
        Just parent -> endValue :: (getPath parent front)

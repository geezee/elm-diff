module Diff exposing (..)

import Front exposing (..)
import Json.Encode
import Json.Decode

type Edit = Insert Int String | Remove Int String

jsonEncodeES : Edit -> Json.Encode.Value
jsonEncodeES es =
    let obj = \t -> \i -> \s ->
        Json.Encode.list [ Json.Encode.int 0, Json.Encode.int i, Json.Encode.string s ]
    in
    case es of
        Insert index str -> obj 0 index str
        Remove index str -> obj 1 index str


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


getEndValue : String -> String -> Value
getEndValue str1 str2 =
    (String.length str1, String.length str2)


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



reachedEnd : String -> String -> Front -> Bool
reachedEnd str1 str2 front =
    contains (getEndValue str1 str2) front


advanceStep : String -> String -> Front -> Front
advanceStep src trgt front =
    let new = mapList (neighbors src trgt) front |> flatten in
    Front new.entries (merge front new |> .parentMap)


getPath : Value -> Front -> List Value
getPath endValue front =
    case getParent endValue front of
        Nothing -> [ endValue ]
        Just parent -> endValue :: (getPath parent front)


computeFinalFrontTimeout : Int -> String -> String -> Front
computeFinalFrontTimeout maxLoop source target =
    let endCriteria = reachedEnd source target in
    let advancer = advanceStep source target in
    let compute = \i -> \front ->
        if (endCriteria front || i >= maxLoop) then front
        else compute (i+1) (advancer front) in
    compute 0 (empty |> push (0, 0))


computeFinalFront : String -> String -> Front
computeFinalFront source target =
    computeFinalFrontTimeout ((String.length source) + (String.length target)) source target


editLength : String -> String -> Int
editLength str1 str2 =
    let front = computeFinalFront str1 str2 in
    List.length (getPath (getEndValue str1 str2) front)


commonSubsequence : String -> String -> String
commonSubsequence source target =
    let front = computeFinalFront source target in
    let path = getPath (getEndValue source target) front |> List.reverse in
    source


getEditScript : String -> String -> List Edit
getEditScript source target =
    let front = computeFinalFront source target in
    let path = getPath (getEndValue source target) front in
    []


diff : String -> String -> String
diff source target =
    let editScript = getEditScript source target in
    Json.Encode.encode 4 (Json.Encode.list (List.map jsonEncodeES editScript))

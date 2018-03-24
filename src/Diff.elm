module Diff exposing (..)

import Front exposing (..)
import Json.Encode
import Json.Decode

type TransitionType = TInsert Int | TRemove Int | TNoChange Int
type Edit = Insert Int String | Remove Int Int

type alias Path = List Value
type alias TransitionSequence = List TransitionType
type alias Diff = List Edit


jsonEncodeES : Edit -> Json.Encode.Value
jsonEncodeES es =
    let l = Json.Encode.list in
    let int = Json.Encode.int in
    let str = Json.Encode.string in
    case es of
        Insert index payload -> l [ int index, str payload ]
        Remove index num -> l [ int index, int num ]



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


getPathRev : Value -> Front -> Path
getPathRev endValue front =
    case getParent endValue front of
        Nothing -> [ endValue ]
        Just parent -> endValue :: (getPathRev parent front)


getPath : Value -> Front -> Path
getPath v f =
    getPathRev v f |> List.reverse


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


findTransSeqRev : Value -> Value -> TransitionSequence
findTransSeqRev start end =
    case (start, end) of
        ((xs, ys), (xe, ye)) ->
            -- base case: there's only one insert, one delete, or one nochange
            if ( ((abs (xs - xe)) <= 1) && ((abs (ys - ye)) <= 1) ) then
                if (xs == xe && ys == ye) then [ ]
                else if ( xs == xe ) then [ TInsert 1 ]
                else if ( ys == ye ) then [ TRemove 1 ]
                else [ TNoChange 1 ]
            -- recurse: there's a diagonal to follow to the base case
            else
                let transSeq = findTransSeqRev (xs+1,ys+1) (xe, ye) in
                case transSeq of
                    [ ] -> [ TNoChange 1 ]
                    TNoChange l :: rest -> TNoChange (l+1) :: rest
                    _ -> TNoChange 1 :: transSeq


findTransSeq : Value -> Value -> TransitionSequence
findTransSeq start end =
    findTransSeqRev start end |> List.reverse



squashPath : Path -> TransitionSequence
squashPath path =
    case path of
        [ ] -> [ ]
        start :: [ ] -> [ ]
        start :: (end :: rest) ->
            let seq = findTransSeq start end in
            let subpath = squashPath (end::rest) in
            case (seq, subpath) of
                (TInsert i::[], TInsert j::subsubpath) ->
                    TInsert (i+j) :: subsubpath
                (TRemove i::[], TRemove j::subsubpath) ->
                    TRemove (i+j) :: subsubpath
                _ ->
                    List.append seq (squashPath (end::rest))



diffToWithIndex : Int -> String -> TransitionSequence -> Diff
diffToWithIndex i target seq =
    case seq of
        [ ] -> [ ]
        transition :: rest ->
        case transition of
            TNoChange n -> diffToWithIndex (i+n) target rest
            TInsert n -> 
                let str = String.slice i (i+n) target in
                    Insert i str :: diffToWithIndex (i+n) target rest
            TRemove n -> Remove i n :: diffToWithIndex i target rest



diffTo : String -> TransitionSequence -> Diff
diffTo =
    diffToWithIndex 0


serializeDiff : Diff -> String
serializeDiff diff =
    List.map (\edit -> jsonEncodeES edit) diff
    |> Json.Encode.list
    |> Json.Encode.encode 0

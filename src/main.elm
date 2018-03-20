import Html
import List
import String
import Maybe exposing (withDefault)

type ScriptEdit = Insert Int Char | Delete Int Char
type alias Diff = List ScriptEdit

scriptEditToString : ScriptEdit -> String
scriptEditToString se =
    case se of
        Insert i c -> "Insert " ++ (toString c) ++ " at " ++ (toString i)
        Delete i c -> "Delete " ++ (toString c) ++ " at " ++ (toString i)

getIndex se =
    case se of
        Insert i c -> i
        Delete i c -> i

setIndex m se =
    case se of
        Insert i c -> Insert m c
        Delete i c -> Delete m c

incrementIndex se = setIndex ((getIndex se) + 1) se
decrementIndex se = setIndex ((getIndex se) - 1) se

isInsert se =
    case se of
        Insert _ _ -> True
        Delete _ _ -> False

isDelete se = not (isInsert se)

min2list : List a -> List a -> List a
min2list l1 l2 =
    if ((List.length l1) <= (List.length l2)) then l1 else l2

strScriptEdit : String -> Bool -> Diff
strScriptEdit str insert =
    let mapper = if insert then List.map (Insert 0) else List.map (Delete 0) in
    let reducer = List.foldl (\toFix -> \fixed ->
        case fixed of
            [] -> [ toFix ]
            top::_ -> (setIndex ((getIndex top) + 1) toFix) :: fixed) [] in
    reducer (mapper (String.toList str)) |> List.reverse

condMap : (ScriptEdit -> Bool) -> (ScriptEdit -> ScriptEdit) -> List ScriptEdit -> List ScriptEdit
condMap cond map list =
    List.map (\se -> if (cond se) then map se else se) list

incrementEdits : (ScriptEdit -> Bool) -> List ScriptEdit -> List ScriptEdit
incrementEdits cond list =
    condMap cond incrementIndex list

decrementEdits : (ScriptEdit -> Bool) -> List ScriptEdit -> List ScriptEdit
decrementEdits cond list =
    condMap cond decrementIndex list

diff : String -> String -> Diff
diff sourceStr targetStr =
    let get_diff = \sourceStr -> \targetStr ->
        let (x, xs) = String.uncons sourceStr |> withDefault (' ', "") in
        let (y, ys) = String.uncons targetStr |> withDefault (' ', "") in
        let isDiagonal = x == y in
        let deletes = (\() -> 
            (Delete 0 x) :: incrementEdits isDelete (diff xs targetStr)) in
        let inserts = (\() ->
            (Insert 0 y) :: incrementEdits isInsert (diff sourceStr ys)) in
        case isDiagonal of
            True -> incrementEdits (\se -> True) (diff xs ys)
            False -> min2list (deletes ()) (inserts ())
    in
    case (sourceStr, targetStr) of
        ("", "") -> [ ]
        ("", _) -> strScriptEdit targetStr True
        (_, "") -> strScriptEdit sourceStr False
        (_, _) -> get_diff sourceStr targetStr


transform : Diff -> String -> String
transform diff str =
    let (x, xs) = String.uncons str |> withDefault ('$', "") in
    let sortedDiff = List.sortBy getIndex diff in
    case sortedDiff of
        [] -> str
        (Insert 0 c)::rest -> (String.cons c (transform (decrementEdits isInsert rest) str))
        (Delete 0 c)::rest -> transform (decrementEdits isDelete rest) xs
        _ -> if str == "" then "" else
            String.cons x (transform (decrementEdits (\se -> True) diff) xs)

str1 = "Hello world"
str2 = "HelL0, worLd!"
diffedResult = diff str1 str2

main = Html.div [] [
    Html.pre [] [ Html.text str1 ],
    Html.pre [] [ Html.text str2 ],
    Html.pre [] [ Html.text (List.foldr
        (\acc -> \str -> (scriptEditToString acc) ++ "\n" ++ str) "" (List.sortBy getIndex diffedResult)) ],
    Html.pre [] [ Html.text (transform diffedResult str1) ] ]

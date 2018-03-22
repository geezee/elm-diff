import Html
import List
import String
import Maybe exposing (withDefault)
import Dict exposing (Dict)



type ScriptEdit = Insert Int String | Delete Int String | Replace Int String
type alias Diff = List ScriptEdit



scriptEditToString : ScriptEdit -> String
scriptEditToString se =
    case se of
        Insert index str -> ">>@" ++ (toString index) ++ " " ++ str
        Delete index str -> "<<@" ++ (toString index) ++ " " ++ str
        Replace index str -> "<>@" ++ (toString index) ++ " " ++ str

getIndex se =
    case se of
        Insert i str -> i
        Delete i str -> i
        Replace i str -> i

setIndex m se =
    case se of
        Insert i str -> Insert m str
        Delete i str -> Delete m str
        Replace i str -> Replace m str

mapIndex map se =
    setIndex (map (getIndex se)) se

incrementIndex n = mapIndex (\i -> i + n)
decrementIndex n = mapIndex (\i -> i - n)

isInsert se =
    case se of
        Insert _ _ -> True
        Delete _ _ -> False
        Replace _ _ -> False

isDelete se =
    case se of
        Insert _ _ -> False
        Delete _ _ -> True
        Replace _ _ -> False

isReplace se =
    case se of
        Insert _ _ -> False
        Delete _ _ -> False
        Replace _ _ -> True



commonPrefix : String -> String -> String
commonPrefix str1 str2 =
    case (str1, str2) of
        ("", _) -> ""
        (_, "") -> ""
        _ -> let (x, xs) = String.uncons str1 |> Maybe.withDefault ('%', "") in
             let (y, ys) = String.uncons str2 |> Maybe.withDefault ('%', "") in
             if x == y then (toString x) ++ (commonPrefix xs ys) else ""



min2list : List a -> List a -> List a
min2list l1 l2 =
    if ((List.length l1) <= (List.length l2)) then l1 else l2




condMapSe : (ScriptEdit -> Bool) -> (ScriptEdit -> ScriptEdit) -> List ScriptEdit -> List ScriptEdit
condMapSe cond map list =
    List.map (\se -> if (cond se) then map se else se) list



incrementEdits : Int -> (ScriptEdit -> Bool) -> List ScriptEdit -> List ScriptEdit
incrementEdits n cond =
    condMapSe cond (incrementIndex n)



decrementEdits : Int -> (ScriptEdit -> Bool) -> List ScriptEdit -> List ScriptEdit
decrementEdits n cond =
    condMapSe cond (decrementIndex n)


splitAtEqual : String -> String -> ((String, String), (String, String))
splitAtEqual str1 str2 =
    if (String.isEmpty str1) then (("", ""), ("", str2))
    else if (String.isEmpty str2) then (("", str1), ("", ""))
    else
        let (x, xs) = String.uncons str1 |> withDefault ('%', "") in
        let (y, ys) = String.uncons str2 |> withDefault ('%', "") in
        if x == y then
            let ((xp, xps), (yp, yps)) = splitAtEqual xs ys in
            (((String.cons x xp), xps), ((String.cons y yp), yps))
        else
            (("", str1), ("", str2))



type alias Node = {
    id : Int,
    score : Float,
    value : (Char, Char),
    parent : Int,
    visited : Bool
}

type alias Path = List Node
type alias PriorityQueue =
    { indexMap : Dict Int Int
    , nodes : List Node }


type alias NodeCounter =
    { count : Int
    , value : Node }

newCounter : Node -> NodeCounter
newCounter value =
    { count = 0
    , value = value }


makeNode : Float -> (Char, Char) -> NodeCounter -> NodeCounter
makeNode score value counter =
    { count = counter.count + 1
    , value =
        { score = score
        , value = value
        , id = counter.count + 1
        , parent = 0
        , visited = False }
    }


updateParent : Node -> Node -> Node
updateParent parent node =
    {node | parent = parent.id }


markVisited : Node -> Node
markVisited node =
    {node | visited = True}


emptyPriorityQueue : PriorityQueue
emptyPriorityQueue =
    { indexMap = Dict.empty, nodes = [ ] }


removeQueue : Node -> PriorityQueue -> PriorityQueue
removeQueue node queue =
    if not (Dict.member node.id queue.indexMap) then queue
    else
        let i = Dict.get node.id queue.indexMap |> withDefault -1 in
        {queue | nodes = (List.take i queue.nodes) ++ (List.drop (i+1) queue.nodes) }


getQueue : Int -> PriorityQueue -> Maybe Node
getQueue id queue =
    if not (Dict.member id queue.indexMap) then Nothing
    else
        let i = Dict.get id queue.indexMap |> withDefault 0 in
        List.take (i+1) queue.nodes |> List.head

pushQueue : Node -> PriorityQueue -> PriorityQueue
pushQueue node queue =
    if (Dict.member node.id queue.indexMap) then
        removeQueue node queue |> pushQueue node
    else
        let (smaller, bigger) = List.partition (\x -> x.score < node.score) queue.nodes in
        { indexMap = Dict.insert node.id (List.length smaller) queue.indexMap
        , nodes = List.append smaller (node :: bigger) }



popQueue : PriorityQueue -> Maybe (Node, PriorityQueue)
popQueue queue =
    case List.head queue.nodes of
        Nothing -> Nothing
        Just head -> Just (head,
            { indexMap = Dict.remove head.id queue.indexMap
            , nodes = List.tail queue.nodes |> withDefault [] })


tentativePushQueue : Node -> PriorityQueue -> PriorityQueue
tentativePushQueue node queue =
    if not (Dict.member node.id queue.indexMap) then
        pushQueue node queue
    else
        let oldscore = getQueue node.id queue |> withDefault node |> .score in
        if (oldscore < node.score) then queue
        else pushQueue node queue


mergeQueues : PriorityQueue -> PriorityQueue -> PriorityQueue
mergeQueues queue1 queue2 =
    if ((List.length queue1.nodes) < (List.length queue2.nodes))
        then mergeQueues queue2 queue1
    else
        List.foldr tentativePushQueue queue2 queue1.nodes


-- TODO
getNeighbors : Node -> List Node
getNeighbors node =
    []

-- TODO
atTarget : PriorityQueue -> Bool
atTarget queue =
    False


heuristic : Node -> Float
heuristic node =
    0

exploreStep : PriorityQueue -> PriorityQueue
exploreStep queue =
    case popQueue queue of
        Nothing -> emptyPriorityQueue
        Just (top, rest) ->
            let neighborsQueue = List.foldr (\neighbor -> \queue -> 
                queue |> pushQueue
                    { neighbor
                    | score = top.score + neighbor.score + (heuristic neighbor)
                    , parent = top.id }) emptyPriorityQueue (getNeighbors top)  in
            mergeQueues neighborsQueue rest |> pushQueue (markVisited top)


diff : String -> String -> Diff
diff sourceStr targetStr =
    if (String.isEmpty sourceStr) then [ Insert 0 targetStr ]
    else if (String.isEmpty targetStr) then [ Delete 0 sourceStr ]
    else
        let prefix = commonPrefix sourceStr targetStr in
        -- we can skip ahead!
        if not (String.isEmpty prefix) then
            incrementEdits (String.length prefix) (\se -> True) (diff xs ys)
        -- need to fork
        else []




-- transform : Diff -> String -> String
-- transform diff str =
--     let (x, xs) = String.uncons str |> withDefault ('$', "") in
--     let sortedDiff = List.sortBy getIndex diff in
--     case sortedDiff of
--         [] -> str
--         (Insert 0 c)::rest -> (String.cons c (transform (decrementEdits isInsert rest) str))
--         (Delete 0 c)::rest -> transform (decrementEdits isDelete rest) xs
--         _ -> if str == "" then "" else
--             String.cons x (transform (decrementEdits (\se -> True) diff) xs)




str1 = "Hello world"
str2 = "HelL0, worLd!"
diffedResult = commonPrefix str1 str2
((x, xs), (y, ys)) = splitAtEqual str1 str2

main = Html.div [] [
      Html.pre [] [ Html.text x ]
    , Html.pre [] [ Html.text xs ]
    , Html.pre [] [ Html.text y ]
    , Html.pre [] [ Html.text ys ]
    ]

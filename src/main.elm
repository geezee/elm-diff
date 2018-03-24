import Html

import Front
import Diff

pre str = Html.pre [] [ Html.text str ]

str1 = "ABCABBA"
str2 = "CBABAC"

main =
    let front = Diff.computeFinalFront str1 str2 in
    let path = Diff.getPath (Diff.getEndValue str1 str2) front in
    let squashed = Diff.squashPath path in
    let diff = Diff.diffTo str2 squashed in
    Html.div []
        [ pre (str1 ++ " -> " ++ str2)
        , pre ("path = " ++ (path |> toString))
        , pre ("squash path = " ++ (squashed |> toString))
        , pre ("diff to = " ++ (diff |> toString))
        , pre ("serialized = " ++ (Diff.serializeDiff diff))
        ]

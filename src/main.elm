import Html

import Front
import Diff

pre str = Html.pre [] [ Html.text str ]

str1 = "ABCABBA"
str2 = "CBABAC"

main =
    let diff = Diff.computeFinalFront str1 str2 in
    Html.div []
        [ pre ("last front = " ++ (diff |> toString))
        , pre ("path = " ++ (diff |> Diff.getPath (Diff.getEndValue str1 str2) |> toString))
        , pre ("edit length = " ++ (Diff.editLength str1 str2 |> toString))
        , pre ("common subsequence = " ++ (Diff.commonSubsequence str1 str2))
        , pre ("diff = " ++ (Diff.diff str1 str2))
        ]

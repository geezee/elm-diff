import Html

import Front
import Diff

pre str = Html.pre [] [ Html.text str ]

startFront = Front.empty |> Front.push (0, 0)

str2 = "ABCABBA"
str1 = "CBABAC"

main = Html.div []
    [
      pre ("After 1 step:  " ++ (startFront |> Diff.advanceStep str1 str2 |> toString))
    , pre ("After 2 steps: " ++ (startFront |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> toString))
    , pre ("After 3 steps: " ++ (startFront |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> Diff.advanceStep str1 str2 |> toString))
    , pre ("After 4 steps: " ++ (startFront |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> toString))
    , pre ("After 5 steps: " ++ (startFront |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> Diff.advanceStep str1 str2 |> toString))
    , pre ("After 6 steps: " ++ (startFront |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> toString))
    , pre ("Path: " ++ (startFront |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> Diff.advanceStep str1 str2
            |> Diff.getPath (Diff.getEndValue str1 str2) |> toString))
    ]

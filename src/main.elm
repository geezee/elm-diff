import Html

import Queue
import Diff

pre str = Html.pre [] [ Html.text str ]

startQueue = Queue.empty |> Queue.push (0, 0)

str2 = "ABCABBA"
str1 = "CBABAC"

main = Html.div []
    [
      pre ("After 1 step:  " ++ (startQueue |> Diff.advanceStep str1 str2 |> toString))
    , pre ("After 2 steps: " ++ (startQueue |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> toString))
    , pre ("After 3 steps: " ++ (startQueue |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> Diff.advanceStep str1 str2 |> toString))
    , pre ("After 4 steps: " ++ (startQueue |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> toString))
    , pre ("After 5 steps: " ++ (startQueue |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> Diff.advanceStep str1 str2 |> toString))
    , pre ("After 6 steps: " ++ (startQueue |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> Diff.advanceStep str1 str2
            |> Diff.advanceStep str1 str2 |> toString))
    ]

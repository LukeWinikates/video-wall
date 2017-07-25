module DomHelpers exposing (px, snap)


px : a -> String
px =
    toString >> (flip (++) "px")


snap : Int -> Int
snap value =
    (round (toFloat value / 10)) * 10

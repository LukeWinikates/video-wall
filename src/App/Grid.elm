module App.Grid exposing (videoBorderWidth, px, snap)


videoBorderWidth =
    10


px : a -> String
px =
    toString >> (flip (++) "px")


snap : Int -> Int
snap value =
    (round (toFloat value / 10)) * 10

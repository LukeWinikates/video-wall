module DomHelpers exposing (px)


px : a -> String
px =
    toString >> (flip (++) "px")

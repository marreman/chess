module Util exposing (..)

import Dict exposing (Dict)


toDict : List comparable -> Dict comparable Bool
toDict list =
    Dict.fromList <|
        List.map (\value -> ( value, True )) list


(=>) : a -> b -> ( a, b )
(=>) x y =
    ( x, y )
infixr 9 =>

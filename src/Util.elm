module Util exposing (..)

import Dict


toDict list =
    Dict.fromList <|
        List.map (\value -> ( value, True )) list


(=>) x y =
    ( x, y )
infixr 9 =>

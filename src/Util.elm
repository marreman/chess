module Util exposing (..)

import Dict exposing (Dict)


toDict : List comparable -> Dict comparable Bool
toDict list =
    Dict.fromList <|
        List.map (\value -> ( value, True )) list


sum : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
sum ( x, y ) ( rx, ry ) =
    ( rx + x, ry + y )


path : List ( Int, Int ) -> ( Int, Int )
path list =
    List.foldl sum ( 0, 0 ) list


north : ( Int, Int )
north =
    ( -1, 0 )


northWest : ( Int, Int )
northWest =
    ( -1, -1 )


northEast : ( Int, Int )
northEast =
    ( -1, 1 )


south : ( Int, Int )
south =
    ( 1, 0 )


southWest : ( Int, Int )
southWest =
    ( 1, -1 )


southEast : ( Int, Int )
southEast =
    ( 1, 1 )


east : ( Int, Int )
east =
    ( 0, 1 )


west : ( Int, Int )
west =
    ( 0, -1 )

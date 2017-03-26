module Update exposing (..)

import Dict exposing (Dict)
import Model exposing (..)
import Util exposing (..)


type Msg
    = Select Piece Position



-- RULES


pawnRules : Position -> List Position
pawnRules ( x, y ) =
    if x == 6 then
        [ ( -1, 0 ), ( -2, 0 ) ]
    else
        [ ( -1, 0 ) ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select piece position ->
            { model
                | selection =
                    Just
                        { position = position
                        , validMoves = validMoves piece.rank position
                        }
            }


calculateMove : Position -> Position -> Position
calculateMove ( x, y ) ( dx, dy ) =
    ( x + dx, y + dy )


validMoves : Rank -> Position -> Dict Position Bool
validMoves rank position =
    case rank of
        Pawn ->
            toDict <| List.map (calculateMove position) (pawnRules position)

        _ ->
            Dict.empty

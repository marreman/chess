module Model exposing (..)

import Dict exposing (Dict)
import Util exposing (..)


type alias Model =
    { selection : Maybe Selection
    , pieces : Dict Position Piece
    }


type alias Selection =
    { position : Position
    , validMoves : Dict Position Bool
    }


type alias Position =
    ( Int, Int )


type alias Piece =
    ( Rank, Color )


type Rank
    = King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn


type Color
    = White
    | Black


model : Model
model =
    { selection = Nothing
    , pieces = pieces
    }


pieces =
    Dict.fromList
        [ ( 7, 1 ) => ( Knight, White )
        , ( 1, 0 ) => ( Pawn, Black )
        ]



-- pieces : Dict Position Piece
-- pieces =
--     Dict.fromList
--         [ ( 0, 0 ) => ( Rook, Black )
--         , ( 0, 1 ) => ( Knight, Black )
--         , ( 0, 2 ) => ( Bishop, Black )
--         , ( 0, 3 ) => ( Queen, Black )
--         , ( 0, 4 ) => ( King, Black )
--         , ( 0, 5 ) => ( Bishop, Black )
--         , ( 0, 6 ) => ( Knight, Black )
--         , ( 0, 7 ) => ( Rook, Black )
--         , ( 1, 0 ) => ( Pawn, Black )
--         , ( 1, 1 ) => ( Pawn, Black )
--         , ( 1, 2 ) => ( Pawn, Black )
--         , ( 1, 3 ) => ( Pawn, Black )
--         , ( 1, 4 ) => ( Pawn, Black )
--         , ( 1, 5 ) => ( Pawn, Black )
--         , ( 1, 6 ) => ( Pawn, Black )
--         , ( 1, 7 ) => ( Pawn, Black )
--         , ( 6, 0 ) => ( Pawn, White )
--         , ( 6, 1 ) => ( Pawn, White )
--         , ( 6, 2 ) => ( Pawn, White )
--         , ( 6, 3 ) => ( Pawn, White )
--         , ( 6, 4 ) => ( Pawn, White )
--         , ( 6, 5 ) => ( Pawn, White )
--         , ( 6, 6 ) => ( Pawn, White )
--         , ( 6, 7 ) => ( Pawn, White )
--         , ( 7, 0 ) => ( Rook, White )
--         , ( 7, 1 ) => ( Knight, White )
--         , ( 7, 2 ) => ( Bishop, White )
--         , ( 7, 3 ) => ( Queen, White )
--         , ( 7, 4 ) => ( King, White )
--         , ( 7, 5 ) => ( Bishop, White )
--         , ( 7, 6 ) => ( Knight, White )
--         , ( 7, 7 ) => ( Rook, White )
--         ]

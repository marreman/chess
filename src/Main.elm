module Main exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Dict exposing (Dict)


main =
    Html.beginnerProgram
        { model = model
        , update = update
        , view = view
        }


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
    { rank : Rank
    , color : Color
    }


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
    , pieces = Dict.fromList pieces
    }


(=>) x y =
    ( x, y )
infixr 9 =>


pieces =
    -- Black
    [ ( 0, 0 ) => Piece Rook Black
    , ( 0, 1 ) => Piece Knight Black
    , ( 0, 2 ) => Piece Bishop Black
    , ( 0, 3 ) => Piece Queen Black
    , ( 0, 4 ) => Piece King Black
    , ( 0, 5 ) => Piece Bishop Black
    , ( 0, 6 ) => Piece Knight Black
    , ( 0, 7 ) => Piece Rook Black
    , ( 1, 0 ) => Piece Pawn Black
    , ( 1, 1 ) => Piece Pawn Black
    , ( 1, 2 ) => Piece Pawn Black
    , ( 1, 3 ) => Piece Pawn Black
    , ( 1, 4 ) => Piece Pawn Black
    , ( 1, 5 ) => Piece Pawn Black
    , ( 1, 6 ) => Piece Pawn Black
    , ( 1, 7 ) => Piece Pawn Black
      -- White
    , ( 6, 0 ) => Piece Pawn White
    , ( 6, 1 ) => Piece Pawn White
    , ( 6, 2 ) => Piece Pawn White
    , ( 6, 3 ) => Piece Pawn White
    , ( 6, 4 ) => Piece Pawn White
    , ( 6, 5 ) => Piece Pawn White
    , ( 6, 6 ) => Piece Pawn White
    , ( 6, 7 ) => Piece Pawn White
    , ( 7, 0 ) => Piece Rook White
    , ( 7, 1 ) => Piece Knight White
    , ( 7, 2 ) => Piece Bishop White
    , ( 7, 3 ) => Piece Queen White
    , ( 7, 4 ) => Piece King White
    , ( 7, 5 ) => Piece Bishop White
    , ( 7, 6 ) => Piece Knight White
    , ( 7, 7 ) => Piece Rook White
    ]


type Msg
    = Select Piece Position



-- RULES


toDict list =
    Dict.fromList <|
        List.map (\value -> ( value, True )) list


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


view model =
    Html.div []
        [ viewBoard model
        , Html.hr [] []
        , Html.text (toString model.selection)
        ]


viewBoard model =
    let
        rank x _ =
            Html.div [ Html.Attributes.class "rank" ] <|
                List.indexedMap (square x) (List.range 0 7)

        square x y _ =
            viewSquare model ( x, y )
    in
        Html.div [] <|
            List.indexedMap rank
                (List.range 0 7)


viewSquare model position =
    let
        ( x, y ) =
            position

        isValidSquare { validMoves } =
            validMoves
                |> Dict.get position
                |> Maybe.withDefault False

        classList =
            [ "square" => True
            , color => True
            , "valid"
                => (model.selection
                        |> Maybe.map isValidSquare
                        |> Maybe.withDefault False
                   )
            ]

        color =
            if (x + y) % 2 == 0 then
                "light"
            else
                "dark"
    in
        Html.div
            [ Html.Attributes.classList classList ]
            [ Html.div []
                [ Html.span [ Html.Attributes.class "position" ]
                    [ Html.text (toString ( x, y )) ]
                , viewPiece ( x, y ) model
                ]
            ]


viewPiece : Position -> Model -> Html Msg
viewPiece position model =
    case Dict.get position model.pieces of
        Nothing ->
            Html.text ""

        Just piece ->
            Html.span
                [ Html.Events.onClick (Select piece position)
                , Html.Attributes.classList
                    [ "piece" => True
                    , (toString piece.rank) => True
                    , (toString piece.color) => True
                    , "selected"
                        => (model.selection
                                |> Maybe.map (\selection -> selection.position == position)
                                |> Maybe.withDefault False
                           )
                    ]
                ]
                []

module View exposing (view)

import Dict
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Model exposing (..)
import Update exposing (..)
import Util exposing (..)


view : Model -> Html Msg
view model =
    let
        rank x =
            div [ class "rank" ] <|
                List.map (square x) (List.range 0 7)

        square x y =
            viewSquare model ( x, y )
    in
    div [ class "board" ] <|
        List.map rank (List.range 0 7)


viewSquare : Model -> Position -> Html Msg
viewSquare model position =
    let
        ( x, y ) =
            position

        isValidSquare { validMoves } =
            validMoves
                |> Dict.get position
                |> Maybe.withDefault False

        validSquare =
            model.selection
                |> Maybe.map isValidSquare
                |> Maybe.withDefault False

        color =
            if modBy 2 (x + y) == 0 then
                "light"

            else
                "dark"

        move =
            if validSquare then
                MoveTo position

            else
                NoOp
    in
    div
        [ onClick move
        , classList
            [ ( "square", True )
            , ( color, True )
            , ( "valid", validSquare )
            ]
        ]
        [ div []
            [ span
                [ class "position" ]
                [ text <| String.concat [ "(", String.fromInt x, ", ", String.fromInt y, ")" ] ]
            , viewPiece ( x, y ) model
            ]
        ]


viewPiece : Position -> Model -> Html Msg
viewPiece position model =
    case Dict.get position model.board of
        Nothing ->
            text ""

        Just piece ->
            span
                [ onClick (Select piece position)
                , classList
                    [ ( "piece", True )
                    , ( viewRank piece.rank, True )
                    , ( viewColor piece.color, True )
                    , ( "selected"
                      , model.selection
                            |> Maybe.map (\selection -> selection.position == position)
                            |> Maybe.withDefault False
                      )
                    ]
                ]
                []


viewRank : Rank -> String
viewRank rank =
    case rank of
        King ->
            "King"

        Queen ->
            "Queen"

        Rook ->
            "Rook"

        Bishop ->
            "Bishop"

        Knight ->
            "Knight"

        Pawn ->
            "Pawn"


viewColor : Color -> String
viewColor color =
    case color of
        White ->
            "White"

        Black ->
            "Black"

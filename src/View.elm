module View exposing (view)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Dict
import Util exposing (..)
import Model exposing (..)
import Update exposing (..)


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

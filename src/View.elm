module View exposing (view)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Dict
import Util exposing (..)
import Model exposing (..)
import Update exposing (..)


view : Model -> Html Msg
view model =
    Html.div []
        [ viewBoard model
        , Html.hr [] []
        , Html.text (toString model.selection)
        ]


viewBoard : Model -> Html Msg
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

        classList =
            [ "square" => True
            , color => True
            , "valid" => validSquare
            ]

        color =
            if (x + y) % 2 == 0 then
                "light"
            else
                "dark"

        move =
            if validSquare then
                MoveTo position
            else
                NoOp
    in
        Html.div
            [ Html.Events.onClick move
            , Html.Attributes.classList classList
            ]
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
                    , (toString <| Tuple.first piece) => True
                    , (toString <| Tuple.second piece) => True
                    , "selected"
                        => (model.selection
                                |> Maybe.map (\selection -> selection.position == position)
                                |> Maybe.withDefault False
                           )
                    ]
                ]
                []

module Update exposing (..)

import Dict exposing (Dict)
import Model exposing (..)
import Util exposing (..)


type Msg
    = NoOp
    | Select Piece Position
    | MoveTo Position


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Select piece position ->
            case model.selection of
                Nothing ->
                    { model
                        | selection =
                            Just
                                { position = position
                                , validMoves = toDict <| getValidMoves model piece position
                                }
                    }

                _ ->
                    model

        MoveTo position ->
            case model.selection of
                Nothing ->
                    model

                Just selection ->
                    let
                        pieceToMove =
                            model.pieces
                                |> Dict.get selection.position

                        movePiece piece =
                            model.pieces
                                |> Dict.remove selection.position
                                |> Dict.update position (\_ -> Just piece)
                    in
                        case pieceToMove of
                            Nothing ->
                                model

                            Just piece ->
                                { model
                                    | pieces = movePiece piece
                                    , selection = Nothing
                                }


getValidMoves : Model -> Piece -> Position -> List Position
getValidMoves model piece position =
    case piece.rank of
        Pawn ->
            let
                regularMoves =
                    case piece.color of
                        Black ->
                            if (Tuple.first position) == 1 then
                                [ south, path [ south, south ] ]
                            else
                                [ south ]

                        White ->
                            if (Tuple.first position) == 6 then
                                [ north, path [ north, north ] ]
                            else
                                [ north ]

                capturingMoves =
                    case piece.color of
                        Black ->
                            [ southEast, southWest ]

                        White ->
                            [ northEast, northWest ]
            in
                (regularMoves
                    |> List.map (sum position)
                    |> List.filter
                        (\pos ->
                            Dict.get pos model.pieces
                                |> Maybe.map (\_ -> False)
                                |> Maybe.withDefault True
                        )
                )
                    ++ (capturingMoves
                            |> List.map (sum position)
                            |> List.filter
                                (\pos ->
                                    Dict.get pos model.pieces
                                        |> Maybe.map (\p -> p.color /= piece.color)
                                        |> Maybe.withDefault False
                                )
                       )

        King ->
            let
                moves =
                    [ north
                    , northEast
                    , east
                    , southEast
                    , south
                    , southWest
                    , west
                    , northWest
                    ]
            in
                moves
                    |> List.map (sum position)
                    |> List.filter (isValidMove model piece)

        Knight ->
            let
                moves =
                    [ path [ north, north, east ]
                    , path [ north, north, west ]
                    , path [ west, west, north ]
                    , path [ west, west, south ]
                    , path [ west, south, south ]
                    , path [ east, east, north ]
                    , path [ east, east, south ]
                    , path [ east, south, south ]
                    ]
            in
                moves
                    |> List.map (sum position)
                    |> List.filter (isValidMove model piece)

        Rook ->
            (traverse north piece position model)
                ++ (traverse east piece position model)
                ++ (traverse south piece position model)
                ++ (traverse west piece position model)

        Bishop ->
            (traverse northEast piece position model)
                ++ (traverse northWest piece position model)
                ++ (traverse southEast piece position model)
                ++ (traverse southWest piece position model)

        Queen ->
            (traverse northEast piece position model)
                ++ (traverse northWest piece position model)
                ++ (traverse southEast piece position model)
                ++ (traverse southWest piece position model)
                ++ (traverse north piece position model)
                ++ (traverse east piece position model)
                ++ (traverse south piece position model)
                ++ (traverse west piece position model)


traverse direction piece position model =
    let
        newPosition =
            sum position direction

        isOutOfBounds =
            let
                ( x, y ) =
                    newPosition
            in
                x > 7 || y > 7 || x < 0 || y < 0

        hasEnemyPiece =
            Dict.get newPosition model.pieces
                |> Maybe.map (\p -> p.color /= piece.color)
                |> Maybe.withDefault False

        hasFriendlyPiece =
            Dict.get newPosition model.pieces
                |> Maybe.map (\p -> p.color == piece.color)
                |> Maybe.withDefault False
    in
        if isOutOfBounds then
            []
        else if hasFriendlyPiece then
            []
        else if hasEnemyPiece then
            [ newPosition ]
        else
            [ newPosition ] ++ traverse direction piece newPosition model


isValidMove : Model -> Piece -> Position -> Bool
isValidMove model capturingPiece position =
    case Dict.get position model.pieces of
        Nothing ->
            True

        Just piece ->
            piece.color /= capturingPiece.color

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
    case piece of
        ( King, _ ) ->
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

        ( Knight, _ ) ->
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

        ( Rook, _ ) ->
            (traverse north piece position model)
                ++ (traverse east piece position model)
                ++ (traverse south piece position model)
                ++ (traverse west piece position model)

        ( Bishop, _ ) ->
            (traverse northEast piece position model)
                ++ (traverse northWest piece position model)
                ++ (traverse southEast piece position model)
                ++ (traverse southWest piece position model)

        ( Queen, _ ) ->
            (traverse northEast piece position model)
                ++ (traverse northWest piece position model)
                ++ (traverse southEast piece position model)
                ++ (traverse southWest piece position model)
                ++ (traverse north piece position model)
                ++ (traverse east piece position model)
                ++ (traverse south piece position model)
                ++ (traverse west piece position model)

        _ ->
            []


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
                |> Maybe.map (\p -> Tuple.second p /= Tuple.second piece)
                |> Maybe.withDefault False

        hasFriendlyPiece =
            Dict.get newPosition model.pieces
                |> Maybe.map (\p -> Tuple.second p == Tuple.second piece)
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
isValidMove model ( _, capturingColor ) position =
    case Dict.get position model.pieces of
        Nothing ->
            True

        Just ( _, color ) ->
            color /= capturingColor

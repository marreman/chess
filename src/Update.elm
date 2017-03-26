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
            let
                selection =
                    Just
                        { position = position
                        , validMoves =
                            toDict <|
                                getValidMoves model.board piece position
                        }
            in
                case model.selection of
                    Nothing ->
                        { model | selection = selection }

                    _ ->
                        model

        MoveTo position ->
            case model.selection of
                Nothing ->
                    model

                Just selection ->
                    let
                        pieceToMove =
                            model.board
                                |> Dict.get selection.position

                        movePiece piece =
                            model.board
                                |> Dict.remove selection.position
                                |> Dict.update position (\_ -> Just piece)
                    in
                        case pieceToMove of
                            Nothing ->
                                model

                            Just piece ->
                                { model
                                    | board = movePiece piece
                                    , selection = Nothing
                                }


getValidMoves : Board -> Piece -> Position -> List Position
getValidMoves board piece position =
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
                            Dict.get pos board
                                |> Maybe.map (\_ -> False)
                                |> Maybe.withDefault True
                        )
                )
                    ++ (capturingMoves
                            |> List.map (sum position)
                            |> List.filter
                                (\pos ->
                                    Dict.get pos board
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
                    |> List.filter (isValidMove board piece)

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
                    |> List.filter (isValidMove board piece)

        Rook ->
            (traverse north piece position board)
                ++ (traverse east piece position board)
                ++ (traverse south piece position board)
                ++ (traverse west piece position board)

        Bishop ->
            (traverse northEast piece position board)
                ++ (traverse northWest piece position board)
                ++ (traverse southEast piece position board)
                ++ (traverse southWest piece position board)

        Queen ->
            (traverse northEast piece position board)
                ++ (traverse northWest piece position board)
                ++ (traverse southEast piece position board)
                ++ (traverse southWest piece position board)
                ++ (traverse north piece position board)
                ++ (traverse east piece position board)
                ++ (traverse south piece position board)
                ++ (traverse west piece position board)


traverse : Position -> Piece -> Position -> Board -> List Position
traverse direction piece position board =
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
            Dict.get newPosition board
                |> Maybe.map (\p -> p.color /= piece.color)
                |> Maybe.withDefault False

        hasFriendlyPiece =
            Dict.get newPosition board
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
            [ newPosition ] ++ traverse direction piece newPosition board


isValidMove : Board -> Piece -> Position -> Bool
isValidMove board capturingPiece position =
    case Dict.get position board of
        Nothing ->
            True

        Just piece ->
            piece.color /= capturingPiece.color

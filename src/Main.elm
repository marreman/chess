module Main exposing (..)

import Html
import Model
import Update
import View


main : Program Never Model.Model Update.Msg
main =
    Html.beginnerProgram
        { model = Model.model
        , update = Update.update
        , view = View.view
        }

module Main exposing (..)

import Browser
import Html
import Model
import Update
import View


main : Program () Model.Model Update.Msg
main =
    Browser.sandbox
        { init = Model.model
        , update = Update.update
        , view = View.view
        }

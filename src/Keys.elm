module Keys exposing (..)

import Json.Decode as Decode
import Types exposing (..)


keyDecoder : Decode.Decoder Msg
keyDecoder =
    let
        xxx =
            Debug.log "key 33 " Decode.string
    in
    Decode.map toKey (Decode.field "key" xxx)


toKey : String -> Msg
toKey keyValue =
    let
        ( char, stuff ) =
            Maybe.withDefault ( 'd', "b" ) (String.uncons keyValue)
    in
    if char == 'w' || keyValue == "ArrowUp" then
        Move Up

    else if char == 's' || keyValue == "ArrowDown" then
        Move Down

    else if char == 'a' || keyValue == "ArrowLeft" then
        Move Left

    else if char == 'd' || keyValue == "ArrowRight" then
        Move Right

    else if keyValue == " " then
        Move Space

    else
        Move None

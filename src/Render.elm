module Render exposing (..)

import Canvas exposing (circle, lineTo, rect, shapes)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)
import Values exposing (..)


renderMap : Float -> Float -> Int -> Int -> Int -> Int -> List Snake -> Vec2 -> Html Msg
renderMap tileW tileH screenwidth screenheight width height snakes apple =
    Canvas.toHtml
        ( screenwidth, screenheight )
        [ style "border" "10px solid rgba(0,0,0,0.1)" ]
        ([ clearScreen screenwidth screenheight ]
            ++ renderSquares screenheight screenwidth width height tileW tileH
            ++ renderSnakes snakes tileW tileH
            ++ [ renderApple apple tileW tileH ]
        )


clearScreen screenwidth screenheight =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat screenwidth) (toFloat screenheight) ]


renderApple : Vec2 -> Float -> Float -> Canvas.Renderable
renderApple apple tileWidth tileHeight =
    shapes [ fill Color.red ]
        [ circle ( (toFloat apple.x + 0.5) * tileWidth, (toFloat apple.y + 0.5) * tileHeight ) (tileWidth / 3) ]


renderVertical : Int -> Float -> Int -> List Canvas.Renderable
renderVertical screenwidth tileHeight number =
    if number == 0 then
        []

    else
        [ shapes [ stroke Color.black ]
            [ rect ( 0, toFloat (number - 1) * tileHeight ) (toFloat screenwidth) tileHeight ]
        ]
            ++ renderVertical screenwidth tileHeight (number - 1)


renderHorisontal : Int -> Float -> Int -> List Canvas.Renderable
renderHorisontal screenheight tileWidth number =
    if number == 0 then
        []

    else
        [ shapes [ fill Color.white, stroke Color.black ]
            [ rect ( toFloat (number - 1) * tileWidth, 0 ) tileWidth (toFloat screenheight) ]
        ]
            ++ renderHorisontal screenheight tileWidth (number - 1)


renderSquares : Int -> Int -> Int -> Int -> Float -> Float -> List Canvas.Renderable
renderSquares screenheight screenwidth width height tileWidth tileHeight =
    renderHorisontal screenheight tileWidth width ++ renderVertical screenwidth tileHeight height


renderRect : Float -> Float -> Vec2 -> Canvas.Renderable
renderRect tileWidth tileHeight vec =
    shapes [ fill Color.green, stroke Color.black ]
        [ rect ( toFloat vec.x * tileWidth, toFloat vec.y * tileHeight ) tileWidth tileHeight ]


renderSnake : Float -> Float -> Snake -> List Canvas.Renderable
renderSnake tileW tileH snake =
    List.map (renderRect tileW tileH) snake.points


renderSnakes snakes tileW tileH =
    List.foldl (++) [] (List.map (renderSnake tileW tileH) snakes)

module Main exposing (main)

import Browser exposing (..)
import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Keys exposing (..)
import Random exposing (..)
import Render exposing (..)
import Snake exposing (..)
import Types exposing (..)
import Utils exposing (..)
import Values exposing (..)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MSG
-- MODEL
-- init


init : { startTime : Float, winWidth : Int, winHeight : Int } -> ( Model, Cmd msg )
init { startTime, winWidth, winHeight } =
    let
        minW =
            if winHeight < winWidth then
                winHeight

            else
                winWidth

        whichwidth =
            if winHeight < winWidth then
                mapheight

            else
                mapwidth

        tilewidth =
            toFloat minW / whichwidth

        tileheight =
            tilewidth

        sHeight =
            round (tileheight * mapheight)

        sWidth =
            round (tilewidth * mapwidth)
    in
    ( { snakes =
            [ { points = [ Vec2 4 0, Vec2 3 0, Vec2 2 0, Vec2 1 0, Vec2 0 0 ]
              , velocity = Vec2 1 0
              , isDead = False
              , isGrowing = 0
              }
            ]
      , timeToGo = timebeforeupdate
      , apple = Vec2 (mapwidth // 2) (mapheight // 4)
      , gameOver = False
      , screen = Screen sWidth sHeight tilewidth tileheight
      }
    , Cmd.none
    )



-- UPDATE


pointrandom : Random.Generator ( Int, Int )
pointrandom =
    Random.pair (Random.int 0 (mapwidth - 1)) (Random.int 0 (mapheight - 1))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        Frame time ->
            ( { model | timeToGo = model.timeToGo - time }, Cmd.none )

        Move dir ->
            if dir == Space && model.gameOver then
                init { startTime = 1.0, winHeight = model.screen.height, winWidth = model.screen.width }

            else
                ( { model | snakes = changeSnakeVel model.snakes dir }, Cmd.none )

        NewApple newpoint ->
            ( { model | apple = Vec2 (Tuple.first newpoint) (Tuple.second newpoint) }, Cmd.none )
    )
        |> (\( m, cmd ) ->
                if m.timeToGo < 0 then
                    let
                        ( newsnakes, gotapple ) =
                            updateApple (updateSnakes m.snakes) model.apple
                    in
                    ( { m
                        | snakes = newsnakes
                        , timeToGo = m.timeToGo + timebeforeupdate
                      }
                    , if gotapple then
                        Random.generate NewApple pointrandom

                      else
                        Cmd.none
                    )

                else
                    ( m, Cmd.none )
           )
        |> (\( m, cmd ) ->
                ( if (firstSnake m.snakes).isDead then
                    { m | gameOver = True }

                  else
                    m
                , cmd
                )
           )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        ([]
            ++ (let
                    firstS =
                        firstSnake model.snakes
                in
                if model.gameOver then
                    [ div [] [ text "game over. Space to restart" ] ]

                else
                    [ div [ style "left" "0px", style "position" "absolute" ] [ text ("score: " ++ String.fromInt (List.length firstS.points)) ]
                    , renderMap model.screen.tileWidth model.screen.tileHeight model.screen.width model.screen.height mapwidth mapheight (List.filter (\snake -> not snake.isDead) model.snakes) model.apple
                    ]
               )
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown keyDecoder
        , onAnimationFrameDelta Frame
        ]

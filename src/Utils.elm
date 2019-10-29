module Utils exposing (..)

import Types exposing (..)


makeEmptySnake : Snake
makeEmptySnake =
    { points = []
    , velocity = Vec2 1 0
    , isDead = False
    , isGrowing = 0
    }


firstSnake li =
    let
        t =
            List.head li
    in
    case t of
        Nothing ->
            makeEmptySnake

        Just v ->
            v


first li =
    let
        t =
            List.head li
    in
    case t of
        Nothing ->
            Vec2 0 1

        Just v ->
            v


last li =
    let
        t =
            List.tail li
    in
    case t of
        Nothing ->
            []

        Just tt ->
            tt

module Snake exposing (..)

import Types exposing (..)
import Utils exposing (..)
import Values exposing (..)


updateSnake : Snake -> Snake
updateSnake snake =
    { snake
        | points =
            [ add (first snake.points) snake.velocity ]
                ++ List.take (List.length snake.points - 1) snake.points
                ++ (if snake.isGrowing > 0 then
                        case
                            List.head (List.reverse snake.points)
                        of
                            Nothing ->
                                []

                            Just li ->
                                [ li ]

                    else
                        []
                   )
        , isGrowing =
            if snake.isGrowing > 0 then
                snake.isGrowing - 1

            else
                0
    }


outsidemap : Vec2 -> Bool
outsidemap point =
    point.x >= mapwidth || point.x < 0 || point.y >= mapheight || point.y < 0


checkCollision : List Snake -> Snake -> Snake
checkCollision snakes snake =
    let
        allpoints =
            List.foldl
                (\snakeinside currval ->
                    if snake == snakeinside then
                        currval

                    else
                        snakeinside.points ++ currval
                )
                []
                snakes
    in
    if snake.isDead then
        snake

    else
        { snake
            | isDead =
                List.any (\pointinli -> first snake.points == pointinli) (allpoints ++ last snake.points)
                    || outsidemap (first snake.points)
        }


updateSnakes snakes =
    List.map updateSnake snakes
        |> List.map (checkCollision snakes)


checkSnakeApple apple snake =
    if snake.isDead then
        ( snake, False )

    else if first snake.points == apple then
        ( { snake | isGrowing = snake.isGrowing + 5 }, True )

    else
        ( snake, False )


updateApple : List Snake -> Vec2 -> ( List Snake, Bool )
updateApple snakes apple =
    List.map (checkSnakeApple apple) snakes
        |> List.unzip
        |> (\( ss, gotapple ) -> ( ss, List.any (\a -> a) gotapple ))


changeSnakeVel : List Snake -> Direction -> List Snake
changeSnakeVel snakes dir =
    let
        temp1 =
            List.head snakes

        firstSnake =
            case temp1 of
                Nothing ->
                    Snake [] (Vec2 1 0) False 0

                Just s ->
                    s

        temp =
            List.tail snakes

        othersnakes =
            case temp of
                Nothing ->
                    []

                Just li ->
                    li
    in
    let
        newvec =
            case dir of
                Up ->
                    Vec2 0 -1

                Right ->
                    Vec2 1 0

                Left ->
                    Vec2 -1 0

                Down ->
                    Vec2 0 1

                --[ { firstSnake | velocity = Vec2 0 1 } ] ++ othersnakes
                None ->
                    Vec2 0 0

                Space ->
                    Vec2 0 0
    in
    if newvec == mult -1 firstSnake.velocity then
        snakes

    else
        [ { firstSnake
            | velocity =
                if newvec == Vec2 0 0 then
                    firstSnake.velocity

                else
                    newvec
          }
        ]
            ++ othersnakes

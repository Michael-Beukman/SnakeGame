module Types exposing (..)


type alias Vec2 =
    { x : Int, y : Int }


type alias Snake =
    { points : List Vec2, velocity : Vec2, isDead : Bool, isGrowing : Int }


type alias Screen =
    { width : Int
    , height : Int
    , tileWidth : Float
    , tileHeight : Float
    }


type alias Model =
    { snakes : List Snake, timeToGo : Float, apple : Vec2, gameOver : Bool, screen : Screen }


type Direction
    = Up
    | Down
    | Left
    | Right
    | None
    | Space


type Msg
    = Move Direction
    | Frame Float
    | NewApple ( Int, Int )


vecstuff : (Int -> Int -> Int) -> Vec2 -> Vec2 -> Vec2
vecstuff op v1 v2 =
    Vec2 (op v1.x v2.x) (op v1.y v2.y)


add =
    vecstuff (+)


mult num vec =
    Vec2 (vec.x * num) (vec.y * num)

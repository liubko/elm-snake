module SnakeGame where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Random exposing (Seed, int, initialSeed, generate)
import Keyboard
import Text
import Window
import Debug

-- MODEL

(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)

cellSize = 20
borderSize = cellSize*1
startLength = 3

type Direction = UP | DOWN | LEFT | RIGHT
type GameState = NewGame | Play | Lose

type alias Snake =
  { dir: Direction
  , body: List Cell
  }

type alias Cell =
  { x : Float
  , y : Float
  }

type alias Game =
  { snake : Snake
  , state : GameState
  , fruit : Cell
  , seed : Seed
  }

type Input
  = Space Bool
  | Coord Cell

defaultSnake : Snake
defaultSnake =
  Snake RIGHT [
    (Cell 0 0),
    (Cell (-1*cellSize) 0),
    (Cell (-2*cellSize) 0)
  ]

defaultGame : Game
defaultGame =
  let
    seed = initialSeed 74811
    (fruit, newSeed) = randomFruit defaultSnake seed
  in
    { snake = defaultSnake
    , state = NewGame
    , fruit = fruit
    , seed = newSeed
    }

randomFruit : Snake -> Seed -> (Cell, Seed)
randomFruit snake seed0 =
  let
    (newX, seed1) = generate (int (borderSize-halfWidth) (halfWidth-borderSize)) seed0
    (newY, seed2) = generate (int (borderSize-halfHeight) (halfHeight-borderSize)) seed1
    roundedX = (toFloat (round ((toFloat newX)/cellSize)) * cellSize)
    roundedY = (toFloat (round ((toFloat newY)/cellSize)) * cellSize)
    isCellAlreadyOccupied = List.any (\a -> a.x==roundedX && a.y==roundedY) snake.body
  in
      if | isCellAlreadyOccupied -> randomFruit snake seed2
         | otherwise -> (Cell roundedX roundedY, seed2)

getSnakeHead : Snake -> Cell
getSnakeHead snake = Maybe.withDefault {x=0, y=0} (List.head snake.body)

-- UPDATE

update : Input -> Game -> Game
update input ({snake, state, fruit, seed} as game) =
  case (input, state) of
    (Space space, NewGame) ->
      { game |
          state <- if space then Play else state
      }

    (Space space, Lose) ->
      if space then defaultGame else game

    (Coord coord, Play) ->
      let
        isSnakeCollision = checkForSnakeCollision snake
        isFruitCollision = checkForSnakeAndFruitCollision snake fruit
        newSnake0 = if isFruitCollision then (incrementSnake snake) else snake
        (newFruit, newSeed) = if isFruitCollision then (randomFruit snake seed) else (fruit, seed)
        newSnake1 = updateSnake coord newSnake0
      in
        { game |
          state <- if not isSnakeCollision then Play else Lose,
          snake <- if isSnakeCollision then snake else newSnake1,
          fruit <- newFruit,
          seed <- newSeed
        }

    _ -> game

checkForSnakeCollision : Snake -> Bool
checkForSnakeCollision snake =
  let
    head = getSnakeHead snake
  in
    List.drop 1 snake.body
      |> List.any (\a -> a.x==head.x && a.y==head.y)

-- twoElementsCollision Point po

checkForSnakeAndFruitCollision : Snake -> Cell -> Bool
checkForSnakeAndFruitCollision snake fruit =
  let
    head = getSnakeHead snake
  in
    head.x==fruit.x && head.y==fruit.y

incrementSnake : Snake -> Snake
incrementSnake ({body} as snake) =
  let
    tail = List.reverse body |> List.take 1
    newBody = List.append body tail
  in
    { snake | body <- newBody }

updateSnake : Cell -> Snake -> Snake
updateSnake coord snake =
  let
    newDirection = if | coord.y > 0 && not (snake.dir == DOWN) -> UP
                      | coord.y < 0 && not (snake.dir == UP) -> DOWN
                      | coord.x < 0 && not (snake.dir == RIGHT) -> LEFT
                      | coord.x > 0 && not (snake.dir == LEFT) -> RIGHT
                      | otherwise -> snake.dir

    snakeHead = getSnakeHead snake

    newX = if | newDirection == LEFT -> snakeHead.x - cellSize
              | newDirection == RIGHT -> snakeHead.x + cellSize
              | otherwise -> snakeHead.x

    newY = if | newDirection == DOWN -> snakeHead.y - cellSize
              | newDirection == UP -> snakeHead.y + cellSize
              | otherwise -> snakeHead.y

    newSnakeHead = {
      y = clamp (borderSize-halfHeight) (halfHeight-borderSize) newY,
      x = clamp (borderSize-halfWidth) (halfWidth-borderSize) newX
    }

  in
    { snake |
        dir <- newDirection,
        body <- newSnakeHead :: (snake.body |> List.reverse |> List.drop 1 |> List.reverse)
    }
    |> Debug.watch "Snake"

-- VIEW

view : (Int,Int) -> Game -> Element
view (w,h) ({snake, state, fruit} as game) =
  let
    score = ((List.length snake.body) - startLength) * 10

    lable = if | state==Lose -> "Your score is " ++ (toString score) ++ ". Press SPACE."
                | state==NewGame -> "Press SPACE to start, use ARROWS to control"
                | otherwise -> ""

    textLabel =
      txt (Text.height 20) lable
        |> toForm
        |> move (0, (borderSize*3) - halfHeight)
  in
    container w h middle <|
    collage gameWidth gameHeight
      (
        [textLabel]
          |> List.append (paintSnake snake)
          |> List.append
            [ rect gameWidth gameHeight
                |> filled fieldColor
            , rect gameWidth gameHeight
                |> outlined { defaultLine |
                                width <- borderSize,
                                color <- borderColor
                            }
            , paintFruit fruit
            ]

      )

paintFruit: Cell -> Form
paintFruit f =
    rect cellSize cellSize
      |> filled fruitColor
      |> move (f.x, f.y)

paintSnake: Snake -> List Form
paintSnake snake =
  List.indexedMap paintSnakeElement snake.body

paintSnakeElement: Int -> Cell -> Form
paintSnakeElement index el =
  let
    alpha = clamp 0.7 1 (1-((toFloat index)*0.05))
  in
    rect cellSize cellSize
      |> filled (hsla 178 0.3 0.52 alpha)
      |> move (el.x, el.y)

txt f string =
  Text.fromString string
    |> Text.color textColor
    |> Text.monospace
    |> f
    |> leftAligned

fieldColor = rgb 234 247 196
borderColor = rgb 88 80 79
fruitColor = rgb 236 100 88
textColor = rgb 90 120 127

-- SIGNALS

gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input


delta =
  Signal.map inSeconds (fps 15)


input : Signal Input
input =
  Signal.mergeMany
    [ Signal.map Space Keyboard.space
    , Signal.map (\a -> {x=(toFloat a.x), y=(toFloat a.y)}) Keyboard.arrows
        |> Signal.map Coord
        |> Signal.sampleOn delta
    ]


main =
  Signal.map2 view Window.dimensions gameState

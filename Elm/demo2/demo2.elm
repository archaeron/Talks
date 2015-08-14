-- See this document for more information on making Pong:
-- http://elm-lang.org/blog/pong
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window
import Debug


-- MODELS

startPlayerX = -400
startPlayerY = 100

pillarSize = 400
pillarWidth = 10

type alias Positioned a =
  { a
  | x : Float
  , y : Float
  }

type alias Named a =
  { a
  | name : String
  }

type alias Pillar =
  { name: String
  , y : Float
  , x : Float
  , vx : Float
  }

type alias Player =
  { name : String
  , x : Float
  , y : Float
  , vy : Float
  , score : Int
  }

type alias Game =
  { pillars : List Pillar
  , player : Player
  }

-- DEFAULT VALUES

defaultPlayer : Player
defaultPlayer =
  Player "Player" startPlayerX startPlayerY 1 0


defaultGame : Game
defaultGame =
  { pillars =
      [ Pillar "Pillar 1 top" -400 1200 10
      , Pillar "Pillar 1 bottom" 400 1200 10
      , Pillar "Pillar 2 top" -400 0 10
      , Pillar "Pillar 2 bottom" 400 0 10
      ]
  , player = defaultPlayer
  }

-- COLORS

red : Color
red = rgb 255 0 0

green : Color
green = rgb 0 255 0

-- SHAPES

getPlayerShape : Shape
getPlayerShape =
  oval 15 15

getObstacleShape : Shape
getObstacleShape =
  rect pillarWidth pillarSize

obstacleForm : Form
obstacleForm =
  filled green getObstacleShape

playerForm : Form
playerForm =
  filled red getPlayerShape

drawForm : Form -> Positioned (Named a) -> Form
drawForm form object =
  move (Debug.watch object.name (object.x, object.y)) form

drawPlayer : Player -> Form
drawPlayer =
  drawForm playerForm

drawObstacle : List Pillar -> List Form
drawObstacle pillars =
  List.map (drawForm obstacleForm) pillars

-- UPDATE

view : (Int, Int) -> Game -> Element
view (dimX, dimY) game =
  collage dimX dimY (drawPlayer game.player :: drawObstacle game.pillars)


updatePlayerVel : Player -> Float -> Bool -> Player
updatePlayerVel player vy isPressed =
  case isPressed of
    True ->
      { player
      | vy <- 20
      , y <- (vy + player.y)
      }
    False ->
      { player
      | vy <- vy - 1
      , y <- (vy + player.y)
      }

updatePillar : Pillar -> Pillar
updatePillar pillar =
  case pillar.x < -1200 of
    False ->
      { pillar
      | x <- (pillar.x - pillar.vx)
      }
    True ->
      { pillar
      | x <- 1200
      }

updateObstacles : List Pillar -> List Pillar
updateObstacles pillars =
  List.map updatePillar pillars

within : Player -> Pillar -> Bool
within player pillar =
  if | horizontalCollision player pillar && verticalCollision player pillar -> True
     | otherwise -> False

horizontalCollision: Player -> Pillar -> Bool  -- still doesnt account for ball width
horizontalCollision player pillar =
 if | (((pillar.x + pillarWidth/2) > player.x) && ((pillar.x - pillarWidth /2) < player.x)) -> True
    | otherwise -> False

verticalCollision: Player -> Pillar -> Bool
verticalCollision player pillar =
 if | (pillar.y > 0.0) && ((pillar.y - pillarSize/2) < player.y) -> True  -- Upper pillar
    | (pillar.y < 0.0) && ((pillar.y + pillarSize/2) > player.y) -> True  -- Lower pillar
    | otherwise -> False

-- SIGNALS
ticker : Signal Time
ticker =
  fps 45

keyAndTick : Signal Bool
keyAndTick = Signal.map2 (\_ isPressed -> isPressed) ticker Keyboard.space

gameState =
  Signal.foldp (\isPressed game ->
    if
      | List.any (within game.player) game.pillars ->
        game
      | otherwise ->
        { game
        | player <- updatePlayerVel game.player game.player.vy isPressed
        , pillars <- updateObstacles game.pillars
        }
  ) defaultGame keyAndTick

main : Signal Element
main =
  Signal.map2 view Window.dimensions gameState

import Debug
import Keyboard
import Maybe
import Maybe (maybe,isJust)
import Random
import Window

-- Model -------------------------------------------------------------

data Direction = Left | Right
data Player    = PlayerA | PlayerB
type Gorilla = 
  { x         : Float 
  , direction : Direction 
  , throwing  : Bool 
  }

type Moveable a = { a | 
    x  : Float
  , y  : Float
  , vx : Float
  , vy : Float 
  }

type Game = 
  { gorillaA : Gorilla 
  , gorillaB : Gorilla 
  , banana   : Maybe Banana 
  , turn     : Maybe Turn
  }

type Banana = 
  { x         : Float 
  , y         : Float
  , vx        : Float 
  , vy        : Float
  , frame     : Int
  , direction : Direction
  , exploding : Bool
  }

type Turn = 
  { player : Player 
  , angle : Int 
  , power: Maybe Float 
  , fired : Bool 
  }

type GameSeed = 
  { width          : Int
  , height         : Int 
  , playerAPosSeed : Float 
  , playerBPosSeed : Float 
  }

type Input = 
  { deltaTime    : Float
  , spacePressed : Bool 
  , upDownDelta  : Int 
  }

gorillaA = { x = -800 , direction = Left , throwing = False }
gorillaB = { x = 800  , direction = Right , throwing = False }
model    = 
  { gorillaA = gorillaA
  , gorillaB = gorillaB 
  , banana = Nothing 
  , turn = Just { player = PlayerA , angle = 45 , power = Nothing , fired = False }
  }

gorillaFromTurn : Game -> Turn -> Gorilla
gorillaFromTurn game t = case t.player of
  PlayerA -> game.gorillaA
  PlayerB -> game.gorillaB    

currentGorilla : Game -> Maybe Gorilla
currentGorilla game = Maybe.map (gorillaFromTurn game) game.turn

modifyCurrentGorilla : (Gorilla -> Gorilla) -> Game -> Game
modifyCurrentGorilla f game = 
  let player = Maybe.map (\t -> t.player) game.turn
  in 
    if player == Just PlayerA
    then { game | gorillaA <- f (game.gorillaA)}
    else if player == Just PlayerB  
    then { game | gorillaA <- f (game.gorillaA)}
    else game

modifyBanana : (Banana -> Banana) -> Game -> Game
modifyBanana f game = { game | banana <- Maybe.map f game.banana }

setBanana : Maybe Banana -> Game -> Game
setBanana b game = { game | banana <- b }

thrownBanana : Int -> Float -> Gorilla -> Banana
thrownBanana angle power g = 
  { x         = g.x 
  , y         = 30 
  , vx        = 20 
  , vy        = 20  
  , frame     = 0 
  , direction = g.direction 
  , exploding = False
  }

throwBanana : Bool -> Game -> Game
throwBanana isSpace game =
  let throwBanana t = thrownBanana t.angle (maybe 1 identity t.power)  (gorillaFromTurn game t)
  in case (isSpace,game.banana) of
    (True,Nothing) -> 
      game 
        |> setBanana (Maybe.map throwBanana game.turn)
        |> modifyCurrentGorilla (\g -> { g | throwing <- True } )
    (_,_) -> game

(>>=) : Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) ma f = maybe Nothing f ma

explodeBanana : Game -> Game
explodeBanana game = 
  let doExplode b = 
    if b.exploding && b.frame >= 3 then Nothing 
    else if b.y <= 0 then Just { b | exploding <- True , vx <- 0 , vy <- 0 }
    else Just b
  in { game | banana <- game.banana >>= doExplode }  

stopThrowAnimation : Game -> Game
stopThrowAnimation = modifyCurrentGorilla (\g -> { g | throwing <- False } ) 
 
stepGame : Input -> Game -> Game
stepGame input game = 
  game 
    |> stopThrowAnimation 
    |> throwBanana input.spacePressed
    |> gravity input.deltaTime
    |> physics input.deltaTime
    |> modifyBanana (\b -> { b | frame <- (b.frame + 1) % 4 } )
    |> explodeBanana 

step : (GameSeed,Input) -> Maybe Game -> Maybe Game
step (seed,input) = maybe (Just model) (stepGame input >> Just)

applyGravity : Float -> Moveable a -> Moveable a
applyGravity dt m = { m | vy <- if m.y > 0 then m.vy - dt else 0 }

gravity : Float -> Game -> Game
gravity dt game = modifyBanana (applyGravity dt) game

applyPhysics : Float -> Moveable a -> Moveable a
applyPhysics dt m = 
  { m | 
    x <- m.x + dt * m.vx,
    y <- max 0 (m.y + dt * m.vy)
  }

physics : Float -> Game -> Game
physics dt game = modifyBanana (applyPhysics dt) game

-- Display -----------------------------------------------------------

directionString : Direction -> String
directionString d = case d of 
  Left  -> "left"
  Right -> "right"

imagePath : String -> Direction -> String -> String
imagePath name dir frame = 
  join "/" ["images",name,directionString dir,frame ++ ".png"]

gorillaForm : Float -> Gorilla -> Form
gorillaForm groundY g =   
  let frame        = if g.throwing then "throwing" else "base"
      src          = imagePath "gorilla" g.direction frame
      gorillaImage = image 60 60 src

  in gorillaImage |> toForm |> move (g.x,groundY)

skyForm : Float -> Float -> Form 
skyForm w h = rect w h |> filled (rgb 174 238 238)

bananaImage : Banana -> Form
bananaImage b = 
  let src = imagePath "banana" b.direction (show b.frame)
  in image 20 20 src |> toForm

bananaExplosion : Banana -> Form
bananaExplosion b =
  let radius = (b.frame + 1) * 10
      grad   = radial (0,0) (radius / 4) (0,0) radius 
        [ (0, rgb 128 0 0)
        , (0.2, rgb 256 0 0)
        , (1,   rgb 256 128 128)
        ] 
  in gradient grad (circle radius) 

bananaForm : Float -> Banana -> Form
bananaForm groundY b =   
  let form = if b.exploding then bananaExplosion b else bananaImage b
  in form |> move (b.x,b.y + groundY)

groundForm : Float -> Float -> Form
groundForm w h =
  rect w 50 |> filled (rgb 74 167 43) |> move (0, 24 - h/2)

gameForms : Float -> Float -> Float -> Game -> [Form]
gameForms w h groundY game = 
  ( gorillaForm groundY game.gorillaA
  :: gorillaForm groundY game.gorillaB
  :: maybe [] (\ x -> [bananaForm groundY x]) game.banana 
  )

display : (Int, Int) -> Maybe Game -> Element
display (w',h') game =
  let (w,h)   = (toFloat w', toFloat h')
      groundY = 62 - h/2
      gFs     = maybe [] (gameForms w h groundY) game
  in collage w' h' (  skyForm w h :: groundForm w h :: gFs )  

-- Input Signals -----------------------------------------------------

input : Signal Input
input = 
  let mkInput dt is ud = { deltaTime = dt , spacePressed = is , upDownDelta = ud }
      sigs = lift3 mkInput time Keyboard.space upDown
  in sampleOn time sigs

upDown : Signal Int
upDown = lift (\x -> x.y) Keyboard.arrows

time : Signal Float
time = lift (\t -> t/20) (fps 25)

gameSeed : Signal GameSeed
gameSeed = 
  let posASeed = Random.float Window.dimensions
      posBSeed = Random.float Window.dimensions
      mkSeed (w,h) a b = 
        { width          = w 
        , height         = h 
        , playerAPosSeed = a 
        , playerBPosSeed = b }
  in 
    lift3 mkSeed Window.dimensions posASeed posBSeed 

gameSignal : Signal (GameSeed,Input)
gameSignal = lift2 (,) gameSeed input

-- Main --------------------------------------------------------------

main : Signal Element
main = lift2 display Window.dimensions (foldp step Nothing gameSignal)
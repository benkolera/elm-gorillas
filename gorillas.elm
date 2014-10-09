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

data TurnState = TurnStart | Firing Int Bool | Fired

type Turn = 
  { player : Player 
  , angle : Int
  , state : TurnState
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

gorillaFromSeed : Int -> Float -> Direction -> Gorilla
gorillaFromSeed fullWidth posSeed dir = 
  let margin = 120
      w = toFloat fullWidth / 2
      posRel = w * posSeed |> max margin |> min (w - margin)
      pos = if dir == Left then 0 - posRel else posRel
  in { x = pos , direction = dir , throwing = False }      

newTurn player = 
  let angle = if player == PlayerA then 275 else 225
  in Just { player = player , angle = 45 , state = TurnStart }

newGame : GameSeed -> Game
newGame seed = 
  { gorillaA = gorillaFromSeed seed.width seed.playerAPosSeed Left
  , gorillaB = gorillaFromSeed seed.width seed.playerBPosSeed Right
  , banana = Nothing 
  , turn = newTurn PlayerA
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
    then { game | gorillaB <- f (game.gorillaB)}
    else game

modifyBanana : (Banana -> Banana) -> Game -> Game
modifyBanana f game = { game | banana <- Maybe.map f game.banana }

setBanana : Maybe Banana -> Game -> Game
setBanana b game = { game | banana <- b }

setTurn : Maybe Turn -> Game -> Game
setTurn t game = { game | turn <- t }
                   
thrownBanana : Int -> Int -> Gorilla -> Banana
thrownBanana angle power g = 
  let (vx,vy) = fromPolar (toFloat power,toFloat angle) 
  in { x         = g.x 
  , y         = 30 
  , vx        = vx 
  , vy        = vy  
  , frame     = 0 
  , direction = g.direction 
  , exploding = False
  } 

stepTurnPower : Float -> Int -> Bool -> TurnState
stepTurnPower dt power increasing = 
   let nextPower = (toFloat power + (dt * if increasing then 1 else -1)) |> round
       nowIncreasing = 
         if nextPower <= 0 then True 
         else if nextPower >= 100 then False 
         else increasing
   in Firing nextPower nowIncreasing 

throwBanana : Float -> Bool -> Game -> Game
throwBanana dt isSpace game =
  case game.turn of
    Nothing   -> game
    Just turn -> 
      let throwBanana' p = thrownBanana turn.angle p (gorillaFromTurn game turn)
      in case (isSpace,turn.state) of
        (True,TurnStart) -> setTurn (Just { turn | state <- Firing 25  True }) game
        (True,Firing p i) -> setTurn (Just { turn | state <- stepTurnPower dt p i }) game
        (False,Firing p _) ->
          game 
            |> setBanana (throwBanana' p |> Just)
            |> modifyCurrentGorilla (\g -> { g | throwing <- True } )
            |> setTurn (Just { turn | state <- Fired })
        _ -> game


explodeBanana : Game -> Game
explodeBanana game = 
  let doExplode b = 
    if b.exploding && b.frame >= 3 then Nothing 
    else if b.y <= 0 then Just { b | exploding <- True , vx <- 0 , vy <- 0 }
    else Just b
  in { game | banana <- game.banana >>= doExplode }  

stopThrowAnimation : Game -> Game
stopThrowAnimation = modifyCurrentGorilla (\g -> { g | throwing <- False } ) 

gorillaOutOfBounds : (GameSeed,Game) -> Bool
gorillaOutOfBounds (seed,game) = 
  let availableWidth = (toFloat seed.width / 2) - 30
  in availableWidth < abs game.gorillaA.x || availableWidth < game.gorillaB.x

stepGame : Input -> Game -> Game
stepGame input game = 
  game 
    |> stopThrowAnimation 
    |> modifyAngle input.deltaTime input.upDownDelta
    |> throwBanana input.deltaTime input.spacePressed
    |> gravity input.deltaTime
    |> physics input.deltaTime
    |> modifyBanana (\b -> { b | frame <- (b.frame + 1) % 4 } )
    |> explodeBanana 
    |> Debug.watch "game"

step : (GameSeed,Input) -> Maybe (GameSeed,Game) -> Maybe (GameSeed,Game)
step (seed,input) = 
  let step' (oldSeed,g) = 
        if gorillaOutOfBounds (seed,g) 
        then newGame' 
        else Just (seed,stepGame input g)
      newGame'          = Just (seed,(newGame seed))
  in maybe newGame' step'

modifyAngle : Float -> Int -> Game -> Game
modifyAngle dt keys game = 
  let modifyAngle' a = toFloat a + (dt * 2 * toFloat keys) |> max 0 |> min 180 |> round
      modifyTurn t = { t | angle <- modifyAngle' t.angle }
  in { game | turn <- Maybe.map modifyTurn game.turn }

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

isTurn : Player -> Game -> Bool
isTurn p game = maybe False (\x -> x.player == p) game.turn

-- Display -----------------------------------------------------------

skyColour    = rgb 174 238 238
groundColour = rgb 74 167 43
redDark      = rgb 128 0 0
red          = rgb 256 0 0
redLight     = rgb 256 128 128

directionString : Direction -> String
directionString d = case d of 
  Left  -> "left"
  Right -> "right"

imagePath : String -> Direction -> String -> String
imagePath name dir frame = 
  join "/" ["images",name,directionString dir,frame ++ ".png"]

angleLine : LineStyle
angleLine = 
  { defaultLine | 
    color   <- red 
    , width <- 5
    , dashing <- [3,3]
  }

gorillaAngle : Int -> Form
gorillaAngle angle = 
  let a = angle |> toFloat |> degrees
  in traced angleLine [(0,0),fromPolar (40,a)]

gorillaForm : Float -> Maybe Int -> Gorilla -> Form
gorillaForm groundY angle g =   
  let frame        = if g.throwing then "throwing" else "base"
      src          = imagePath "gorilla" g.direction frame
      gorillaImage = image 60 60 src |> toForm 
      angleForm    = Maybe.map gorillaAngle angle               
      forms        = gorillaImage :: maybeToList angleForm
  in forms |> collage 80 80 |> toForm |> move (g.x,groundY)

skyForm : Float -> Float -> Form 
skyForm w h = rect w h |> filled skyColour

bananaImage : Banana -> Form
bananaImage b = 
  let src = imagePath "banana" b.direction (show b.frame)
  in image 20 20 src |> toForm

bananaExplosion : Banana -> Form
bananaExplosion b =
  let radius = (b.frame + 1) * 10
      grad   = radial (0,0) (radius / 4) (0,0) radius 
        [(0, redDark),(0.2, red),(1,redLight)] 
  in gradient grad (circle radius) 

bananaForm : Float -> Banana -> Form
bananaForm groundY b =   
  let form = if b.exploding then bananaExplosion b else bananaImage b
  in form |> move (b.x, b.y + groundY)

groundForm : Float -> Float -> Form
groundForm w h =
  rect w 50 |> filled groundColour |> move (0, 24 - h/2)

gameElement : (GameSeed,Game) -> Element
gameElement (seed,game) =
  let groundY = 62 - toFloat seed.height / 2
      w = toFloat seed.width
      h = toFloat seed.height
      a = Maybe.map (\x -> x.angle) game.turn     
      mkGForm = gorillaForm groundY 
  in collage seed.width seed.height 
    ( skyForm w h
    :: groundForm w h
    :: mkGForm (mfilter (always (isTurn PlayerA game)) a) game.gorillaA
    :: mkGForm (mfilter (always (isTurn PlayerB game)) a) game.gorillaB
    :: (game.banana |> Maybe.map (bananaForm groundY) |> maybeToList)
    )

display : Maybe (GameSeed,Game) -> Element
display = maybe empty gameElement

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
main = lift display (foldp step Nothing gameSignal)

-- Extra stuff that haskell spoils me by already having --------------

(>>=) : Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) ma f = maybe Nothing f ma
            
maybeToList : Maybe a -> [a]
maybeToList = maybe [] (\x -> [x])
             
mfilter : (a -> Bool) -> Maybe a -> Maybe a
mfilter f m = m >>= \a -> if f a then Just a else Nothing
import Debug
import Keyboard
import Maybe
import Maybe (maybe,isJust)
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

type Model = 
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

gorillaA = { x = -800 , direction = Left , throwing = False }
gorillaB = { x = 800  , direction = Right , throwing = False }
model    = 
  { gorillaA = gorillaA
  , gorillaB = gorillaB 
  , banana = Nothing 
  , turn = Just { player = PlayerA , angle = 45 , power = Nothing , fired = False }
  }

gorillaFromTurn : Model -> Turn -> Gorilla
gorillaFromTurn game t = case t.player of
  PlayerA -> game.gorillaA
  PlayerB -> game.gorillaB    

currentGorilla : Model -> Maybe Gorilla
currentGorilla game = Maybe.map (gorillaFromTurn game) game.turn

modifyCurrentGorilla : (Gorilla -> Gorilla) -> Model -> Model
modifyCurrentGorilla f game = 
  let player = Maybe.map (\t -> t.player) game.turn
  in 
    if player == Just PlayerA
    then { game | gorillaA <- f (game.gorillaA)}
    else if player == Just PlayerB  
    then { game | gorillaA <- f (game.gorillaA)}
    else game

modifyBanana : (Banana -> Banana) -> Model -> Model
modifyBanana f game = { game | banana <- Maybe.map f game.banana }

setBanana : Maybe Banana -> Model -> Model
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

throwBanana : Bool -> Model -> Model
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

explodeBanana : Model -> Model
explodeBanana game = 
  let doExplode b = 
    if b.exploding && b.frame >= 3 then Nothing 
    else if b.y <= 0 then Just { b | exploding <- True , vx <- 0 , vy <- 0 }
    else Just b
  in { game | banana <- game.banana >>= doExplode }  

stopThrowAnimation : Model -> Model
stopThrowAnimation = modifyCurrentGorilla (\g -> { g | throwing <- False } ) 
 
step : (Float,Bool,Int) -> Model -> Model
step (dt,isSpace,dy) game = 
  game 
    |> stopThrowAnimation 
    |> throwBanana isSpace
    |> gravity dt
    |> physics dt
    |> modifyBanana (\b -> { b | frame <- (b.frame + 1) % 4 } )
    |> explodeBanana 

applyGravity : Float -> Moveable a -> Moveable a
applyGravity dt m = { m | vy <- if m.y > 0 then m.vy - dt else 0 }

gravity : Float -> Model -> Model
gravity dt game =
  { game | banana <- Maybe.map (applyGravity dt) game.banana }

applyPhysics : Float -> Moveable a -> Moveable a
applyPhysics dt m = 
  { m | 
    x <- m.x + dt * m.vx,
    y <- max 0 (m.y + dt * m.vy)
  }

physics : Float -> Model -> Model
physics dt game =
  { game | banana <- Maybe.map (applyPhysics dt) game.banana }

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

display : (Int, Int) -> Model -> Element
display (w',h') m =
  let (w,h)      = (toFloat w', toFloat h')
      groundY    = 62 - h/2
  in collage w' h' 
    (  skyForm w h
    :: groundForm w h
    :: gorillaForm groundY m.gorillaA
    :: gorillaForm groundY m.gorillaB
    :: maybe [] (\ x -> [bananaForm groundY x]) m.banana )
      
-- Input Signals -----------------------------------------------------

input : Signal (Float,Bool,Int)
input = 
  let sigs = lift3 (,,) time Keyboard.space upDown
  in sampleOn time sigs

upDown : Signal Int
upDown = lift (\x -> x.y) Keyboard.arrows

time : Signal Float
time = lift (\t -> t/20) (fps 25)

-- Main --------------------------------------------------------------

main : Signal Element
main = lift2 display Window.dimensions (foldp step model input)
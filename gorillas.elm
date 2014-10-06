import Keyboard
import Maybe (maybe)
import Window

data Direction = Left | Right
type Gorilla = 
  { x         : Float 
  , direction : Direction 
  , throwing  : Bool 
  }

type Model = 
  { gorilla1 : Gorilla 
  , gorilla2 : Gorilla 
  , banana   : Maybe Banana 
  }

type Banana = 
  { x         : Float 
  , y         : Float
  , dx        : Float 
  , dy        : Float
  , frame     : Int
  , direction : Direction
  }

gorilla1 : Gorilla           
gorilla1 = { x = -800 , direction = Left , throwing = False }
gorilla2 : Gorilla           
gorilla2 = { x = 800  , direction = Right , throwing = False }
model : Model
model    = { gorilla1 = gorilla1, gorilla2 = gorilla2 , banana = Nothing }

stepGorilla : Bool -> Gorilla -> Gorilla 
stepGorilla isSpace g = { g | throwing <- isSpace}

step : Bool -> Model -> Model
step isSpace game = 
  { game | gorilla1 <- stepGorilla isSpace game.gorilla1 }

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

bananaForm : Banana -> Form
bananaForm b =   
  let src = imagePath "banana" b.direction (show b.frame)
      img = image 20 20 src
  in img |> toForm |> move (b.x,b.y)

groundForm : Float -> Float -> Form
groundForm w h =
  rect w 50 |> filled (rgb 74 167 43) |> move (0, 24 - h/2)

display : (Int, Int) -> Model -> Element
display (w',h') m =
  let (w,h)      = (toFloat w', toFloat h')
      groundY    = 62 - h/2
  in
    collage w' h' 
      (  skyForm w h
      :: groundForm w h
      :: gorillaForm groundY m.gorilla1
      :: gorillaForm groundY m.gorilla2
      :: maybe [] (\ x -> [bananaForm x]) m.banana )
      
main : Signal Element
main = lift2 display Window.dimensions (foldp step model input)

input : Signal Bool
input = Keyboard.space
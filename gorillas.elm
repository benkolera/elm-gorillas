import Keyboard
import Window

data Direction = Left | Right
type Gorilla = 
  { x        : Float 
  , dir      : Direction 
  , throwing : Bool 
  }

type Model = { gorilla1 : Gorilla }

gorilla = { x = 0 , dir = Left , throwing = False }
model   = { gorilla1 = gorilla }

stepGorilla : Bool -> Gorilla -> Gorilla 
stepGorilla isSpace g = { g | throwing <- isSpace}

step : Bool -> Model -> Model
step isSpace game = { game | gorilla1 <- stepGorilla isSpace game.gorilla1 }

gorillaForm : Float -> Gorilla -> Form
gorillaForm groundY g =   
  let frame        = if g.throwing then "1" else "0"
      src          = "images/gorilla_" ++ frame ++ ".png"
      gorillaImage = image 35 35 src
  in gorillaImage |> toForm |> move (g.x,groundY)

display : (Int, Int) -> Model -> Element
display (w',h') m =
  let (w,h)      = (toFloat w', toFloat h')
      src        = "images/gorilla_0.png"
      groundY    = 62 - h/2
  in
      collage w' h'
          [ rect w h |> filled (rgb 174 238 238)
          , rect w 50
              |> filled (rgb 74 167 43)
              |> move (0, 24 - h/2)
          , gorillaForm groundY m.gorilla1
          ]
      
main : Signal Element
main = lift2 display Window.dimensions (foldp step model input)

input : Signal Bool
input = Keyboard.space
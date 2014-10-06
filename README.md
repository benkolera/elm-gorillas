Elm-Gorilla
===========

A [Gorilla-Like](http://en.wikipedia.org/wiki/Gorillas_(video_game))
game written in ElmJS to learn and demonstrate FRP concepts in a
non-trivial setting.

Hopefully this code sees the light of day as a talk and an associated
hack night at the [Brisbane Functional Programming Group](http://bfpg.org).

Running the code
----------------

* Install the [elm-platform](https://github.com/elm-lang/elm-platform)
* Run elm-reactor in this directory.
* Open up [http://localhost:8000/gorillas.elm](http://localhost:8000/gorillas.elm)

ToDo
----
- Build the gorilla positions from signals (random + window width)
- Show current gorilla visually with angle selection ui element
- Flow cursor keys into step function so that currentPlayer can choose angle
- Hold down space to choose power (and animate the power meter on the angle selection bar)
- Detect explosion collision with gorillas
- Visually distinguish winning and dead gorilla.

Acknowledgements
----------------

Game assets have been borrowed from:

http://www.java-online.ch/gamegrid/index.php?inhalt_mitte=simulationen/gorillagame.inc.php

Though given these have been published without any visible license I should create my
own assets sometime soon.

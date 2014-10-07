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
* Show current gorilla visually with angle selection ui element
* Modify current angle with dy input.
* Hold down space to choose power (and animate the power meter on the angle selection bar)
* Detect explosion collision with gorillas
* Visually distinguish winning and dead gorilla.

Notes
-----
* Floats are being used for the coords because that's what Graphics.Collage does. Still dunno why. Pixels are indivisible, right?
* Why am I using records for everything. It's getting annoying.

Acknowledgements
----------------

Game assets have been borrowed from:

http://www.java-online.ch/gamegrid/index.php?inhalt_mitte=simulationen/gorillagame.inc.php

Though given these have been published without any visible license I should create my
own assets sometime soon.

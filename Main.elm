import Game exposing (view, gameState)
import Window

main =
  Signal.map2 view Window.dimensions gameState

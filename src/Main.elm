module Main where

import List
import Array
import Color (..)
import Graphics.Element (..)
import Graphics.Collage (..)

type alias Edge = (Int, Int)
type alias Point = (Float, Float)
type alias Model =
  { edges: List Edge
  , points: Array.Array Point
  }

init : Model
init =
  { edges= [(0,1), (0,2), (2,1)]
  , points= Array.fromList [(0.5, 0.2), (0.3, 0.6), (0.8, 0.65)]
  }

background w h = [ rect (toFloat w) (toFloat h) |> filled darkOrange ]

node w h (x,y) = oval 35 35 |> filled darkBrown |> move (((x-0.5) * (toFloat w)), ((y-0.5) * (toFloat h)))

nodes w h points = Array.map (node w h) points

render : (Int,Int) -> Model -> Element
render (w,h) m =
  [ background w h
  , nodes w h m.points |> Array.toList
  ]
  |> List.concat
  |> collage w h

main = render (800,600) init

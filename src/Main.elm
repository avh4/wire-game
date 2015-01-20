module Main where

import List
import Array
import Color (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import Window
import Mouse
import Signal
import Debug

type alias Edge = (Int, Int)
type alias Point = (Float, Float)
type alias Model =
  { edges: List Edge
  , points: Array.Array Point
  , selected: Maybe Int
  }

init : Model
init =
  { edges= [(0,1), (0,2), (2,1)]
  , points= Array.fromList [(0.5, 0.2), (0.3, 0.6), (0.8, 0.65)]
  , selected= Just 0
  }

type Command
  = Move (Float,Float)
  | Release
  | NoOp

update : Command -> Model -> Model
update c m = case c of
  NoOp -> m
  Move p' -> case m.selected of
    Nothing -> m
    Just i -> { m | points <- Array.set i p' m.points }
  Release -> { m | selected <- Nothing }

background w h = [ rect (toFloat w) (toFloat h) |> filled darkOrange ]

conv : Int -> Float -> Float
conv w x = (x-0.5) * (toFloat w)

deconv : Int -> Int -> Float
deconv w x = (toFloat x) / (toFloat w)

node w h sel i (x,y) = oval 35 35
  |> filled (if sel == (Just i) then lightBrown else darkBrown)
  |> move (conv w x, conv -h y)

lookupPoint points p = case Array.get p points of
  Just p' -> p'
  Nothing -> Debug.crash ("Unknown point: " ++ toString p)

lookupEdge points (a,b) =
  ( lookupPoint points a
  , lookupPoint points b
  )

drawEdge : Int -> Int -> ((Float,Float), (Float,Float)) -> Form
drawEdge w h ((x1,y1),(x2,y2)) =
  segment (conv w x1, conv -h y1) (conv w x2, conv -h y2)
  |> traced (solid darkBrown)

render : (Int,Int) -> Model -> Element
render (w,h) m =
  [ background w h
  , m.edges
    |> List.map (lookupEdge m.points)
    |> List.map (drawEdge w h)
  , m.points
    |> Array.indexedMap (node w h m.selected)
    |> Array.toList
  ]
  |> List.concat
  |> collage w h

commands : Signal Command
commands = Signal.mergeMany
  [ Mouse.position
    |> Signal.map2 (\(w,h) (x,y) -> Move (deconv w x, deconv h y)) Window.dimensions
  , Mouse.clicks |> Signal.map (\_ -> Release)
  ]

state : Signal Model
state = Signal.foldp update init commands

main = Signal.map2 render Window.dimensions state

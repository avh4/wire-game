module Main where

import List
import Array
import Color (..)
import Svg (..)
import Svg.Attributes (..)
import Svg.Attributes as A
import Svg.Events (..)
import Window
import Mouse
import Signal
import Debug
import Html (Html)
import Random as R

type alias Edge = (Int, Int)
type alias Point = (Float, Float)
type alias Model =
  { edges: List Edge
  , points: Array.Array Point
  , selected: Maybe Int
  }

init : R.Seed -> Model
init seed =
  { edges= [(0,1), (0,2), (0,3), (0,4), (1,2), (1,3), (1,4), (2,1), (3,1), (3,2), (3,0), (4,2), (4,0)]
  , points= R.list 5 (R.pair (R.float 0.2 0.8) (R.float 0.2 0.8))
    |> (flip R.generate) seed |> fst
    |> Array.fromList
  , selected= Nothing
  }

type Command
  = Move (Float,Float)
  | Select Int
  | Release
  | Reset R.Seed
  | NoOp

update : Command -> Model -> Model
update c m = case c of
  NoOp -> m
  Move p' -> case m.selected of
    Nothing -> m
    Just i -> { m | points <- Array.set i p' m.points }
  Release -> { m | selected <- Nothing }
  Select i -> { m | selected <- Just i }
  Reset seed -> init seed

commandsChannel : Signal.Channel Command
commandsChannel = Signal.channel NoOp

background col w h =
  [ rect
    [ fill col
    , x "0", y "0"
    , width (toString w), height (toString h)
    ] []
  ]

conv : Int -> Float -> Float
conv w x = (x) * (toFloat w)

deconv : Int -> Int -> Float
deconv w x = (toFloat x) / (toFloat w)

node : Int -> Int -> Maybe Int -> (Int, (Float,Float)) -> Svg
node w h sel (i,(x',y')) = circle
  [ if sel == (Just i) then fill "#c17d11" else fill "#8f5902"
  , cx (toString <| conv w x')
  , cy (toString <| conv h y')
  , r "20"
  , onMouseDown (Signal.send commandsChannel (Select i))
  ] []

lookupPoint points p = case Array.get p points of
  Just p' -> p'
  Nothing -> Debug.crash ("Unknown point: " ++ toString p)

lookupEdge points (a,b) =
  ( lookupPoint points a
  , lookupPoint points b
  )

drawEdge : Int -> Int -> ((Float,Float), (Float,Float)) -> Svg
drawEdge w h ((x1',y1'),(x2',y2')) = line
  [ x1 (toString <| conv w x1')
  , x2 (toString <| conv w x2')
  , y1 (toString <| conv h y1')
  , y2 (toString <| conv h y2')
  , A.style "stroke: #8f5902"
  ] []

area2 : (Float,Float) -> (Float,Float) -> (Float,Float) -> Float
area2 (ax,ay) (bx,by) (cx,cy) = (bx-ax)*(cy-ay) - (cx-ax)*(by-ay)

isLeft a b c = area2 a b c > 0

intersects : Array.Array Point -> Edge -> Edge -> Bool
intersects points a b = case (lookupEdge points a, lookupEdge points b) of
  ((a1,a2), (b1,b2)) -> if
    | a1 == b1 || a1 == b2 -> False
    | a2 == b1 || a2 == b2 -> False
    | otherwise ->
      (isLeft a1 a2 b1 `xor` isLeft a1 a2 b2)
      && (isLeft b1 b2 a1 `xor` isLeft b1 b2 a2)

noEdgesCross : Array.Array Point -> List Edge -> Bool
noEdgesCross points edges = case edges of
  [] -> True
  (next::rest) -> case List.any (intersects points next) rest of
    True -> False
    False -> noEdgesCross points rest

noPointsOverlap points = case points of
  [] -> True
  (next::rest) -> case List.any ((==) next) rest of
    True -> False
    False -> noPointsOverlap rest

win : Array.Array Point -> List Edge -> Bool
win points edges = noPointsOverlap (Array.toList points) && noEdgesCross points edges

render : (Int,Int) -> Model -> Html
render (w,h) m =
  [ background "#fcaf3e" w h
  , if win m.points m.edges then background "#affc3e" w h else []
  , m.edges
    |> List.map (lookupEdge m.points)
    |> List.map (drawEdge w h)
  , m.points
    |> Array.toIndexedList
    |> List.map (node w h m.selected)
  ]
  |> List.concat
  |> svg
    [ version "1.1", x "0", y "0"
    , viewBox ("0 0 " ++ toString w ++ " " ++ toString h)
    , A.style "display:block"
    ]

commands : Signal Command
commands = Signal.mergeMany
  [ Mouse.position
    |> Signal.map2 (\(w,h) (x,y) -> Move (deconv w x, deconv h y)) Window.dimensions
  , Mouse.clicks |> Signal.map (\_ -> Release)
  , Signal.subscribe commandsChannel
  ]

state : Signal Model
state = Signal.foldp update (init <| R.randomSeed) commands

main = Signal.map2 render Window.dimensions state

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
  , selected= Nothing
  }

type Command
  = Move (Float,Float)
  | Select Int
  | Release
  | NoOp

update : Command -> Model -> Model
update c m = case c of
  NoOp -> m
  Move p' -> case m.selected of
    Nothing -> m
    Just i -> { m | points <- Array.set i p' m.points }
  Release -> { m | selected <- Nothing }
  Select i -> { m | selected <- Just i }

commandsChannel : Signal.Channel Command
commandsChannel = Signal.channel NoOp

background w h =
  [ rect
    [ fill "#fcaf3e"
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

render : (Int,Int) -> Model -> Html
render (w,h) m =
  [ background w h
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
    ]

commands : Signal Command
commands = Signal.mergeMany
  [ Mouse.position
    |> Signal.map2 (\(w,h) (x,y) -> Move (deconv w x, deconv h y)) Window.dimensions
  , Mouse.clicks |> Signal.map (\_ -> Release)
  , Signal.subscribe commandsChannel
  ]

state : Signal Model
state = Signal.foldp update init commands

main = Signal.map2 render Window.dimensions state

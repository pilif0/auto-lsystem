-- Filip Smola
-- Examples of LSystems for the Renderer

import Renderer
import LSystem

-- Levy C curve
-- params: iterations
levy :: Int -> Command
levy n = construct n base key
  where
    base = "--f"
    key = makeKeyEntries chars trans coms ++ rotations 45
    chars = ['f']
    trans = ["+f--f+"]
    coms = [GrabPen black :#: Go 10]

-- Dragon curve
-- params: iterations
dragon :: Int -> Command
dragon n = construct n base key
  where
    base = "fx"
    key = makeKeyEntries chars trans coms ++ rotations 90
    chars = ['x','y','f']
    trans = ["x+yf+","-fx-y","f"]
    coms = [GrabPen black :#: Go 10, GrabPen red :#: Go 10, Go 10]

-- Pythagoras tree
-- params: iterations
pythTree :: Int -> Command
pythTree n = construct n base key
  where
    base = "0"
    key = makeKeyEntries chars trans coms ++ rotations 45 ++ branches
    chars = ['0','1']
    trans = ["1[-0]+0","11"]
    coms = [GrabPen green :#: Go 10, GrabPen black :#: Go 10]

-- Cantor set
-- params: iterations
cantor :: Int -> Command
cantor n = construct n base key
  where
    base = "a"
    key = makeKeyEntries chars trans coms
    chars = ['a','b']
    trans = ["aba","bbb"]
    coms = [GrabPen black :#: Go 10, GrabPen Inkless :#: Go 10]

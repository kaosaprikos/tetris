{- |
Module      : Shapes
Description : Types and functions for shapes. The list of all tetris pieces.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
Student names: Harsimat Kour, Alba Mori Wallin, Amanda Juarez Andino 
Grupp nummer: 22
-}

module Shapes where

import Data.List (transpose)
import Data.Maybe (isNothing)
import Test.QuickCheck

-- * Shapes

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
  deriving (Eq, Bounded, Enum, Show)

type Square = Maybe Colour

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

type Row   = [Square]
data Shape = Shape [Row] deriving Eq

rows :: Shape -> [Row]
rows (Shape rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape (Shape rows) = unlines [showRow r | r <- rows]
 where
  showRow r = [showSquare s | s <- r]
    
  showSquare Nothing      = '.'
  showSquare (Just Black) = '#' -- can change to '█' on linux/mac
  showSquare (Just Grey)  = 'g' -- can change to '▓'
  showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss) ++ r

-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of 4 connected blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [Shape (makeSquares s) | s <- shapes] 
 where
   makeSquares = map (map colour)
   colour c    = lookup c [ ('I', Red),  ('J', Grey),  ('T', Blue), ('O', Yellow)
                          , ('Z', Cyan), ('L', Green), ('S', Purple) ]
   shapes = [["I",
              "I",
              "I",
              "I"],
             [" J",
              " J",
              "JJ"],
             [" T",
              "TT",
              " T"],
             ["OO",
              "OO"],
             [" Z",
              "ZZ",
              "Z "],
             ["LL",
              " L",
              " L"],
             ["S ",
              "SS",
              " S"]]

-- * Some simple functions


--Used for A1, A8, A9
rep :: Int -> Int -> [[Maybe a]]
rep a b = replicate a (replicate b Nothing)

-- ** A1
-- ** A1
emptyShape :: (Int, Int) -> Shape
emptyShape (x, y) = Shape (rep x y)


-- ** A2

-- | The size (height and width) of a shape (rows x columns)
shapeSize :: Shape -> (Int, Int)
shapeSize (Shape []) = (0,0)
shapeSize (Shape x)  = (length x, length (x !! 0))

-- ** A3

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (Shape xs) = sum[1 | x <- (concat xs), x /= Nothing]

-- * The Shape invariant

-- ** A4
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape (Shape rs)
  | null rs = False
  | otherwise = all (== length (head rs)) (map length rs)

-- * Test data generators

-- ** A5
genColour :: Gen Colour
genColour = elements [Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey]

instance Arbitrary Colour where
  arbitrary = genColour

-- ** A6
-- | A random generator for shapes
genShape :: Gen Shape
genShape = elements allShapes

instance Arbitrary Shape where
  arbitrary = genShape

-- * Transforming shapes

-- ** A7
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (Shape x) = Shape (reverse(transpose x))


-- ** A8
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (a, b) (Shape x) = Shape (right b (down a (x))) 
  where right b x = map (replicate b Nothing ++) x
        down  a x = rep a (length (x !! 0)) ++ x

-- ** A9
-- | padShape adds empty square below and to the right of the shape
padShape :: (Int, Int) -> Shape -> Shape
padShape (a, b) (Shape x) = Shape (left b (up a (x))) 
  where left b x = map (++ replicate b Nothing) x
        up   a x = x ++ rep a (length (x !! 0))

-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (a, b) (Shape x) = padShape ((a-a'), (b-b')) (Shape x)
  where 
    (a', b') = shapeSize (Shape x)

-- * Comparing and combining shapes
B1
-- | Test if two shapes overlap

rowsOverlap :: Row -> Row -> Bool
rowsOverlap r1 r2 = or $ zipWith bothFilled r1 r2
  where
    bothFilled :: Square -> Square -> Bool
    bothFilled (Just _) (Just _) = True
    bothFilled _ _               = False
overlaps :: Shape -> Shape -> Bool
overlaps (Shape rs1) (Shape rs2) = or $ zipWith rowsOverlap rs1 rs2
  
-- ** B2
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
zipShapeWith f (Shape rs1) (Shape rs2) = Shape (zipWith (zipWith f) rs1 rs2)


-- ** B3
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
combine s1 s2
  | overlaps s1p s2p = error "Shapes overlap!"
  | otherwise = zipShapeWith combineSquares s1p s2p
  where
    (h1, w1) = shapeSize s1
    (h2, w2) = shapeSize s2
    h = max h1 h2
    w = max w1 w2
    s1p = padShapeTo (h, w) s1
    s2p = padShapeTo (h, w) s2
    combineSquares :: Square -> Square -> Square
    combineSquares sq1 sq2 = sq1 <|> sq2

drawTetris  :: Tetris -> Shape
startTetris :: [Double] -> Tetris
stepTeris   :: Action -> Tetris -> Maybe (Int,Tetris)

-- B4 
--Define a property that checks the following things:

--that the falling shape in the well satisfies the Shape Invariant (prop_Shape),
--that the size of the well is correct, i.e. equal to wellSize.
prop_Tetris :: Tetris -> Bool
prop_Tetris t = prop_Shape (fallingShape t)
             && prop_Shape (well t)
             && shapeSize (well t) == wellSize


--B5 
--Write a function that adds black walls around a given shape.
addWalls :: Shape -> Shape
addWalls shape = topBottomRow : map addWallsToRow shape ++ [topBottomRow]
  where
    width = length (head shape) + 2
    topBottomRow = replicate width '#'
    addWallsToRow row = '#' : row ++ "#"
--B6
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris action t = Just (0, t) -- incomplete !!!
drawTetris :: Tetris -> Shape
drawTetris t = addWalls combinedShape
  where
    shiftedFalling = shiftShape (pos t) (fallingShape t)
    combinedShape = combine (well t) shiftedFalling

--B7
move :: (Int, Int) -> Tetris -> Tetris
move delta t = setPos newPos t
  where
    newPos = add delta (pos t)

--B8
tick :: Tetris -> Maybe (Int, Tetris)
tick t = Just (0, move (1, 0) t)
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris Tick t = tick t
stepTetris _ t = Just (0, t) -- other actions not handled yet


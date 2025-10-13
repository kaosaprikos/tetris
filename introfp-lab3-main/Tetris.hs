
{- |
Module      : Tetris
Description : The Tetris game (main module)
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental
-}

module Tetris where

import ConsoleGUI       -- cabal install ansi-terminal 
import Shapes

--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main ::IO ()
main = runGame tetrisGame

tetrisGame :: Game Tetris
tetrisGame = Game 
  { startGame     = startTetris
  , stepGame      = stepTetris
  , drawGame      = drawTetris
  , gameInfo      = defaultGameInfo prop_Tetris
  , tickDelay     = defaultDelay
  , gameInvariant = prop_Tetris
  }

shape1 :: Shape
shape1 = Shape [[Just Red, Just Red, Nothing],[Nothing, Just Red, Just Red]]
--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation

type Pos   = (Int, Int)    -- (row index, col index)
type Piece = (Pos, Shape)
type Well  = Shape

-- | The state of the game consists of three parts:
data Tetris = Tetris 
  { piece  :: Piece    -- ^ The position and shape of the falling piece
  , well   :: Shape    -- ^ The well (the playing field), where the falling pieces pile up
  , shapes :: [Shape]  -- ^ An infinite supply of random shapes
  }

-- | The size of the well
-- | The size of the well
wellSize :: (Int, Int)
wellSize = (wellHeight, wellWidth)
wellWidth  = 10
wellHeight = 20

-- | Starting position for falling pieces
startPosition :: Pos
startPosition = (0, wellWidth `div` 2 - 1)

-- | Pos addition
add :: Pos -> Pos -> Pos
(h1, w1) `add` (h2, w2) = (h1 + h2, w1 + w2)

-- | Move the falling piece into position
place :: Piece -> Shape
place (v, s) = shiftShape v s

--B4

--a property that checks the following things:
  --that the falling shape in the well satisfies the Shape Invariant (prop_Shape),
  --that the size of the well is correct, i.e. equal to wellSize.
prop_Tetris :: Tetris -> Bool
prop_Tetris t = prop_Shape shape1 && wellSize == shapeSize (well t)
  where
    (pos, shape1) = piece t


--B5
-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls (Shape r) = Shape (firstLast2 [blackList] (firstLast1 r))
  where
    blackList = replicate width (Just Black)
    width = length (head r) + 2
    --length (head r) = width of the first row
    -- + 2 = add one black cell on each side (left + right)
    firstLast1 r = map ([Just Black] ++) (map (++ [Just Black]) r) 
    --map (++ [Just Black]) r → adds black on the right
    --map ([Just Black] ++) ... → adds black on the left
    firstLast2 ad r = ad ++ r ++ ad
    -- This adds one black row at the top and one at the bottom

--B6
-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris piece well _) = addWalls (combine (shiftShape pos pshape) well)
  where
    (pos, pshape) = piece 


-- | The initial game state
{-startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition, piece) well supply
 where
  well         = emptyShape wellSize
  piece:supply = repeat (allShapes !! 1) -}
  
-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
{-stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris MoveLeft t = Just (0, move (0,-1) t)
stepTetris MoveRight t = Just (0, move (0,1) t) 
stepTetris Rotate (Tetris piece w s) = Just (0, Tetris (pos, rotateShape shape) w s)
  where 
    (pos, shape) = piece
stepTetris MoveDown t  = tick t
stepTetris _ t = tick t-}

 
--B7
--  a function to move the falling piece
move :: (Int, Int) -> Tetris -> Tetris
move pos1 (Tetris piece w s) = Tetris (add pos1 pos2, x) w s
  where 
    (pos2, x) = piece


--B8
-- the piece should fall one row per tick
{-tick :: Tetris -> Maybe (Int, Tetris)
tick t = Just (0, move (1,0) t)-}

-- C1
-- Check if the falling piece has collided with walls or well

collision :: Tetris -> Bool
collision (Tetris piece well _) =
  let (row, col) = fst piece         -- original position of the piece
      shape = snd piece              -- shape of original piece
      -- gets dimensions of the shape and the well
      (height, width) = shapeSize shape
      (wellHeight, wellWidth) = shapeSize well
      placedShape = place piece
  in
     col < 0                             -- too far left
  || col + width > wellWidth             -- too far right
  || row + height > wellHeight           -- too far down
  || overlaps well placedShape           -- overlaps with existing well blocks

-- Improved tick function with collision check
tick :: Tetris -> Maybe (Int, Tetris)
tick t =
  let newTetris = move (1, 0) t          -- move piece down by one row
  in if collision newTetris              -- check for collision
     then dropNewPiece t                 -- if collision, drop the piece and add a new one
     else Just (0, newTetris)


--C2
-- rewrite stepTetris to handle the MoveDown action

stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris MoveLeft t  = Just (0, movePiece (-1) t)  
stepTetris MoveRight t = Just (0, movePiece 1 t)
stepTetris Rotate t = Just (0, rotatePiece t)
stepTetris MoveDown t  = tick t
stepTetris _ t        = tick t


--C3
-- We define a function that can be called from stepTetris to handle the MoveLeft and MoveRight actions
-- movePiece takes an Int (-1 for left, 1 for right) and tries to move the piece horizontally
movePiece :: Int -> Tetris -> Tetris
movePiece dir t =
  let moved = move (0, dir) t      -- move left or right
  in if collision moved            -- check for collision
     then t                        -- if collision, stay in old position
     else moved                    -- else accept the new position


--C4
-- We define a function that rotates the falling piece
rotate :: Tetris -> Tetris
rotate (Tetris (pos, shape) well shapes) = Tetris (pos, rotateShape shape) well shapes 
-- (pos, shape): the current falling piece, where pos is the piece’s position (row, col) inside the well,
-- shape is the shape of the piece.
-- well the current well (playing field).
-- shapes the list of upcoming shapes (the supply).

--C5
--C6
-- checks if the rotation causes collision 
rotatePiece :: Tetris -> Tetris
rotatePiece t =
  let rotated = rotate t    
  in if collision rotated
     then t
     else rotated

--C7
-- a function that handles the case when the falling piece can’t move any further down.
dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece (Tetris piece well (x:xs)) 

 | overlaps newShape well = Nothing
 | otherwise = Just (n, (Tetris newPiece newWell xs))
  where  
    newShape = shiftShape startPosition x
    newPiece = (startPosition, x)
    newWell = (combine clearedWell (place (piece))) 
    (n, clearedWell) = clearLines well 



--C8
--Modify the startTetris function so that it uses this parameter to create a list of random shapes for the supply.
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition, piece) well list
 where
  well         = emptyShape wellSize -- Creates an empty grid (the Tetris well) of a given size
  piece:list   = shapeList rs -- shapeList rs returns a list of random shapes, and the pattern piece:list splits it into:
                              -- piece = the first shape (the one falling now)
                              -- list = the rest (the queue of upcoming shapes)


shapeList :: [Double] -> [Shape]
shapeList [] = []
shapeList (x:xs)
 | f x == length allShapes = (allShapes !! (f x - 1)) : shapeList xs 
 -- If f x happens to equal length allShapes (which could happen if x == 1.0),
 -- it subtracts 1 to avoid going out of bounds.
 | otherwise = (allShapes !! f x) : shapeList xs
  where  
    f x = floor (fromIntegral (length allShapes) * x) 

 

--C9
-- a function that removes completed lines from the well.

isComplete :: Row -> Bool
isComplete rw = all (/= Nothing) rw  -- Check that every element in the row is not equal to Nothing

clearLines :: Shape -> (Int, Shape)
clearLines (Shape rows) = 
  let
    isCompleterows = filter (not.(isComplete)) rows      -- this line keeps only incomplete rows (removes the filled ones)
    clearedCount = length rows - length isCompleterows   -- How many rows are cleared 
    emptyRow = replicate wellWidth Nothing               -- Adds new empty rows to the well 
    newRows = replicate clearedCount emptyRow ++ isCompleterows -- adds the rows that were removed 
  in 
    (clearedCount, Shape newRows)  -- returns how many rows were removed and updates the well

      
                        
 

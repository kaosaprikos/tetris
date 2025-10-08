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
wellSize   = (wellWidth, wellHeight)
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

-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris t = prop_Shape shape1 && wellSize == shapeSize (well t)
  where
    (pos, shape1) = piece t

-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls (Shape r) = Shape (firstLast2 [blackList] (firstLast1 r))
  where
    blackList = replicate width (Just Black)
    width = length (head r) + 2
    firstLast1 r = map ([Just Black] ++) (map (++ [Just Black]) r) 
    firstLast2 ad r = ad ++ r ++ ad


-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris piece well _) = addWalls (combine (shiftShape pos pshape) well)
  where
    (pos, pshape) = piece
    int2 = div wellWidth 2 - 1


-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition, piece) well supply
 where
  well         = emptyShape wellSize
  piece:supply = repeat (allShapes !! 1) -- incomplete !!!

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris (MoveLeft) t = Just (0, move (0,1) t)
stepTetris (MoveRight) t = Just (0, move (0,1) t) 
stepTetris (Rotate) (Tetris piece w s) = Just (0, Tetris (pos, rotateShape shape) w s)
  where 
    (pos, shape) = piece
stepTetris _ t = tick t



move :: (Int, Int) -> Tetris -> Tetris
move pos1 (Tetris piece w s) = Tetris (add pos1 pos2, x) w s
  where 
    (pos2, x) = piece

tick :: Tetris -> Maybe (Int, Tetris)
tick t = Just (0, move (1,0) t)

-- C1
-- Check if the falling piece has collided with walls or well


collision :: Tetris -> Bool
collision (Tetris piece well _) =
  let (row, col) = fst piece         -- original position of the piece
      shape = snd piece             -- shape of original piece
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
stepTetris Rotate t = Just (0, rotate t)
stepTetris MoveDown t  = tick t
stepTetris _ t        = tick t

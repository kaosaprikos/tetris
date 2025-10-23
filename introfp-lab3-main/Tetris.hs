
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

 | overlaps newShape clearedWell = Nothing
 | otherwise = Just (n, (Tetris newPiece clearedWell xs))
  where  
    newShape = shiftShape startPosition x
    newPiece = (startPosition, x)
    newWell = (combine well (place (piece))) 
    (n, clearedWell) = clearLines newWell 



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



    {- |
Module      : Simplify
Description : Skeleton for Lab 4: simplifying polynomials.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : <Alba Mori Wallin, Amanda Andino Juarez, Harsimrat Kour>
Lab group   : <group 22>
-}

module Simplify where

import Parser
import Poly
import Test.QuickCheck
import Data.Maybe
import System.Directory (doesFileExist)
import System.Random (randomRIO)
import Test.QuickCheck (generate, vectorOf, choose)
import Control.Monad (forever)

-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp deriving Eq

--------------------------------------------------------------------------------
-- * A1

{-PROMPT

Ask the coding assistant to define a data type Expr to represent arithmetic expressions. 
The data type should be able to represent, integers, expressions built using addition and multiplication, 
and integer powers of a single variable e.g. x2, x9, x10 etc. All integers 
here should make use of Haskell’s Int type. For binary operators it should use a single 
constructor and you must make use of the helper data type BinOp:

data BinOp = AddOp | MulOp
It should not be able to represent exponential expressions which are anything except x raised 
to some integer power, so for example you should not be able to represent x(1 + 1), 2x, 23, 
etc. Similarly, it should not be able to represent expressions with more than one variable, 
so you should not be able to represent x + y.-}


-- Expression type
data Expr
  = Const Int                -- Integer constant
  | VarPow Int               -- x raised to a non-negative integer power (e.g., x^2, x^9)
  | BinExpr BinOp Expr Expr  -- Addition or multiplication of two expressions
  deriving (Eq)

--------------------------------------------------------------------------------
-- * A2


prop_expr :: Expr -> Bool
prop_expr (Const _)         = True
prop_expr (VarPow n)        = n >= 0
prop_expr (BinExpr _ e1 e2) = prop_expr e1 && prop_expr e2

exp8 = VarPow 8
exp9 = VarPow (-6)
exp10 = BinExpr AddOp (VarPow 8) (VarPow (-9))
exp11 = BinExpr MulOp (VarPow 10) (VarPow 3)




--------------------------------------------------------------------------------
-- * A3

{- PROMPT

Use the coding assistant to make Expr an instance of Show, which makes sure that 
only brackets are added where they are needed. Use Haskell notation for exponents e.g. x^2. 
The instance should show x1 as just x, but otherwise it should not simplify your expressions 
in any way before it shows them.-}

instance Show BinOp where
  show AddOp = "+"
  show MulOp = "*"

instance Show Expr where
  show = showExpr

-- Helper to determine precedence
precedence :: BinOp -> Int
precedence AddOp = 1
precedence MulOp = 2

-- Show expression with minimal parentheses
showExpr :: Expr -> String
showExpr = go 0
  where
    go _ (Const n) = show n
    go _ (VarPow 1) = "x"
    go _ (VarPow n) = "x^" ++ show n
    go p (BinExpr op e1 e2) =
      let opPrec = precedence op
          left = go opPrec e1
          right = go (opPrec + 1) e2
          inner = left ++ " " ++ show op ++ " " ++ right
      in if opPrec < p then "(" ++ inner ++ ")" else inner

exp1 = Const 5
exp2 = VarPow 1
exp3 = VarPow 8
exp4 = BinExpr AddOp exp1 exp2
exp5 = BinExpr MulOp exp4 exp1
exp6 = BinExpr AddOp exp5 exp4
exp7 = BinExpr MulOp exp5 exp6
 
--------------------------------------------------------------------------------
-- * A4

{- PROMPT

Let the coding assistant make Expr an instance of Arbitrary and check the data 
type invariant that you defined in A2 using QuickCheck. 
You should make sure that the generated expressions are not too big. 
Hint: we discussed in a lecture how to write a generator that takes the size 
of the to be generated expression as an argument. Pass this information on to 
the coding assistant.-}

-- Must come before genExpr
instance Arbitrary BinOp where
  arbitrary = elements [AddOp, MulOp]


instance Arbitrary Expr where
  arbitrary = sized genExpr

genExpr :: Int -> Gen Expr
genExpr 0 = oneof [Const <$> genInt, VarPow <$> choose (0, 5)]
genExpr n = frequency
  [ (1, Const <$> genInt)
  , (1, VarPow <$> choose (0, 5))
  , (2, do
        op <- arbitrary  -- This now works because BinOp has an Arbitrary instance
        let n' = n `div` 2
        e1 <- genExpr n'
        e2 <- genExpr n'
        return $ BinExpr op e1 e2)
  ]


-- Helper to avoid ambiguity
genInt :: Gen Int
genInt = Test.QuickCheck.arbitrary


--------------------------------------------------------------------------------
-- * A5


{- PROMPT 

Instruct the coding assistant to define a function eval :: Int -> Expr -> Int 
that takes a value for x and an expression and evaluates it.-}


-- Evaluate an expression given a value for x
eval :: Int -> Expr -> Int
eval _ (Const n) = n
eval x (VarPow n) = x ^ n
eval x (BinExpr op e1 e2) =
  let v1 = eval x e1
      v2 = eval x e2
  in case op of
       AddOp -> v1 + v2
       MulOp -> v1 * v2 

-- Write properties!

prop_eval_add :: Int -> Expr -> Expr -> Bool
prop_eval_add x e1 e2 = (eval x e1) + (eval x e2) == eval x (BinExpr AddOp e1 e2)

prop_eval_mul :: Int -> Expr -> Expr -> Bool
prop_eval_mul x e1 e2 = (eval x e1) * (eval x e2) == eval x (BinExpr MulOp e1 e2)
--------------------------------------------------------------------------------
-- * A6

{- PROMT

Ask the coding assistant to define exprToPoly :: Expr -> Poly that converts an expression into a polynomial. -}


exprToPoly :: Expr -> Poly
exprToPoly (Const n) = fromInteger (toInteger n)
exprToPoly (VarPow k) = fromList (replicate k 0 ++ [1])
exprToPoly (BinExpr AddOp e1 e2) = exprToPoly e1 + exprToPoly e2
exprToPoly (BinExpr MulOp e1 e2) = exprToPoly e1 * exprToPoly e2

-- Define (and check) prop_exprToPoly, which checks that evaluating the
-- polynomial you get from exprToPoly gives the same answer as evaluating
-- the expression.

prop_exprToPoly :: Int -> Expr -> Bool
prop_exprToPoly x expr = eval x expr == evalPoly x (exprToPoly expr)

--------------------------------------------------------------------------------
-- * A7
-- Now define the function going in the other direction.

{- PROMT
Continue with the generation of a function going in the other direction, 
polyToExpr :: Poly -> Expr. Instruct the coding assistant to use “smart constructors” 
to ensure that you don’t introduce “junk” like multiplication by 1 in your result. 
We covered such smart constructors in week 6.-}

-- Smart constructor for constants
mkConst :: Int -> Maybe Expr
mkConst 0 = Nothing
mkConst n = Just (Const n)

-- Smart constructor for variable powers
mkVarPow :: Int -> Maybe Expr
mkVarPow 0 = Nothing
mkVarPow 1 = Just (VarPow 1)
mkVarPow n = Just (VarPow n)

-- Smart constructor for multiplication
mkMul :: Expr -> Expr -> Expr
mkMul (Const 1) e = e
mkMul e (Const 1) = e
mkMul e1 e2       = BinExpr MulOp e1 e2

-- Smart constructor for addition
mkAdd :: Expr -> Expr -> Expr
mkAdd (Const 0) e = e
mkAdd e (Const 0) = e
mkAdd e1 e2       = BinExpr AddOp e1 e2

-- ...existing code...
polyToExpr :: Poly -> Expr
polyToExpr p =
  case catMaybes terms of
    []     -> Const 0
    (t:ts) -> foldl mkAddExpr t ts
  where
    -- toList returns coefficients most-significant-first, so reverse to get
    -- little-endian [c0,c1,c2,...] where index = power
    coeffs = reverse (toList p)
    terms  = [term c pow | (pow, c) <- zip [0..] coeffs]

    term 0 _ = Nothing
    term c 0 = mkConst c
    term c 1 = Just $ mkMul (Const c) (VarPow 1)
    term c k = Just $ mkMul (Const c) (VarPow k)

    mkAddExpr = mkAdd


-- Write (and check) a quickCheck property for this function similar to
-- question 6. 

prop_polyToExpr :: Int -> Poly -> Bool
prop_polyToExpr x pol = eval x (polyToExpr pol) == evalPoly x pol
--------------------------------------------------------------------------------
-- * A8

{- PROMPT
Let the coding assistant write a function simplify :: Expr -> Expr that simplifies 
an expression by converting it to a polynomial and back again (this is easy)-}

simplify :: Expr -> Expr
simplify = polyToExpr . exprToPoly

--------------------------------------------------------------------------------
-- * A9

{- PROMPT
The idea with simplify is that it should end you up with a simplified expression (of course). 
But this depends exactly how the function polyToExpr is defined (in A7)-}

prop_noJunk :: Expr -> Bool
prop_noJunk = noJunk . simplify

-- Recursive check for junk patterns
noJunk :: Expr -> Bool
noJunk (Const _) = True
noJunk (VarPow 0) = False  -- x^0 is junk
noJunk (VarPow _) = True
noJunk (BinExpr AddOp (Const 0) _) = False
noJunk (BinExpr AddOp _ (Const 0)) = False
noJunk (BinExpr AddOp (Const _) (Const _)) = False
noJunk (BinExpr MulOp (Const 0) _) = False
noJunk (BinExpr MulOp _ (Const 0)) = False
noJunk (BinExpr MulOp (Const 1) _) = False
noJunk (BinExpr MulOp _ (Const 1)) = False
noJunk (BinExpr MulOp (Const _) (Const _)) = False
noJunk (BinExpr _ exp1 exp2) = noJunk exp1 && noJunk exp2


--------------------------------------------------------------------------------
-- * A10

{- PROMPT

-We are going to define a function play that lets a user solve polynomial expressions of a particularly difficulty. 
The difficulty is going to be saved to file, such that a user can resume solving polynomials at the appropriate difficulty. 
The difficulty is modelled as a natural number and is stored in a file called difficulty.txt. The file should be stored in 
  the same directory as the source files.
Let the coding assistant write two IO functions that read respectively write the difficulty:

type Difficulty = Int

readDifficulty :: IO Difficulty

writeDifficulty :: Difficulty -> IO ()-}


type Difficulty = Int

diffFile :: FilePath
diffFile = "difficulty.txt"

readDifficulty :: IO Difficulty
readDifficulty = do
  exists <- doesFileExist diffFile
  if not exists
    then return 1
    else do
      content <- readFile diffFile
      case reads content of
        [(n, "")] | n >= 1 -> return n
        _                  -> return 1

writeDifficulty :: Difficulty -> IO ()
writeDifficulty n = writeFile diffFile (show n)


--------------------------------------------------------------------------------
-- * A11


{- PROMPT
Continue with the aid of the coding assistant to define the play :: IO () function that

reads the difficulty from file,
generates a random expression for that difficulty,
generates a random value for x,
shows the simplified expression,
and ask the user to solve it.
If the guess by the user is as expected: give a nice feedback 
message and increase the difficulty by one, using the appropriate function from A10. 
If the guess was wrong, again give feedback to the user and decrease the difficulty by one. 
After having given feed back you should let the user play again.

The text below shows an example interaction:

ghci> play
Simplify the following expression with x = 4

13*x^5+15*x^3+57*x^2+15*x

> 132
No, it should have been 15244.

Simplify the following expression with x = 4

2*x^3

> 128
Well done!

Simplify the following expression with x = 2

x^3+x^2

>-}

-- Generate a random polynomial based on difficulty
genPoly :: Difficulty -> IO Poly
genPoly d = do
  coeffs <- generate $ vectorOf (d + 1) (choose (-5, 5))
  return $ fromList coeffs

-- Main play loop
play :: IO ()
play = forever $ do
  difficulty <- readDifficulty
  poly <- genPoly difficulty
  let expr = simplify (polyToExpr poly)

  x <- randomRIO (1, 5)
  putStrLn $ "\nSimplify the following expression with x = " ++ show x ++ "\n"
  putStrLn $ show expr
  putStrLn "\n> "

  input <- getLine
  case reads input of
    [(guess, "")] -> do
      let correct = eval x expr
      if guess == correct
        then do
          putStrLn "Well done!"
          writeDifficulty (difficulty + 1)
        else do
          putStrLn $ "No, it should have been " ++ show correct

--------------------------------------------------------------------------------
-- * A12

{- PROMPT

A parser is a tool (a function) that converts a string to a value of a particular type. 
For example, a compiler uses a parser to convert the contents of a source code file (a string), 
to an abstract syntax tree that is used to generate an executable. Ask the coding assistant to define a parser, 
which must use the Parser.hs module, for the simple arithmetic expression language you have defined. 
That is, it should write a function that converts a string, 
which can contain numbers, additions, and multiplications, to a value of type Expr. 
The function should have the following type signature:

parseExpr :: String -> Maybe Expr
-}


parseExpr :: String -> Maybe Expr
parseExpr input = parse (trim exprParser) input

-- Expression parser with precedence
exprParser :: Parse Expr
exprParser = chainl termParser addOp

termParser :: Parse Expr
termParser = chainl factorParser mulOp

factorParser :: Parse Expr
factorParser = parseConst <|> parseVarPow <|> parens exprParser

-- Parse constants like "42"
parseConst :: Parse Expr
parseConst = Const <$> int

parseVarPow :: Parse Expr
parseVarPow = VarPow <$> (char 'x' *> ((char '^' *> int) `opt` 1))

-- Operators
addOp :: Parse (Expr -> Expr -> Expr)
addOp = (\_ -> BinExpr AddOp) <$> char '+'

mulOp :: Parse (Expr -> Expr -> Expr)
mulOp = (\_ -> BinExpr MulOp) <$> char '*'

prop_roundTrip :: Expr -> Bool
prop_roundTrip e = 
  case parseExpr (show e) of
    Just e' -> simplify e == simplify e'
    Nothing -> False

main :: IO ()
main = quickCheck prop_roundTrip

      
                        
 

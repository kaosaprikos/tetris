module CheckPartA where

import Data.List (nub, transpose, (\\))
import Test.Hspec
import Test.QuickCheck

-- Import the student's code
import Shapes ( Shape(..), Square, Row, Colour(..)
              , emptyShape, shapeSize, blockCount, prop_Shape, rotateShape
              , shiftShape, padShape, padShapeTo, overlaps, rows
              , genColour
              )

-- ToDo: define newtype for Shape and implement shrinking. 

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

genShape :: Gen Shape
genShape = elements allShapes

genShape' :: Gen Shape
genShape' = let dim = choose (1, 10) in do
  c  <- elements [Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey]
  h  <- dim
  w  <- dim
  rs <- vectorOf h $ vectorOf w $ elements [Just c, Nothing]
  return (Shape rs)

apply :: Int -> (a -> a) -> a -> a
apply n f = (!! n) . iterate f

noColour :: [[Square]] -> Bool
noColour = all (all (== Nothing))

main :: IO ()
main = hspec $ do
  describe "Task A1: emptyShape" $ do
    it "Should have the correct size and only contain Nothing elems:" $ 
      property $ \(NonNegative r, NonNegative c) -> let s = emptyShape (r, c) in
        noColour (rows s) && 
        length (rows s) == r && 
        all ((== c) . length) (rows s)

  describe "Task A2: shapeSize" $ do
    it "The size of a empty shape is (0,0): " $ shapeSize (Shape []) `shouldBe` (0, 0)
    it "Correct size: " $ property $ \(Positive r, Positive c) -> 
      shapeSize (emptyShape (r, c)) == (r, c)

  describe "Task A3: blockCount" $ do
    it "The count of an empty shape: " $ blockCount (Shape []) `shouldBe` 0
    it "Correct count: " $ property $ forAll genShape $ \s -> 
      blockCount s == 4

  describe "Task A4: prop_Shape" $ do
    it "Should hold for all given shapes" $ property $ forAll genShape $ \s ->
      prop_Shape s 
    it "Should fail for the empty shape: " $ not $ prop_Shape (Shape [])
    let wrongShape = Shape [[Just Black], [Just Black, Just Black]]
    it "Should fail for an irregular shape: " $ not $ prop_Shape wrongShape

  describe "Task A5: genColour" $ do
    let g = vectorOf 10000 genColour
    it "Should generate all possible colours: " $ property $ forAll g $ \cs ->
      null ([Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey] \\ cs)

  describe "Task A6: genShape" $ do
    it "Should generate valid shapes: " $ property prop_Shape

  describe "Task A7: rotateShape" $ do
    it "Should not change the block count: " $ property $ forAll genShape $ 
      \s (NonNegative n) -> blockCount s == blockCount (apply n rotateShape s)
    it "Rotating four times should give the same shape: " $ property $ forAll genShape $
      \s -> s == apply 4 rotateShape s
    it "Rotating M x N shape gives a N x M shape: " $ property $ forAll genShape $
      \s -> shapeSize s == let (m, n) = shapeSize (rotateShape s) in (n, m)
  
  describe "Task A8: shiftShape" $ do
    it "Expands the right amount" $ property $ forAll genShape $ 
      \s (NonNegative m) (NonNegative n) -> 
        shapeSize (shiftShape (m, n) s) == let (r, c) = shapeSize s in (r+m, c+n)
    it "The original shape should be preserved: " $ property $ forAll genShape $
      \s (NonNegative m) (NonNegative n) -> let (Shape rs) = shiftShape (m, n) s in
        rows s == map (drop n) (drop m rs)
    it "The added rows and columns should be empty: " $ property $ forAll genShape $
      \s (NonNegative m) (NonNegative n) -> let (Shape rs) = shiftShape (m, n) s in
        conjoin [noColour (take m rs), noColour (map (take n) rs)]
  
  describe "Task A9: padShape" $ do
    it "Expands the right amount" $ property $ forAll genShape $ 
      \s (NonNegative m) (NonNegative n) -> 
        shapeSize (padShape (m, n) s) == let (r, c) = shapeSize s in (r+m, c+n)
    it "The original shape should be preserved: " $ property $ forAll genShape $
      \s (NonNegative m) (NonNegative n) -> 
        let (Shape rs) = padShape (m, n) s
            (r, c)     = shapeSize s
        in  rows s === map (take c) (take r rs)
    it "The added rows and columns should be empty: " $ property $ forAll genShape $
      \s (NonNegative m) (NonNegative n) -> 
        let (Shape rs) = padShape (m, n) s
            (r, c)     = shapeSize s
        in  conjoin [noColour (drop (r+m) rs), noColour (map (drop (c+n)) rs)]


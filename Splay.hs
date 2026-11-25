module Splay where

import Diagrams.Backend.Rasterific
import Diagrams.Prelude hiding (Empty)
import Diagrams.TwoD.Layout.Tree

import System.Random

import Data.Maybe (fromJust, isJust)

-- From https://gist.github.com/m2ym/4232390

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

leftTree :: Int -> Tree Int
leftTree 0 = Node Leaf 0 Leaf
leftTree n = Node (leftTree (n - 1)) n Leaf

rightTree :: Int -> Tree Int
rightTree n = rightTree' 0 n
 where
  rightTree' i n
    | i > n = Leaf
    | otherwise = Node Leaf i (rightTree' (i + 1) n)

zigZagTree :: Int -> Tree Int
zigZagTree n = zig 0 n
 where
  zig lo hi
    | lo > hi = Leaf
    | otherwise = Node Leaf lo (zag (lo + 1) hi)
  zag lo hi
    | lo > hi = Leaf
    | otherwise = Node (zig lo (hi - 1)) hi Leaf

replace :: a -> Tree a -> Tree a
replace a Leaf = Node Leaf a Leaf
replace a (Node l _ r) = Node l a r

rotateR :: Tree a -> Tree a
rotateR (Node (Node x a y) b z) = Node x a (Node y b z)

rotateL :: Tree a -> Tree a
rotateL (Node x a (Node y b z)) = Node (Node x a y) b z

data Ctx a = Root | L a (Tree a) (Ctx a) | R a (Tree a) (Ctx a)
  deriving (Show)
data Zipper a = Zipper (Tree a) (Ctx a)
  deriving (Show)

update :: (Tree a -> Tree a) -> Zipper a -> Zipper a
update f (Zipper t c) = Zipper (f t) c

downto :: Ord a => a -> Zipper a -> Zipper a
downto _ z@(Zipper Leaf _) = z
downto a z@(Zipper (Node l b r) c) = case compare a b of
  EQ -> z
  LT -> downto a $ Zipper l (L b r c)
  GT -> downto a $ Zipper r (R b l c)

up :: Zipper a -> Zipper a
up z@(Zipper _ Root) = z
up (Zipper l (L a r c)) = Zipper (Node l a r) c
up (Zipper r (R a l c)) = Zipper (Node l a r) c

fromZipper :: Zipper a -> Tree a
fromZipper (Zipper t Root) = t
fromZipper z = fromZipper $ up z

toZipper :: Tree a -> Zipper a
toZipper = flip Zipper Root

empty :: Tree a
empty = Leaf

splay :: Zipper a -> Zipper a
splay z@(Zipper _ Root) = z
-- ZIG
splay z@(Zipper _ (L _ _ Root)) = update rotateR . up $ z
splay z@(Zipper _ (R _ _ Root)) = update rotateL . up $ z
-- ZIG-ZIG
splay z@(Zipper _ (L _ _ (L _ _ _))) = splay . update rotateR . update rotateR . up . up $ z
splay z@(Zipper _ (R _ _ (R _ _ _))) = splay . update rotateL . update rotateL . up . up $ z
-- ZIG-ZAG
splay z@(Zipper _ (L _ _ (R _ _ _))) = splay . update rotateL . up . update rotateR . up $ z
splay z@(Zipper _ (R _ _ (L _ _ _))) = splay . update rotateR . up . update rotateL . up $ z

locate :: Ord a => a -> Tree a -> Maybe (Tree a)
locate a t = case downto a $ toZipper t of
  Zipper Leaf _ -> Nothing
  z -> Just . fromZipper . splay $ z

member :: Ord a => a -> Tree a -> Bool
member a = isJust . locate a

insert :: Ord a => a -> Tree a -> Tree a
insert a = fromZipper . update (replace a) . downto a . toZipper

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert empty

toBTree :: Tree a -> BTree a
toBTree Leaf = Empty
toBTree (Node l a r) = BNode a (toBTree l) (toBTree r)

locateSequence :: [Int] -> Tree Int -> Tree Int
locateSequence [] t = t
locateSequence (x : xs) t = case locate x t of
  Nothing -> locateSequence xs t
  Just t' -> locateSequence xs t'

k :: Int
k = 1000

n :: Int
n = 5000

drawSplayResult :: [Int] -> Tree Int -> Diagram B
drawSplayResult accesses =
  maybe
    mempty
    ( renderTree
        (\n -> (text (show n) # fontSizeL 0.5 <> circle 0.5 # fc white))
        (~~)
    )
    . symmLayoutBin' (with & slVSep .~ 1.5 & slHSep .~ 1.5)
    . toBTree
    . locateSequence accesses

-- main :: IO ()
-- main = do
--   g <- getStdGen
--   let accesses :: [Int]
--       accesses = take n $ randomRs (0, k) g
--   defaultMain (drawSplayResult accesses (leftTree k))

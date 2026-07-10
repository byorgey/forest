module BinaryTrees where

import Diagrams.Backend.Rasterific
import Diagrams.Prelude hiding (Empty)
import Diagrams.TwoD.Layout.Tree

fringify :: BTree () -> BTree (Maybe ())
fringify Empty = BNode (Just ()) Empty Empty
fringify (BNode _ l r) = BNode Nothing (fringify l) (fringify r)

drawBin :: Bool -> BTree () -> Diagram B
drawBin shouldFringify =
  maybe emptyTree (renderTree drawNode (~~))
    . symmLayoutBin' (with & slVSep .~ 0.5)
    . if shouldFringify then fringify else fmap (const Nothing)

emptyTree, branchNode :: Diagram B
emptyTree = square 0.25 # fc white
branchNode = circle 0.1 # fc black

drawNode Nothing = branchNode
drawNode (Just ()) = emptyTree

b l r = BNode () l r
e = Empty
lf = leaf ()

exampleTrees :: [BTree ()]
exampleTrees =
  [ e
  , lf
  , b lf e
  , b lf (b (b e lf) e)
  ]

treesOfSize :: Int -> [BTree ()]
treesOfSize 0 = [Empty]
treesOfSize n = [BNode () l r | k <- [0 .. n - 1], l <- treesOfSize (n - k - 1), r <- treesOfSize k]

bsize :: BTree a -> Int
bsize Empty = 0
bsize (BNode _ l r) = 1 + bsize l + bsize r

left :: BTree a -> BTree a
left (BNode _ l _) = l

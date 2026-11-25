{-# LANGUAGE FlexibleContexts #-}

module SplayDia where

import Diagrams.Backend.Rasterific
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree

data SplayNode = Internal Char | Subtree Char

drawSplay :: BTree SplayNode -> Diagram B
drawSplay =
  maybe mempty (renderTree drawSplayNode (~~))
    . symmLayoutBin' (with & slVSep .~ 1 & slHSep .~ 2.5)
 where
  drawSplayNode (Internal x) =
    text [x] # fontSizeL 0.4 <> circle 0.5 # fc white
  drawSplayNode (Subtree x) =
    (text [x] # fontSizeL 0.4 <> triangle 1 # scaleY 1.2) # alignT

nd :: Char -> BTree SplayNode -> BTree SplayNode -> BTree SplayNode
nd x l r = BNode (Internal x) l r

sub :: Char -> BTree SplayNode
sub x = leaf (Subtree x)

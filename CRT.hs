{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module CRT where

import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Color.HSV
import Diagrams.Prelude

data CRTGridOpts = CGO
  { whichSquares :: Maybe [Int]
  , showAllNumbers :: Bool
  }

instance Default CRTGridOpts where
  def =
    CGO
      { whichSquares = Nothing
      , showAllNumbers = False
      }

crtGrid :: Int -> Int -> Diagram B
crtGrid = crtGrid' def

crtGridTotient :: Int -> Int -> Diagram B
crtGridTotient m n = crtGrid' (with {whichSquares = Just phi, showAllNumbers = True}) m n
 where
  phi = [k | k <- [0 .. lcm m n - 1], gcd k (m * n) == 1]

crtGrid' :: CRTGridOpts -> Int -> Int -> Diagram B
crtGrid' opts m n =
  mconcat
    [ gridLines
    , mconcat
        [ sq k i j (k `elem` sqs)
        | k <- [0 .. lcm m n - 1]
        , let i = k `mod` m
        , let j = k `mod` n
        ]
    ]
    # lwO 1
 where
  sqs = case whichSquares opts of
    Nothing -> [0 .. lcm m n - 1]
    Just ss -> ss
  gridLines =
    mconcat
      [ vsep 1 (replicate (m + 1) (hrule (fromIntegral n) # alignL # translateX (-0.5)))
          # translateY 0.5
      , hsep 1 (replicate (n + 1) (vrule (fromIntegral m) # alignT # translateY 0.5))
          # translateX (-0.5)
      ]
      # lineCap LineCapRound

  sq _ _ _ False | not (showAllNumbers opts) = mempty
  sq k i j hi =
    mconcat
      [ text (show k) # fontSizeL (if hi then 0.5 else 0.3)
      , square 1 # (if hi then fc (c k) else id) # lw none
      ]
      # moveTo (fromIntegral j ^& fromIntegral (-i))
  c k = hsvBlend (fromIntegral k / fromIntegral (lcm m n)) lightblue yellow

-- main = defaultMain (gridGrid [1 .. 9] [1 .. 5] # frame 1 # bg white)

gridGrid :: [Int] -> [Int] -> Diagram B
gridGrid = gridWith crtGrid

gridWith :: (Int -> Int -> Diagram B) -> [Int] -> [Int] -> Diagram B
gridWith f ms ns =
  [[(m, n) | n <- ns] | m <- ms]
    # map (map (uncurry f))
    # map (map alignTL)
    # map (hsep 1)
    # vsep 1

data CRTOpts = CRTOpts
  { _cellHighlight :: Int -> Style V2 Double
  , _rowHighlight :: Int -> Style V2 Double
  , _colHighlight :: Int -> Style V2 Double
  , _cellFontSize :: Double
  }

makeLenses ''CRTOpts

instance Default CRTOpts where
  def =
    CRTOpts
      { _cellHighlight = const mempty
      , _rowHighlight = const (mempty # lw none)
      , _colHighlight = const (mempty # lw none)
      , _cellFontSize = 0.5
      }

crtRelPm :: Int -> Int -> CRTOpts -> Diagram B
crtRelPm m n opts =
  mconcat
    [ grid
    , rowHighlights
    , colHighlights
    , colLabels
    , rowLabels
    ]
    # lwO 1
 where
  rowHighlights = flip foldMap [0 .. m - 1] $ \i ->
    rect (fromIntegral n + 1) 1
      # applyStyle ((opts ^. rowHighlight) i)
      # alignL
      # translate ((-1.5) ^& fromIntegral (-i))

  colHighlights = flip foldMap [0 .. n - 1] $ \i ->
    rect 1 (fromIntegral m + 1)
      # applyStyle ((opts ^. colHighlight) i)
      # alignT
      # translate (fromIntegral i ^& 1.5)

  mkLabel i = text (show i) # fontSizeL 0.3 <> square 1 # lw none
  colLabels = foldMap (\i -> mkLabel i # translate (fromIntegral i ^& 1)) [0 .. n - 1]
  rowLabels = foldMap (\i -> mkLabel i # translate ((-1) ^& (-fromIntegral i))) [0 .. m - 1]
  grid = foldMap sq [0 .. m * n - 1]
  sq x =
    mconcat
      [ text (show x) # fontSizeL (opts ^. cellFontSize)
      , square 1 # applyStyle ((opts ^. cellHighlight) x)
      ]
      # moveTo (fromIntegral (x `mod` n) ^& fromIntegral (-(x `mod` m)))

-- main = defaultMain dia

-- dia :: Diagram B
-- dia =
--   crtRelPm 7 11 (with & rowHighlight .~ (highlightRelPm 7 # lw none)
--                       & colHighlight .~ (highlightRelPm 11 # lw none))
--   # frame 1 # bg white

highlightRelPm m i
  | gcd i m == 1 = mempty # fcA (blue `withOpacity` 0.4)
  | otherwise = mempty

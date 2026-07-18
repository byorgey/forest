{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (guard, void)
import Control.Monad.Writer.Strict
import Data.Char (isDigit)
import Data.Hashable
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T
import Development.Shake
import Development.Shake.FilePath
import Text.ParserCombinators.ReadP (
  ReadP,
  many1,
  readP_to_S,
  satisfy,
  skipSpaces,
  string,
  (+++),
 )
import Text.Printf (printf)
import Text.Read (readMaybe)

main :: IO ()
main = shakeArgs shakeOptions $ do
  action $ do
    trees <- getDirectoryFiles "trees-raw" ["//*.tree"]
    need ["trees" </> tree | tree <- trees]

  "trees//*.tree" %> \out -> do
    let rawTree = replaceDirectory1 out "trees-raw"
    need [rawTree]
    blocks <- liftIO $ extractBlocks rawTree out "trees/source" "_blocks"
    need ["assets/blocks" </> block <.> "png" | block <- blocks]

  "assets/blocks/*.ly.png" %> \out -> do
    let lilypondSrc = ("_blocks" </>) . dropExtension . takeFileName $ out
        lilypondArgs = words "-dtall-page-formats=png -dno-use-paper-size-for-page"
    cmd_ ("lilypond" :: String) lilypondArgs ["-o", dropExtension out] [lilypondSrc]

  "assets/blocks/*.dia.png" %> \out -> do
    let diagramSrc = ("_blocks" </>) . dropExtension . takeFileName $ out
        [w, h] = splitOn "x" (drop 1 . takeExtension . dropExtension $ diagramSrc)
        ws = guard (w /= "*") *> ["-w", w]
        hs = guard (h /= "*") *> ["-h", h]
    cmd_ ("diagrams-builder-rasterific" :: String) ws hs ["-o", out] [diagramSrc]

-- Extract lilypond + diagrams blocks. Produces:
--   - Corresponding .tree file in trees/, with blocks replaced by images
--   - .tree file with original source in trees/HASH.tree
--   - Each block extracted to an appropriate file _blocks/HASH.ext
-- Returns list of extracted block files
extractBlocks :: FilePath -> FilePath -> FilePath -> FilePath -> IO [FilePath]
extractBlocks rawTreeFile processedTreeFile treesDir blocksDir = do
  tree <- T.readFile rawTreeFile
  (tree', blockFiles) <- runWriterT $ interpolate (blockStart (eitherP lilypondStart diagramStart)) blockEnd (processBlock treesDir blocksDir) tree
  T.writeFile processedTreeFile tree'
  pure blockFiles

eitherP :: ReadP a -> ReadP b -> ReadP (Either a b)
eitherP ra rb = (Left <$> ra) +++ (Right <$> rb)

hashToHexStr :: Int -> String
hashToHexStr n = printf "%016x" n'
 where
  n' :: Integer
  n' = fromIntegral n - fromIntegral (minBound :: Int)

lilypondHeader :: Text
lilypondHeader =
  """
  \\version "2.24.4"
  \\paper{
    indent=0\\mm
    oddFooterMarkup=##f
    oddHeaderMarkup=##f
    bookTitleMarkup = ##f
    scoreTitleMarkup = ##f
  }
  \\language "english"

  """

processBlock :: FilePath -> FilePath -> Either () DiagramMetadata -> [Text] -> WriterT [FilePath] IO [Text]
processBlock srcDir blockDir blockType ls = do
  let content = T.unlines ls
      hx = hashToHexStr $ hash content
      showSize = maybe "*" show
      blockFile = case blockType of
        Left () -> hx <.> "ly"
        Right (DiagramMetadata w h) -> hx <.> (showSize w ++ "x" ++ showSize h) <.> "dia"
      pngFile = blockFile <.> "png"
      header = case blockType of
        Left {} -> lilypondHeader
        _ -> ""

      srcHeader =
        """
        \\taxon{Source}
        \\p{\\pre\\verb<<<|

        """
      srcFooter =
        """
        <<<}
        """
  liftIO $ T.writeFile (blockDir </> blockFile) (header <> content)
  liftIO $ T.writeFile (srcDir </> hx <.> "tree") (srcHeader <> content <> srcFooter)
  tell [blockFile]

  pure ["\\imgblock{" <> T.pack hx <> "}{" <> T.pack pngFile <> "}"]

-- | Given a recognizer that extracts some information from a starting
--   line, a predicate recognizing an ending line, and a function for
--   processing + transforming contents, go through the given text looking for
--   starting and ending lines, transforming the contents in between.
interpolate :: forall m a. Monad m => (Text -> Maybe a) -> (Text -> Bool) -> (a -> [Text] -> m [Text]) -> Text -> m Text
interpolate begin end process = fmap T.unlines . go . T.lines
 where
  go :: [Text] -> m [Text]
  go [] = pure []
  go (t : ts) = case begin t of
    Nothing -> (t :) <$> go ts
    Just a ->
      let (block, drop 1 -> rest) = break end ts
       in (++) <$> process a block <*> go rest

blockStart :: ReadP a -> Text -> Maybe a
blockStart p = fmap fst . listToMaybe . readP_to_S p . T.unpack

data DiagramMetadata = DiagramMetadata
  { _width :: Maybe Int
  , _height :: Maybe Int
  }

diagramStart :: ReadP DiagramMetadata
diagramStart = DiagramMetadata <$> (skipSpaces *> string "\\diagram{" *> size) <*> (string "}{" *> size <* string "}{\\verb<<<|")
 where
  size :: ReadP (Maybe Int)
  size = (readMaybe <$> many1 (satisfy isDigit)) +++ (Nothing <$ string "*")

lilypondStart :: ReadP ()
lilypondStart = void $ skipSpaces *> string "\\lilypond{\\verb<<<|"

blockEnd :: Text -> Bool
blockEnd = (== "<<<}")

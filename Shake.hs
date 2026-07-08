{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (void)
import Control.Monad.Writer.Strict
import Data.Char (isDigit)
import Data.Hashable
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
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

main :: IO ()
main = shakeArgs shakeOptions $ do
  action $ do
    trees <- getDirectoryFiles "trees-raw" ["//*.tree"]
    need ["trees" </> tree | tree <- trees]

  "trees//*.tree" %> \out -> do
    let rawTree = replaceDirectory1 out "trees-raw"
    need [rawTree]
    blocks <- liftIO $ extractBlocks rawTree out "_blocks"
    -- needed ["_blocks" </> block | block <- blocks] -- XXX do we need this?
    need ["assets/blocks" </> block <.> "png" | block <- blocks]

  "assets/blocks/*.ly.png" %> \out -> do
    let lilypondSrc = ("_blocks" </>) . dropExtension . takeFileName $ out
        lilypondArgs = words "-dtall-page-formats=png -dno-use-paper-size-for-page"
    cmd_ "lilypond" lilypondArgs ["-o", dropExtension out] [lilypondSrc]

-- Extract lilypond + diagrams blocks. Produces:
--   - Corresponding .tree file in trees/, with XXX
--   - Each block extracted to an appropriate file XXX HASH.ext
-- Returns list of extracted block files
extractBlocks :: FilePath -> FilePath -> FilePath -> IO [FilePath]
extractBlocks rawTreeFile processedTreeFile blocksDir = do
  tree <- T.readFile rawTreeFile
  (tree', blockFiles) <- runWriterT $ interpolate (blockStart (eitherP lilypondStart diagramStart)) blockEnd (processBlock blocksDir) tree
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

processBlock :: FilePath -> Either () DiagramMetadata -> [Text] -> WriterT [FilePath] IO [Text]
processBlock blockDir (Left ()) ls = do
  let content = T.unlines ls
      hx = hashToHexStr $ hash content
      blockFile = hx <.> "ly"
      pngFile = blockFile <.> "png"
  liftIO $ T.writeFile (blockDir </> blockFile) (lilypondHeader <> content)
  tell [blockFile]

  pure ["\\imgblock{" <> T.pack pngFile <> "}"]

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
  { width :: Int
  , height :: Int
  }

diagramStart :: ReadP DiagramMetadata
diagramStart = DiagramMetadata <$> (skipSpaces *> string "\\diagram{" *> int) <*> (string "}{" *> int <* string "}{\\verb<<<|")
 where
  int :: ReadP Int
  int = read <$> many1 (satisfy isDigit)

lilypondStart :: ReadP ()
lilypondStart = void $ skipSpaces *> string "\\lilypond{\\verb<<<|"

blockEnd :: Text -> Bool
blockEnd = (== "<<<}")

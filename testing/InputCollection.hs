module InputCollection (Input, Run, parseStdins) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List (isSuffixOf)
import Data.Word (Word64)
import RBT.Kernel
import System.Directory
import System.FilePath
import qualified Data.ByteString as B

type Input = (Cmd,Word64)
type Run a = [a]

parseInput :: Parser Input
parseInput = do
  c <- anyWord8
  x <- anyWord8
  return (if c == 0 then Delete else Insert, fromIntegral x)

parseRun :: Parser (Run Input)
parseRun = many parseInput

parseStdin :: FilePath -> IO (Either String (Run Input))
parseStdin stdin = parseOnly parseRun <$> B.readFile stdin

parseStdins :: FilePath ->IO (Either String [Run Input])
parseStdins dir = do
  files <- listDirectory dir
  let stdins = filter (".stdin" `isSuffixOf`) files
  sequence <$> forM stdins (parseStdin . combine dir)

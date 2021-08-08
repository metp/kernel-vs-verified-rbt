module InputCollection (Input, TestCase, parseStdins) where

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
type TestCase a = [a]

inputParser :: Parser Input
inputParser = do
  c <- anyWord8
  x <- anyWord8
  return (if c == 0 then Delete else Insert, fromIntegral x)

testCaseParser :: Parser (TestCase Input)
testCaseParser = many inputParser

parseStdin :: FilePath -> IO (Either String (TestCase Input))
parseStdin stdin = parseOnly testCaseParser <$> B.readFile stdin

parseStdins :: FilePath -> IO (Either String [TestCase Input])
parseStdins dir = do
  files <- listDirectory dir
  let stdins = filter (".stdin" `isSuffixOf`) files
  sequence <$> forM stdins (parseStdin . combine dir)

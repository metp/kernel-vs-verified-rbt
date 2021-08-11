module InputCollection (Input, TestCase, Result(..), parseStdins, printTestCaseResults) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString (Parser, anyWord8, parseOnly)
import Data.ByteString (ByteString)
import Data.List (isSuffixOf)
import Data.Word (Word64)
import RBT.Kernel
import System.Directory
import System.FilePath
import qualified Data.ByteString as B
import qualified RBT.Kernel as Kernel (Handle)

type Input = (Cmd,Word64)
type TestCase a = [a]

data Result = Result {
  input :: Input,
  vTree :: IRBT,
  kTreeIO :: Kernel.Handle -> IO IRBT }

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
  symbolicLink <- pathIsSymbolicLink dir
  dir <- if symbolicLink
    then getSymbolicLinkTarget dir
    else return dir
  files <- listDirectory dir
  let stdins = filter (".stdin" `isSuffixOf`) files
  sequence <$> forM stdins (parseStdin . combine dir)

printTestCaseResults :: [TestCase Result] -> IO ()
printTestCaseResults tss = forM_ tss $ \ts -> do
  print (fromEnum Reset)
  print 0 -- stub key (makes parsing on the other side easier)
  forM_ ts $ \Result {input=(cmd,key)} -> do
    print (fromEnum cmd)
    print key

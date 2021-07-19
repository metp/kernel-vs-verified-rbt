module Main where

import Control.Monad
import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Word (Word64)
import Options.Applicative
import RBT.Kernel (IRBT)
import RBT.Verified
import Strategy
import System.Random
import qualified RBT.Kernel as Kernel
import qualified RBT.Verified as RBT (empty)
import qualified RBT.Verified as Verified (insert, delete)

data Options = Options {
  strategy :: Strategy,
  verbose :: Bool }

data RandomOptions = RandomOptions {
  runs :: Word64,
  seed :: Int }

newtype ExhaustiveOptions = ExhaustiveOptions { runs :: Word64 }
newtype SymbolicOptions = SymbolicOptions { directory :: FilePath }

data Strategy =
  Random RandomOptions |
  Exhaustive ExhaustiveOptions |
  Symbolic SymbolicOptions

naturalParser :: (Integral i, Read i) => ReadM i
naturalParser = eitherReader $ \s -> if read s >= 0
  then Right $ read s
  else Left "Not a positive value"

options :: ParserInfo Options
options = info (opts <**> helper) desc where
  desc = fullDesc <> header "Userspace testing harness for the Linux Red-Black tree implementation"
  opts = Options <$> strategies <*> switch ( short 'v' <> help "verbose")
  strategies :: Parser Strategy
  strategies = hsubparser $
      command "random" (info randomOpts mempty) <>
      command "exhaustive" (info exhaustiveOpts mempty) <>
      command "symbolic" (info symbolicOpts mempty)
  randomOpts :: Parser Strategy
  randomOpts = Random <$> (RandomOptions <$> runParser <*>
    option auto ( short 's' <> metavar "<seed>" <> showDefault <> value 42 <>
      help "Seed for the pseudo-random-number generator"))
  exhaustiveOpts :: Parser Strategy
  exhaustiveOpts = Exhaustive . ExhaustiveOptions <$> runParser
  runParser = option naturalParser (short 'n' <> metavar "<runs>" <> help "Number of runs")
  symbolicOpts :: Parser Strategy
  symbolicOpts = Symbolic <$> (SymbolicOptions <$>
    strOption (long "directory" <> short 'd' <> help "Directory containing all .stdin test cases"))

restartHeader = "------Restart------\n"

main :: IO ()
main = do
  Options{..} <- execParser options
  hdl <- Kernel.init

  rss <- case strategy of
    Random RandomOptions{..} -> pure $ Strategy.random hdl runs seed
    Exhaustive ExhaustiveOptions{..} -> pure $ Strategy.exhaustive hdl runs
    Symbolic SymbolicOptions{..} -> Strategy.symbolic hdl directory >>= \case
      Left _ -> error "Parsing failed"
      Right r -> pure r

  forM_ rss $ \rs -> do
    Kernel.reset hdl
    when verbose $ putStrLn restartHeader
    checkResults verbose RBT.empty rs

  Kernel.cleanup hdl

compareTrees :: IRBT -> IRBT -> Either [String] ()
compareTrees vTree kTree
  | rbt kTree && inorder kTree == inorder vTree = Right ()
  | otherwise = Left $ map fst $ filter (not . snd) [
      ("color"     ,  invc kTree) ,
      ("height"    ,  invh kTree) ,
      ("root_black",  rootBlack kTree) ,
      ("inorder"   ,  inorder vTree == inorder kTree) ]

printTrees :: Cmd -> Word64 -> IRBT -> IRBT -> IRBT -> [String] -> IO ()
printTrees cmd key vTree kTree kTreePrev invs = do
  putStrLn $ unwords $ if null invs
  then [show cmd, show key]
  else ["After", show cmd, show key, "following invariants failed:"] ++ invs
  putStrLn $ "Kernel tree before:  " ++ show kTreePrev
  putStrLn $ "Kernel tree after:   " ++ show kTree
  putStrLn $ "Verified tree after: " ++ show vTree
  putStrLn ""

checkResults :: Bool -> IRBT -> [Result] -> IO ()
checkResults _ _ [] = return ()
checkResults verbose kTreePrev (Result{..}:rs) = do
  kTree' <- kTree
  case compareTrees vTree kTree' of
    Left invs ->
      printTrees cmd key vTree kTree' kTreePrev invs
    Right _ -> do
      when verbose $ printTrees cmd key vTree kTree' kTreePrev []
      checkResults verbose kTree' rs

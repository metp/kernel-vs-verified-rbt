module Main where

import Control.Monad
import Data.Word (Word64)
import InputCollection
import Options.Applicative
import RBT.Kernel (IRBT, Cmd(..))
import RBT.Verified
import Strategy
import qualified RBT.Kernel as Kernel
import qualified RBT.Verified as RBT (empty, equal_tree)
import qualified RBT.Verified as Verified (insert, delete)

data Options = Options {
  strategy :: Strategy,
  verbose :: Bool,
  structural :: Bool }

data RandomOptions = RandomOptions {
  n :: Word64,
  seed :: Int }

newtype ExhaustiveOptions = ExhaustiveOptions { n :: Word64 }
newtype SymbolicOptions = SymbolicOptions { directory :: FilePath }

data Strategy =
  Random RandomOptions |
  Exhaustive ExhaustiveOptions |
  Symbolic SymbolicOptions

restartHeader = "------Restart------\n"

main :: IO ()
main = do
  Options{..} <- execParser options
  hdl <- Kernel.init

  rss <- case strategy of
    Random RandomOptions{..} -> pure $ Strategy.random hdl n seed
    Exhaustive ExhaustiveOptions{..} -> pure $ Strategy.exhaustive hdl n
    Symbolic SymbolicOptions{..} -> Strategy.symbolic hdl directory >>= \case
      Left _ -> error "Parsing failed"
      Right r -> pure r

  forM_ rss $ \rs -> do
    Kernel.reset hdl
    when verbose $ putStrLn restartHeader
    checkResults verbose structural RBT.empty rs

  Kernel.cleanup hdl


structuralCompare :: IRBT -> IRBT -> Either [String] ()
structuralCompare a b = unless (equal_tree a b) $ Left ["RBTs not equal"]

invariantCompare :: IRBT -> IRBT -> Either [String] ()
invariantCompare vTree kTree = unless (rbt kTree && inorder kTree == inorder vTree) $
  Left $ map fst $ filter (not . snd) [
      ("color"     ,  invc kTree) ,
      ("height"    ,  invh kTree) ,
      ("root_black",  rootBlack kTree) ,
      ("inorder"   ,  inorder vTree == inorder kTree) ]

printTrees :: Input -> IRBT -> IRBT -> IRBT -> [String] -> IO ()
printTrees (cmd,key) vTree kTree kTreePrev invs = do
  putStrLn $ unwords $ if null invs
  then [show cmd, show key]
  else ["After", show cmd, show key, "following invariants failed:"] ++ invs
  putStrLn $ "Kernel tree before:  " ++ show kTreePrev
  putStrLn $ "Kernel tree after:   " ++ show kTree
  putStrLn $ "Verified tree after: " ++ show vTree
  putStrLn ""

checkResults :: Bool -> Bool -> IRBT -> [Result] -> IO ()
checkResults _ _ _ [] = return ()
checkResults verbose structural kTreePrev (Result{..}:rs) = do
  kTree <- kTreeIO
  let cmpResult = (if structural
      then structuralCompare
      else invariantCompare) vTree kTree

  case cmpResult of
    Left invs -> printTrees input vTree kTree kTreePrev invs
    Right _ -> do
      when verbose $ printTrees input  vTree kTree kTreePrev []
      checkResults verbose structural kTree rs


{- HLINT ignore options "Monoid law, left identity" -}
options :: ParserInfo Options
options = info (opts <**> helper) desc where
  desc = fullDesc <> header "Userspace testing harness for the Linux Red-Black tree implementation"

  opts = Options 
    <$> strategies
    <*> switch ( short 'v' <> help "verbose" )
    <*> switch ( short 's' <> help "Use the structural comparison method" )

  strategies :: Parser Strategy
  strategies = hsubparser $ mempty
    <> command "random" (info randomOpts mempty) 
    <> command "exhaustive" (info exhaustiveOpts mempty)
    <> command "symbolic" (info symbolicOpts mempty)

  naturalParser :: (Integral i, Read i) => ReadM i
  naturalParser = eitherReader $ \s -> 
    if 0 <= read s
    then Right $ read s
    else Left "Not a positive value"

  numberParser :: (Integral i, Read i) => Parser i
  numberParser = option naturalParser (short 'n')

  randomOpts :: Parser Strategy
  randomOpts = fmap Random $ RandomOptions 
    <$> numberParser
    <*> option auto (mempty
          <> short 's'
          <> metavar "<seed>"
          <> showDefault
          <> value 42
          <> help "Seed for the pseudo-random-number generator" )

  exhaustiveOpts :: Parser Strategy
  exhaustiveOpts = Exhaustive . ExhaustiveOptions <$> numberParser

  symbolicOpts :: Parser Strategy
  symbolicOpts = fmap Symbolic $ SymbolicOptions
    <$> strOption (mempty
          <> short 'd'
          <> long "directory"
          <> help "Directory containing all .stdin test cases" )

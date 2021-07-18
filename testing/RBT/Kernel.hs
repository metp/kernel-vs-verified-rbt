{-# LANGUAGE RecordWildCards #-}
module RBT.Kernel(Cmd(..), IRBT, RBT.Kernel.Handle, RBT.Kernel.init, cleanup, reset, insert, delete) where

import Data.Word (Word64)
import GHC.IO.Handle
import RBT.Verified (Tree, Color)
import System.IO
import qualified RBT.Verified as RBT (equal_tree, empty)

type IRBT = Tree (Word64, Color)

keyFile = "/sys/kernel/debug/rbt_if/key"
cmdFile = "/sys/kernel/debug/rbt_if/cmd"

data Cmd = Reset | Insert | Delete deriving (Enum, Bounded)

printCmd hdl = hPrint hdl . fromEnum

instance Show Cmd where
  show Reset = "reseting"
  show Insert = "inserting"
  show Delete = "deleting"

data Handle = Handle {
  keyHdl :: GHC.IO.Handle.Handle,
  cmdHdl :: GHC.IO.Handle.Handle }

init :: IO RBT.Kernel.Handle
init = do
  keyHdl <- openFile keyFile WriteMode
  cmdHdl <- openFile cmdFile ReadWriteMode
  let hdl = Handle{..}
  hSetBuffering keyHdl LineBuffering
  hSetBuffering cmdHdl LineBuffering
  reset hdl
  return hdl

cleanup :: RBT.Kernel.Handle -> IO ()
cleanup hdl = do
  reset hdl
  hClose $ keyHdl hdl
  hClose $ cmdHdl hdl

exec :: Cmd -> Maybe Word64 -> RBT.Kernel.Handle -> IO IRBT
exec cmd x Handle{..} = do
  maybe (pure ()) (hPrint keyHdl) x
  printCmd cmdHdl cmd
  hSeek cmdHdl AbsoluteSeek 0
  read <$> hGetLine cmdHdl

reset :: RBT.Kernel.Handle -> IO IRBT
reset hdl = do 
  tree <- exec Reset Nothing hdl
  if RBT.equal_tree RBT.empty tree
    then pure tree
    else errorWithoutStackTrace "Kernel RB-Tree initialization failed"

insert :: RBT.Kernel.Handle -> Word64 -> IO IRBT
insert hdl x = exec Insert (Just x) hdl

delete :: RBT.Kernel.Handle -> Word64 -> IO IRBT
delete hdl x = exec Delete (Just x) hdl

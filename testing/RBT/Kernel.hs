module RBT.Kernel(Cmd(..), IRBT, RBT.Kernel.Handle, RBT.Kernel.init, cleanup, insert, delete) where

import GHC.IO.Handle
import RBT.Verified (Tree, Color)
import System.IO
import qualified RBT.Verified as RBT (isEmpty)

type IRBT = Tree (Int, Color)

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
  hSetBuffering keyHdl LineBuffering
  hSetBuffering cmdHdl LineBuffering
  printCmd cmdHdl Reset
  kTreeInit <- read <$> hGetLine cmdHdl :: IO IRBT
  if RBT.isEmpty kTreeInit
    then pure (Handle keyHdl cmdHdl)
    else errorWithoutStackTrace "Kernel interface initialization failed"

cleanup :: RBT.Kernel.Handle -> IO ()
cleanup hdls = do
  hClose $ keyHdl hdls
  hClose $ cmdHdl hdls

insert :: RBT.Kernel.Handle -> Int -> IO IRBT
insert (Handle keyHdl cmdHdl) k = do
  hPrint keyHdl k
  printCmd cmdHdl Insert
  hSeek cmdHdl AbsoluteSeek 0
  read <$> hGetLine cmdHdl

delete :: RBT.Kernel.Handle -> Int -> IO IRBT
delete (Handle keyHdl cmdHdl) k = do
  hPrint keyHdl k
  printCmd cmdHdl Delete
  hSeek cmdHdl AbsoluteSeek 0
  read <$> hGetLine cmdHdl

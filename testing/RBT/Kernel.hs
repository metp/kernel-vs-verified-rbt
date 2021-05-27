module RBT.Kernel where

import RBT.Verified (Tree, Color)
import System.IO
import GHC.IO.Handle

keyFile = "/sys/kernel/debug/rbt_if/key"
cmdFile = "/sys/kernel/debug/rbt_if/cmd"

data KernelHandle = KernelHandle { keyHdl :: Handle, cmdHdl :: Handle }

init :: IO (KernelHandle, Tree (Int, Color))
init = do
  keyHdl <- openFile keyFile WriteMode
  cmdHdl <- openFile cmdFile ReadWriteMode
  hSetBuffering keyHdl LineBuffering
  hSetBuffering cmdHdl NoBuffering
  hPutStr cmdHdl "2"
  leaf <- read <$> hGetLine cmdHdl
  return (KernelHandle keyHdl cmdHdl, leaf)

cleanup :: KernelHandle -> IO ()
cleanup hdls = do
  hClose $ keyHdl hdls
  hClose $ cmdHdl hdls

insert :: KernelHandle -> Int -> IO (Tree (Int,Color))
insert (KernelHandle keyHdl cmdHdl) k = do
  hPrint keyHdl k
  hPutStr cmdHdl "0"
  hSeek cmdHdl AbsoluteSeek 0
  read <$> hGetLine cmdHdl

delete :: KernelHandle -> Int -> IO (Tree (Int,Color))
delete (KernelHandle keyHdl cmdHdl) k = do
  hPrint keyHdl k
  hPutStr cmdHdl "1"
  hSeek cmdHdl AbsoluteSeek 0
  read <$> hGetLine cmdHdl

module Main(main) where

import Data.Dwarf.Elf (loadElfDwarf)
import System.Environment (getArgs)
import qualified Data.Dwarf.ADT as Dwarf.ADT

main :: IO ()
main = do
  [filename] <- getArgs
  (cuDies, dieMap) <- loadElfDwarf filename
  mapM_ (print . Dwarf.ADT.parseCU dieMap) cuDies

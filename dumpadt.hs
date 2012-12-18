module Main(main) where

import Data.Dwarf.Elf (loadElfDwarf, elfSectionByName)
import System.Environment (getArgs)
import qualified Data.Dwarf as Dwarf
import qualified Data.Dwarf.ADT as Dwarf.ADT

main :: IO ()
main = do
  [filename] <- getArgs
  let
    endianess = Dwarf.LittleEndian
    targetSize = Dwarf.TargetSize32
  (elf, (cuDies, dieMap)) <- loadElfDwarf endianess filename
  let
    sections =
      Dwarf.ADT.Sections
      { Dwarf.ADT.dsDebugLine = elfSectionByName elf ".debug_line" }
    cus = map (Dwarf.ADT.parseCU endianess targetSize sections dieMap) cuDies
  print $ head cus

module Main(main) where

import Data.Dwarf.Elf (loadElfDwarf)
import Data.Tree (Tree(..), drawTree)
import System.Environment (getArgs)
import qualified Data.Dwarf as Dwarf

dieTree :: Dwarf.DIE -> Tree Dwarf.DIE
dieTree die = Node die . map dieTree $ Dwarf.dieChildren die

main :: IO ()
main = do
  [filename] <- getArgs
  (_, (cuDies, _)) <- loadElfDwarf Dwarf.LittleEndian filename
  putStrLn . drawTree . fmap show . head $ map dieTree cuDies

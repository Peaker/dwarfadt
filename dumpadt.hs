import System.Environment (getArgs)
import qualified Data.Dwarf as Dwarf
import qualified Data.Dwarf.ADT.Pretty as Dwarf.ADT.Pretty
import qualified Data.Dwarf.Elf as Dwarf.Elf

main :: IO ()
main = do
  [filename] <- getArgs
  let endianess = Dwarf.LittleEndian
  cus <- Dwarf.Elf.parseElfDwarfADT endianess filename
  let allWarnings = concatMap snd cus
  mapM_ print allWarnings
  print $ map (Dwarf.ADT.Pretty.compilationUnit . fst) cus

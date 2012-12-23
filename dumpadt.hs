import System.Environment (getArgs)
import qualified Data.Dwarf as Dwarf
import qualified Data.Dwarf.ADT.Pretty as DwarfPretty
import qualified Data.Dwarf.Elf as Dwarf.Elf

main :: IO ()
main = do
  [filename] <- getArgs
  let endianess = Dwarf.LittleEndian
  (dwarf, warnings) <- Dwarf.Elf.parseElfDwarfADT endianess filename
  mapM_ print warnings
  print $ DwarfPretty.dwarf dwarf

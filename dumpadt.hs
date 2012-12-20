import System.Environment (getArgs)
import qualified Data.Dwarf as Dwarf
import qualified Data.Dwarf.Elf as Dwarf.Elf

main :: IO ()
main = do
  [filename] <- getArgs
  let endianess = Dwarf.LittleEndian
  cus <- Dwarf.Elf.parseElfDwarfADT endianess filename
  print $ map show cus

module ElfDwarf (loadElfDwarf) where

import Control.Applicative ((<$>), Applicative(..))
import Control.Error (justErr)
import Data.Elf (parseElf, Elf(..), ElfSection(..))
import Data.List (find)
import System.IO.Posix.MMap (unsafeMMapFile)
import qualified Data.Dwarf as Dwarf

loadElfDwarf :: FilePath -> IO ([Dwarf.DIE], Dwarf.DIEMap)
loadElfDwarf filename = do
  bs <- unsafeMMapFile filename
  let
    elf = parseElf bs
    get name =
      justErr ("Could not find " ++ name) .
      fmap elfSectionData .
      find ((== name) . elfSectionName) $ elfSections elf
  sections <-
    either fail return $
    Dwarf.Sections <$> get ".debug_info" <*> get ".debug_abbrev" <*> get ".debug_str"
  pure $ Dwarf.parseInfo Dwarf.LittleEndian sections

module Data.Dwarf.Elf (loadElfDwarf, elfSectionByName) where

import Control.Applicative (Applicative(..))
import Data.Elf (parseElf, Elf(..), ElfSection(..))
import Data.List (find)
import Data.Maybe (fromMaybe)
import System.IO.Posix.MMap (unsafeMMapFile)
import qualified Data.ByteString as BS
import qualified Data.Dwarf as Dwarf

elfSectionByName :: Elf -> String -> BS.ByteString
elfSectionByName elf name =
  fromMaybe (error ("Could not find " ++ name)) .
  fmap elfSectionData .
  find ((== name) . elfSectionName) $ elfSections elf

loadElfDwarf :: Dwarf.Endianess -> FilePath -> IO (Elf, ([Dwarf.DIE], Dwarf.DIEMap))
loadElfDwarf endianess filename = do
  bs <- unsafeMMapFile filename
  let elf = parseElf bs
      get = elfSectionByName elf
      sections =
        Dwarf.Sections (get ".debug_info") (get ".debug_abbrev") (get ".debug_str")
  pure (elf, Dwarf.parseInfo endianess sections)

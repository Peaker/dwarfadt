module Data.Dwarf.Elf
  ( loadElfDwarf
  , elfSectionByName
  , parseElfDwarfADT
  ) where

import Control.Applicative (Applicative(..), (<$>), pure)
import Data.Dwarf.ADT (Dwarf)
import Data.Elf (parseElf, Elf(..), ElfData(..), ElfSection(..))
import Data.List (find)
import System.IO.Posix.MMap (unsafeMMapFile)
import qualified Data.ByteString as BS
import qualified Data.Dwarf as Dwarf
import qualified Data.Dwarf.ADT as Dwarf.ADT

elfSectionByName :: Elf -> String -> Either String BS.ByteString
elfSectionByName elf name =
  maybe (Left ("Missing section " ++ name)) Right .
  fmap elfSectionData .
  find ((== name) . elfSectionName) $ elfSections elf

loadElfDwarf :: FilePath -> IO (Elf, ([Dwarf.DIE], Dwarf.DIEMap))
loadElfDwarf filename = do
  bs <- unsafeMMapFile filename
  let elf = parseElf bs
      get = elfSectionByName elf
      endianess = case elfData elf of
        ELFDATA2LSB -> Dwarf.LittleEndian
        ELFDATA2MSB -> Dwarf.BigEndian
  sections <-
    either fail return $
    Dwarf.Sections
    <$> get ".debug_info"
    <*> get ".debug_abbrev"
    <*> get ".debug_str"
  pure (elf, Dwarf.parseInfo endianess sections)

parseElfDwarfADT :: FilePath -> IO (Dwarf, [Dwarf.ADT.Warning])
parseElfDwarfADT filename = do
  (_elf, (cuDies, dieMap)) <- loadElfDwarf filename
  pure $ Dwarf.ADT.fromDies dieMap cuDies

module Data.Dwarf.Elf
  ( loadElfDwarf
  , elfSectionByName
  , parseElfDwarfADT
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Data.Dwarf.ADT (Boxed, CompilationUnit)
import Data.Elf (parseElf, Elf(..), ElfSection(..))
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

loadElfDwarf :: Dwarf.Endianess -> FilePath -> IO (Elf, ([Dwarf.DIE], Dwarf.DIEMap))
loadElfDwarf endianess filename = do
  bs <- unsafeMMapFile filename
  let elf = parseElf bs
      get = elfSectionByName elf
  sections <-
    either fail return $
    Dwarf.Sections
    <$> get ".debug_info"
    <*> get ".debug_abbrev"
    <*> get ".debug_str"
  pure (elf, Dwarf.parseInfo endianess sections)

parseElfDwarfADT ::
  Dwarf.Endianess -> FilePath -> IO [(Boxed CompilationUnit, [Dwarf.ADT.Warning])]
parseElfDwarfADT endianess filename = do
  (_elf, (cuDies, dieMap)) <- loadElfDwarf endianess filename
  pure $ map (Dwarf.ADT.parseCU dieMap) cuDies

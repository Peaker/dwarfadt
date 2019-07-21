{-# LANGUAGE OverloadedStrings #-}
module Data.Dwarf.Elf
  ( loadElfDwarf
  , elfSectionByName
  , parseElfDwarfADT
  ) where

import qualified Data.ByteString as BS
import qualified Data.Dwarf as Dwarf
import           Data.Dwarf.ADT (Dwarf)
import qualified Data.Dwarf.ADT as Dwarf.ADT
import           Data.Elf (parseElf, Elf(..), ElfSection(..))
import           Data.List (find)
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.IO.Posix.MMap (unsafeMMapFile)

elfSectionByName :: Elf -> Text -> Either Text BS.ByteString
elfSectionByName elf name =
  maybe (Left ("Missing section " <> name)) Right .
  fmap elfSectionData .
  find ((== name) . Text.pack . elfSectionName) $ elfSections elf

loadElfDwarf :: Dwarf.Endianess -> FilePath -> IO (Elf, ([Dwarf.DIE], Dwarf.DIEMap))
loadElfDwarf endianess filename = do
  bs <- unsafeMMapFile filename
  let elf = parseElf bs
      get = elfSectionByName elf
  sections <-
    either (fail . Text.unpack) return $
    Dwarf.Sections
    <$> get ".debug_info"
    <*> get ".debug_abbrev"
    <*> get ".debug_str"
    <*> get ".debug_line"
  pure (elf, Dwarf.parseInfo endianess sections)

parseElfDwarfADT :: Dwarf.Endianess -> FilePath -> IO ((Dwarf, [Dwarf.ADT.Warning]))

parseElfDwarfADT endianess filename = do
  (_elf, (cuDies, dieMap)) <- loadElfDwarf endianess filename
  pure $ (Dwarf.ADT.fromDies dieMap cuDies)

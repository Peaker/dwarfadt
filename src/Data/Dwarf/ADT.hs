module Data.Dwarf.ADT (parseCU, CompilationUnit(..)) where

import Data.Dwarf (DieID, DIE(..), DW_TAG(..))
import qualified Data.Dwarf as Dwarf

data CompilationUnit = CompilationUnit
  { cuDIE :: DieID
  } deriving (Eq, Ord, Read, Show)

verifyTag :: DW_TAG -> DIE -> a -> a
verifyTag expected die x
  | tag == expected = x
  | otherwise = error $ "Invalid tag: " ++ show tag
  where
    tag = dieTag die

parseCU :: Dwarf.DIEMap -> DIE -> CompilationUnit
parseCU _dieMap die =
  verifyTag DW_TAG_compile_unit die .
  CompilationUnit $ dieId die

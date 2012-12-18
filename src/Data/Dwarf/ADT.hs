module Data.Dwarf.ADT (parseCU, CompilationUnit(..)) where

import Data.Dwarf (DieID, DIE(..), DW_TAG(..), DW_AT(..), DW_ATVAL(..), (!?))
import qualified Data.Dwarf as Dwarf
import qualified Data.Dwarf.Lens as Dwarf.Lens

-- Example attributes
-- DW_AT_producer=(DW_ATVAL_STRING "GNU C 4.4.5")
-- DW_AT_language=(DW_ATVAL_UINT 1)
-- DW_AT_name=(DW_ATVAL_STRING "../src/closures.c")
-- DW_AT_comp_dir=(DW_ATVAL_STRING "/home/ian/zz/ghc-7.4.1/libffi/build/i386-unknown-linux-gnu")
-- DW_AT_low_pc=(DW_ATVAL_UINT 135625548)
-- DW_AT_high_pc=(DW_ATVAL_UINT 135646754)
-- DW_AT_stmt_list=(DW_ATVAL_UINT 0)
data CompilationUnit = CompilationUnit
  { cuDIE :: DieID
  , producer :: String
  , language :: Dwarf.DW_LANG
  } deriving (Eq, Ord, Read, Show)

verifyTag :: DW_TAG -> DIE -> a -> a
verifyTag expected die x
  | tag == expected = x
  | otherwise = error $ "Invalid tag: " ++ show tag
  where
    tag = dieTag die

uniqueAttr :: String -> DIE -> DW_AT -> DW_ATVAL
uniqueAttr msg die at =
  case die !? at of
  [val] -> val
  [] -> error $ msg ++ ": Missing value for attribute: " ++ show at
  xs -> error $ msg ++ ": Multiple values for attribute: " ++ show at ++ ": " ++ show xs

getAttrVal :: String -> DIE -> DW_AT -> Dwarf.Lens.ATVAL_NamedPrism a -> a
getAttrVal container die at prism =
  Dwarf.Lens.getATVal (container ++ " attribute " ++ show at) prism $
  uniqueAttr container die at

parseCU :: Dwarf.DIEMap -> DIE -> CompilationUnit
parseCU _dieMap die =
  verifyTag DW_TAG_compile_unit die $
  CompilationUnit
  (dieId die)
  (getAttr DW_AT_producer Dwarf.Lens.aTVAL_STRING)
  (Dwarf.dw_lang (getAttr DW_AT_language Dwarf.Lens.aTVAL_UINT))
  where
    getAttr = getAttrVal "Compilation unit" die

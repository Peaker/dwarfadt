module Data.Dwarf.ADT (Sections(..), parseCU, CompilationUnit(..)) where

import Data.Dwarf (DieID, DIE(..), DW_TAG(..), DW_AT(..), DW_ATVAL(..), (!?))
import Data.Word (Word64)
import qualified Data.ByteString as BS
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
  , cuProducer :: String
  , cuLanguage :: Dwarf.DW_LANG
  , cuName :: String
  , cuCompDir :: String
  , cuLowPc :: Word64
  , cuHighPc :: Word64
--  , cuLineNumInfo :: ([String], [Dwarf.DW_LNE])
  } deriving (Eq, Ord, Show)

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

newtype Sections = Sections
  { dsDebugLine :: BS.ByteString
  }

parseCU ::
  Dwarf.Endianess -> Dwarf.TargetSize -> Sections ->
  Dwarf.DIEMap -> DIE -> CompilationUnit
parseCU _endianess _targetSize _sections _dieMap die =
  verifyTag DW_TAG_compile_unit die $
  CompilationUnit
  (dieId die)
  (getAttr DW_AT_producer Dwarf.Lens.aTVAL_STRING)
  (Dwarf.dw_lang (getAttr DW_AT_language Dwarf.Lens.aTVAL_UINT))
  (getAttr DW_AT_name Dwarf.Lens.aTVAL_STRING)
  (getAttr DW_AT_comp_dir Dwarf.Lens.aTVAL_STRING)
  (getAttr DW_AT_low_pc Dwarf.Lens.aTVAL_UINT)
  (getAttr DW_AT_high_pc Dwarf.Lens.aTVAL_UINT)
  -- lineNumInfo
  where
    -- lineNumInfo = Dwarf.parseLNE endianess targetSize stmt_list $ dsDebugLine sections
    -- stmt_list = getAttr DW_AT_stmt_list Dwarf.Lens.aTVAL_UINT
    getAttr = getAttrVal "Compilation unit" die

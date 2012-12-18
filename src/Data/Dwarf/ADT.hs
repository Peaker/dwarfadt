{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Dwarf.ADT
  ( Sections(..), parseCU
  , CompilationUnit(..)
  , Decl(..)
  , Def(..)
  , BaseType(..)
  , Typedef(..)
  , PtrType(..)
  , ConstType(..)
  , Member(..), StructureType(..)
  , SubrangeType(..), ArrayType(..)
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad.Fix (MonadFix, mfix)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (Reader, runReader)
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Dwarf (DieID, DIEMap, DIE(..), DW_TAG(..), DW_AT(..), DW_ATVAL(..), (!?))
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (maybeToList)
import Data.Traversable (traverse)
import Data.Word (Word, Word64)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString as BS
import qualified Data.Dwarf as Dwarf
import qualified Data.Dwarf.Lens as Dwarf.Lens
import qualified Data.Map as Map

verifyTag :: DW_TAG -> DIE -> a -> a
verifyTag expected die x
  | tag == expected = x
  | otherwise = error $ "Invalid tag: " ++ show tag
  where
    tag = dieTag die

uniqueAttr :: DIE -> DW_AT -> DW_ATVAL
uniqueAttr die at =
  case die !? at of
  [val] -> val
  [] -> error $ show die ++ ": Missing value for attribute: " ++ show at
  xs -> error $ show die ++ ": Multiple values for attribute: " ++ show at ++ ": " ++ show xs

maybeAttr :: DIE -> DW_AT -> Maybe DW_ATVAL
maybeAttr die at =
  case die !? at of
  [val] -> Just val
  [] -> Nothing
  xs -> error $ show die ++ ": Multiple values for attribute: " ++ show at ++ ": " ++ show xs

getATVal :: DIE -> DW_AT -> Dwarf.Lens.ATVAL_NamedPrism a -> DW_ATVAL -> a
getATVal die at prism = Dwarf.Lens.getATVal (show die ++ " attribute " ++ show at) prism

getAttrVal :: DW_AT -> Dwarf.Lens.ATVAL_NamedPrism a -> DIE -> a
getAttrVal at prism die = getATVal die at prism $ uniqueAttr die at

getMAttrVal :: DW_AT -> Dwarf.Lens.ATVAL_NamedPrism a -> DIE -> Maybe a
getMAttrVal at prism die =
  getATVal die at prism <$> maybeAttr die at

getName :: DIE -> String
getName = getAttrVal DW_AT_name Dwarf.Lens.aTVAL_STRING

---------- Monad
newtype M a = M (StateT (Map DieID Def) (Reader DIEMap) a)
  deriving (Functor, Applicative, Monad, MonadFix)
runM :: DIEMap -> M a -> a
runM dieMap (M act) = runReader (evalStateT act Map.empty) dieMap

askDIEMap :: M DIEMap
askDIEMap = liftDefCache $ lift Reader.ask

liftDefCache :: StateT (Map DieID Def) (Reader DIEMap) a -> M a
liftDefCache = M
---------- Monad

cachedMake :: DieID -> M Def -> M Def
cachedMake i act = do
  found <- liftDefCache . State.gets $ Map.lookup i
  case found of
    Just res -> pure res
    Nothing -> mfix $ \res -> do
      liftDefCache . State.modify $ Map.insert i res
      act

parseAt :: DieID -> M Def
parseAt i = cachedMake i $ do
  dieMap <- askDIEMap
  let die = Dwarf.dieRefsDIE $ dieMap Map.! i
  parseDefI die

-------------------

data TypeRef = Void | TypeRef Def
  deriving (Eq, Ord)

instance Show TypeRef where
  show Void = "void"
  show (TypeRef _) = "(..type..)"

toTypeRef :: Maybe Def -> TypeRef
toTypeRef Nothing = Void
toTypeRef (Just x) = TypeRef x

-------------------

data Decl = Decl
  { declFile :: Maybe Word64 -- TODO: Convert to FilePath with LNI
  , declLine :: Maybe Int
  , declColumn :: Maybe Int
  } deriving (Eq, Ord)

instance Show Decl where
  show (Decl f l c) = intercalate ":" $ fmap ("FN"++) (toList f) ++ toList l ++ toList c
    where
      toList x = maybeToList $ fmap show x

getDecl :: DIE -> Decl
getDecl die =
  Decl
  (get DW_AT_decl_file)
  (fromIntegral <$> get DW_AT_decl_line)
  (fromIntegral <$> get DW_AT_decl_column)
  where
    get at = getMAttrVal at Dwarf.Lens.aTVAL_UINT die

-- DW_AT_byte_size=(DW_ATVAL_UINT 4)
-- DW_AT_encoding=(DW_ATVAL_UINT 7)
-- DW_AT_name=(DW_ATVAL_STRING "long unsigned int")
data BaseType = BaseType
  { btId :: DieID
  , btByteSize :: Word
  , btEncoding :: Word
  , btName :: Maybe String
  } deriving (Eq, Ord, Show)

parseBaseType :: DIE -> M BaseType
parseBaseType die =
  pure $
  BaseType (dieId die)
  (fromIntegral (getAttrVal DW_AT_byte_size Dwarf.Lens.aTVAL_UINT die))
  (fromIntegral (getAttrVal DW_AT_encoding Dwarf.Lens.aTVAL_UINT die))
  (getMAttrVal DW_AT_name Dwarf.Lens.aTVAL_STRING die)

-- DW_AT_name=(DW_ATVAL_STRING "ptrdiff_t")
-- DW_AT_decl_file=(DW_ATVAL_UINT 3)
-- DW_AT_decl_line=(DW_ATVAL_UINT 149)
-- DW_AT_type=(DW_ATVAL_REF (DieID 62))}
data Typedef = Typedef
  { tdName :: String
  , tdDecl :: Decl
  , tdType :: TypeRef
  } deriving (Eq, Ord)

instance Show Typedef where
  show (Typedef name decl _) = "Typedef " ++ show name ++ "@(" ++ show decl ++ ") = .."

parseTypeRef :: DIE -> M TypeRef
parseTypeRef die =
  fmap toTypeRef . traverse parseAt $ getMAttrVal DW_AT_type Dwarf.Lens.aTVAL_REF die

parseTypedef :: DIE -> M Typedef
parseTypedef die =
  Typedef (getName die) (getDecl die) <$>
  parseTypeRef die

data PtrType = PtrType
  { ptType :: TypeRef
  , ptByteSize :: Word
  } deriving (Eq, Ord)

instance Show PtrType where
  show (PtrType t _) = "Ptr to " ++ show t

parsePtrType :: DIE -> M PtrType
parsePtrType die =
  PtrType
  <$> parseTypeRef die
  <*> (pure . fromIntegral) (getAttrVal DW_AT_byte_size Dwarf.Lens.aTVAL_UINT die)

-- DW_AT_type=(DW_ATVAL_REF (DieID 104))
data ConstType = ConstType
  { ctType :: TypeRef
  } deriving (Eq, Ord, Show)

parseConstType :: DIE -> M ConstType
parseConstType die =
  ConstType <$> parseTypeRef die

-- DW_AT_name=(DW_ATVAL_STRING "__val")
-- DW_AT_decl_file=(DW_ATVAL_UINT 4)
-- DW_AT_decl_line=(DW_ATVAL_UINT 144)
-- DW_AT_type=(DW_ATVAL_REF (DieID 221))
-- DW_AT_data_member_location=(DW_ATVAL_BLOB "#\NUL")
data Member = Member
  { membName :: String
  , membDecl :: Decl
  , membLoc :: DW_ATVAL -- TODO: Parse this?
  , membType :: TypeRef
  } deriving (Eq, Ord, Show)

parseMember :: DIE -> M Member
parseMember die =
  verifyTag DW_TAG_member die .
  Member (getName die) (getDecl die) (uniqueAttr die DW_AT_data_member_location) <$>
  parseTypeRef die

-- DW_AT_byte_size=(DW_ATVAL_UINT 8)
-- DW_AT_decl_file=(DW_ATVAL_UINT 4)
-- DW_AT_decl_line=(DW_ATVAL_UINT 144)
-- DW_AT_sibling=(DW_ATVAL_REF (DieID 221))}
data StructureType = StructureType
  { stByteSize :: Word
  , stDecl :: Decl
  , stMembers :: [Member]
  } deriving (Eq, Ord, Show)

parseStructureType :: DIE -> M StructureType
parseStructureType die =
  StructureType
  (fromIntegral (getAttrVal DW_AT_byte_size Dwarf.Lens.aTVAL_UINT die))
  (getDecl die)
  <$> mapM parseMember (dieChildren die)

-- DW_AT_type=(DW_ATVAL_REF (DieID 101))
-- DW_AT_upper_bound=(DW_ATVAL_UINT 1)
data SubrangeType = SubrangeType
  { subRangeUpperBound :: Int
  , subRangeType :: TypeRef
  } deriving (Eq, Ord, Show)

parseSubrangeType :: DIE -> M SubrangeType
parseSubrangeType die =
  verifyTag DW_TAG_subrange_type die .
  SubrangeType
  (fromIntegral (getAttrVal DW_AT_upper_bound Dwarf.Lens.aTVAL_UINT die))
  <$> parseTypeRef die

-- DW_AT_type=(DW_ATVAL_REF (DieID 62))
data ArrayType = ArrayType
  { atSubrangeType :: SubrangeType
  , atType :: TypeRef
  } deriving (Eq, Ord, Show)

parseArrayType :: DIE -> M ArrayType
parseArrayType die =
  ArrayType <$> parseSubrangeType child <*> parseTypeRef die
  where
    child = case dieChildren die of
      [x] -> x
      cs -> error $ "Array must have exactly one child, not: " ++ show cs

data Def
  = DefBaseType BaseType
  | DefTypedef Typedef
  | DefPtrType PtrType
  | DefConstType ConstType
  | DefStructureType StructureType
  | DefArrayType ArrayType
  deriving (Eq, Ord, Show)

noChildren :: DIE -> DIE
noChildren die@DIE{dieChildren=[]} = die
noChildren die@DIE{dieChildren=cs} = error $ show die ++ " is not expected to have children, has: " ++ show cs

parseDefI :: DIE -> M Def
parseDefI die =
  case dieTag die of
  DW_TAG_base_type    -> fmap DefBaseType . parseBaseType $ noChildren die
  DW_TAG_typedef      -> fmap DefTypedef . parseTypedef $ noChildren die
  DW_TAG_pointer_type -> fmap DefPtrType . parsePtrType $ noChildren die
  DW_TAG_const_type   -> fmap DefConstType . parseConstType $ noChildren die
  DW_TAG_structure_type -> fmap DefStructureType $ parseStructureType die
  DW_TAG_array_type   -> fmap DefArrayType $ parseArrayType die
  _ -> error $ "unsupported: " ++ show die

parseDef :: DIE -> M Def
parseDef die = cachedMake (dieId die) $ parseDefI die

-- DW_AT_producer=(DW_ATVAL_STRING "GNU C 4.4.5")
-- DW_AT_language=(DW_ATVAL_UINT 1)
-- DW_AT_name=(DW_ATVAL_STRING "../src/closures.c")
-- DW_AT_comp_dir=(DW_ATVAL_STRING "/home/ian/zz/ghc-7.4.1/libffi/build/i386-unknown-linux-gnu")
-- DW_AT_low_pc=(DW_ATVAL_UINT 135625548)
-- DW_AT_high_pc=(DW_ATVAL_UINT 135646754)
-- DW_AT_stmt_list=(DW_ATVAL_UINT 0)
data CompilationUnit = CompilationUnit
  { cuId :: DieID
  , cuProducer :: String
  , cuLanguage :: Dwarf.DW_LANG
  , cuName :: String
  , cuCompDir :: String
  , cuLowPc :: Word64
  , cuHighPc :: Word64
--  , cuLineNumInfo :: ([String], [Dwarf.DW_LNE])
  , cuDefs :: [Def]
  } deriving (Show)

newtype Sections = Sections
  { dsDebugLine :: BS.ByteString
  }

parseCU ::
  Dwarf.Endianess -> Dwarf.TargetSize -> Sections ->
  DIEMap -> DIE -> CompilationUnit
parseCU _endianess _targetSize _sections dieMap die =
  runM dieMap $
  verifyTag DW_TAG_compile_unit die .
  CompilationUnit
  (dieId die)
  (getAttrVal DW_AT_producer Dwarf.Lens.aTVAL_STRING die)
  (Dwarf.dw_lang (getAttrVal DW_AT_language Dwarf.Lens.aTVAL_UINT die))
  (getName die)
  (getAttrVal DW_AT_comp_dir Dwarf.Lens.aTVAL_STRING die)
  (getAttrVal DW_AT_low_pc Dwarf.Lens.aTVAL_UINT die)
  (getAttrVal DW_AT_high_pc Dwarf.Lens.aTVAL_UINT die)
  -- lineNumInfo
  <$> mapM parseDef (dieChildren die)
  where
    -- lineNumInfo = Dwarf.parseLNE endianess targetSize stmt_list $ dsDebugLine sections
    -- stmt_list = getAttr DW_AT_stmt_list Dwarf.Lens.aTVAL_UINT

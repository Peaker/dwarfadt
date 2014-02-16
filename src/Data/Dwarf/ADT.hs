{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Data.Dwarf.ADT
  ( Warning(..)
  , Dwarf(..), fromDies
  , Boxed(..), CompilationUnit(..), fromDie
  , Decl(..)
  , Def(..), DefType(..)
  , TypeRef(..)
  , BaseType(..)
  , Typedef(..)
  , PtrType(..)
  , ConstType(..)
  , VolatileType(..)
  , Member(..), StructureType(..), UnionType(..)
  , SubrangeType(..), ArrayType(..)
  , EnumerationType(..), Enumerator(..)
  , SubroutineType(..), FormalParameter(..)
  , Subprogram(..)
  , Variable(..)
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens (_1)
import Control.Lens.Operators
import Control.Monad (when)
import Control.Monad.Fix (MonadFix, mfix)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.Trans.Writer (Writer, runWriter)
import Data.Dwarf (DieID, DIEMap, DIE(..), DW_TAG(..), DW_AT(..), DW_ATVAL(..))
import Data.Dwarf.AttrGetter (AttrGetterT)
import Data.Dwarf.Lens (_ATVAL_INT, _ATVAL_UINT, _ATVAL_REF, _ATVAL_STRING, _ATVAL_BLOB, _ATVAL_BOOL)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Traversable (traverse)
import Data.Word (Word, Word64)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Dwarf as Dwarf
import qualified Data.Dwarf.AttrGetter as AttrGetter
import qualified Data.Map as Map

getName :: Monad m => AttrGetterT m String
getName = AttrGetter.getAttr DW_AT_name _ATVAL_STRING

getMName :: Monad m => AttrGetterT m (Maybe String)
getMName = AttrGetter.findAttr DW_AT_name _ATVAL_STRING

data Warning = Warning
  { warningDieId :: DIE
  , warningUnusedAttrs :: [(DW_AT, DW_ATVAL)]
  }

instance Show Warning where
  show (Warning i unusedAttrs) =
    unlines
    $ ("WARNING: ignored attributes in: " ++ show i)
    : map (\(key, val) -> show key ++ "=" ++ show val) unusedAttrs

---------- { Monad
newtype M a = M (StateT (Map DieID (Boxed DefType)) (ReaderT DIEMap (Writer [Warning])) a)
  deriving (Functor, Applicative, Monad, MonadFix)
runM :: DIEMap -> M a -> Writer [Warning] a
runM dieMap (M act) = runReaderT (evalStateT act Map.empty) dieMap

liftDefCache :: StateT (Map DieID (Boxed DefType)) (ReaderT DIEMap (Writer [Warning])) a -> M a
liftDefCache = M

askDIEMap :: M DIEMap
askDIEMap = M $ lift Reader.ask

tellWarning :: Warning -> M ()
tellWarning warn = M . lift . lift $ Writer.tell [warn]

runAttrGetterT :: DIE -> AttrGetterT M a -> M a
runAttrGetterT die act = do
  (res, allUnusedAttrs) <- AttrGetter.run die act

  -- We don't care about the sibling attribute
  let unusedAttrs = filter ((/= DW_AT_sibling) . fst) allUnusedAttrs

  when (not (null unusedAttrs)) .
    tellWarning $ Warning die unusedAttrs
  return res
---------- } Monad

cachedMake :: DieID -> M (Boxed DefType) -> M (Boxed DefType)
cachedMake i act = do
  found <- liftDefCache . State.gets $ Map.lookup i
  case found of
    Just res -> pure res
    Nothing -> mfix $ \res -> do
      liftDefCache . State.modify $ Map.insert i res
      act

parseAt :: DieID -> M (Boxed DefType)
parseAt i = cachedMake i $ do
  dieMap <- askDIEMap
  let die = Dwarf.dieRefsDIE $ dieMap Map.! i
  parseDefTypeI die

data Loc = LocOp Dwarf.DW_OP | LocUINT Word64
  deriving (Eq, Ord, Show)

-------------------

data TypeRef = Void | TypeRef (Boxed DefType)
  deriving (Eq, Ord)

instance Show TypeRef where
  show Void = "void"
  show (TypeRef _) = "(..type..)"

toTypeRef :: Maybe (Boxed DefType) -> TypeRef
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

getDecl :: (Monad m, Applicative m) => AttrGetterT m Decl
getDecl =
  Decl
  <$> getUINT DW_AT_decl_file
  <*> (fmap fromIntegral <$> getUINT DW_AT_decl_line)
  <*> (fmap fromIntegral <$> getUINT DW_AT_decl_column)
  where
    getUINT = (`AttrGetter.findAttr` _ATVAL_UINT)

getByteSize :: (Monad m, Applicative m) => AttrGetterT m Word
getByteSize = fromIntegral <$> AttrGetter.getAttr DW_AT_byte_size _ATVAL_UINT

getMByteSize :: (Monad m, Applicative m) => AttrGetterT m (Maybe Word)
getMByteSize = fmap fromIntegral <$> AttrGetter.findAttr DW_AT_byte_size _ATVAL_UINT

data Boxed a = Boxed
  { bDieId :: DieID
  , bData :: a
  } deriving (Eq, Ord, Show, Functor)

mkBox :: DIE -> AttrGetterT M a -> M (Boxed a)
mkBox die act = Boxed (dieId die) <$> runAttrGetterT die act

box :: DW_TAG -> DIE -> AttrGetterT M a -> M (Boxed a)
box tag die act
  | tag == dieTag die = mkBox die act
  | otherwise =
    fail $ "Expected DIE with tag: " ++ show tag ++ " but found: " ++ show die

-- DW_AT_byte_size=(DW_ATVAL_UINT 4)
-- DW_AT_encoding=(DW_ATVAL_UINT 7)
-- DW_AT_name=(DW_ATVAL_STRING "long unsigned int")
data BaseType = BaseType
  { btByteSize :: Word
  , btEncoding :: Dwarf.DW_ATE
  , btName :: Maybe String
  } deriving (Eq, Ord, Show)

parseBaseType :: (Monad m, Applicative m) => AttrGetterT m BaseType
parseBaseType =
  BaseType
  <$> getByteSize
  <*> (Dwarf.dw_ate <$> AttrGetter.getAttr DW_AT_encoding _ATVAL_UINT)
  <*> getMName

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

parseTypeRef :: AttrGetterT M TypeRef
parseTypeRef =
  lift . fmap toTypeRef . traverse parseAt =<<
  AttrGetter.findAttr DW_AT_type _ATVAL_REF

parseTypedef :: AttrGetterT M Typedef
parseTypedef =
  Typedef <$> getName <*> getDecl <*> parseTypeRef

data PtrType = PtrType
  { ptType :: TypeRef
  , ptByteSize :: Word
  } deriving (Eq, Ord)

instance Show PtrType where
  show (PtrType t _) = "Ptr to " ++ show t

parsePtrType :: AttrGetterT M PtrType
parsePtrType =
  PtrType
  <$> parseTypeRef
  <*> getByteSize

-- DW_AT_type=(DW_ATVAL_REF (DieID 104))
data ConstType = ConstType
  { ctType :: TypeRef
  } deriving (Eq, Ord, Show)

parseConstType :: AttrGetterT M ConstType
parseConstType = ConstType <$> parseTypeRef

-- See ConstType
data VolatileType = VolatileType
  { vtType :: TypeRef
  } deriving (Eq, Ord, Show)

parseVolatileType :: AttrGetterT M VolatileType
parseVolatileType = VolatileType <$> parseTypeRef

-- DW_AT_name=(DW_ATVAL_STRING "__val")
-- DW_AT_decl_file=(DW_ATVAL_UINT 4)
-- DW_AT_decl_line=(DW_ATVAL_UINT 144)
-- DW_AT_type=(DW_ATVAL_REF (DieID 221))
-- DW_AT_data_member_location=(DW_ATVAL_BLOB "#\NUL")
data Member loc = Member
  { membName :: Maybe String
  , membDecl :: Decl
  , membLoc :: loc
  , membType :: TypeRef
  } deriving (Eq, Ord, Show)

parseMember :: (Dwarf.Reader -> AttrGetterT M loc) -> DIE -> M (Boxed (Member loc))
parseMember getLoc die =
  box DW_TAG_member die $
  Member <$> getMName <*> getDecl <*> getLoc (dieReader die) <*> parseTypeRef

-- DW_AT_name=(DW_ATVAL_STRING "__pthread_mutex_s")
-- DW_AT_byte_size=(DW_ATVAL_UINT 24)
-- DW_AT_decl_file=(DW_ATVAL_UINT 6)
-- DW_AT_decl_line=(DW_ATVAL_UINT 79)
data StructureType = StructureType
  { stName :: Maybe String
  , stByteSize :: Maybe Word -- Does not exist for forward-declarations
  , stDecl :: Decl
  , stIsDeclaration :: Bool -- is forward-declaration
  , stMembers :: [Boxed (Member Dwarf.DW_OP)]
  } deriving (Eq, Ord, Show)

getDeclaration :: AttrGetterT M Bool
getDeclaration = fromMaybe False <$> AttrGetter.findAttr DW_AT_declaration _ATVAL_BOOL

parseStructureType :: [DIE] -> AttrGetterT M StructureType
parseStructureType children =
  StructureType
  <$> getMName
  <*> getMByteSize
  <*> getDecl
  <*> getDeclaration
  <*> mapM (lift . parseMember getLoc) children
  where
    getLoc reader =
      Dwarf.parseDW_OP reader <$>
      AttrGetter.getAttr DW_AT_data_member_location _ATVAL_BLOB
  -- TODO: Parse the member_location, It's a blob with a DWARF program..

-- DW_AT_type=(DW_ATVAL_REF (DieID 101))
-- DW_AT_upper_bound=(DW_ATVAL_UINT 1)
data SubrangeType = SubrangeType
  { subRangeUpperBound :: Maybe Word
  , subRangeType :: TypeRef
  } deriving (Eq, Ord, Show)

parseSubrangeType :: DIE -> M (Boxed SubrangeType)
parseSubrangeType die =
  box DW_TAG_subrange_type die $
  SubrangeType
  <$> (fmap fromIntegral <$> AttrGetter.findAttr DW_AT_upper_bound _ATVAL_UINT)
  <*> parseTypeRef

-- DW_AT_type=(DW_ATVAL_REF (DieID 62))
data ArrayType = ArrayType
  { atSubrangeType :: [Boxed SubrangeType]
  , atType :: TypeRef
  } deriving (Eq, Ord, Show)

parseArrayType :: [DIE] -> AttrGetterT M ArrayType
parseArrayType cs =
  ArrayType <$> lift (mapM parseSubrangeType cs) <*> parseTypeRef

----------------

-- DW_AT_byte_size=(DW_ATVAL_UINT 4)
-- DW_AT_decl_file=(DW_ATVAL_UINT 6)
-- DW_AT_decl_line=(DW_ATVAL_UINT 96)
data UnionType = UnionType
  { unionName :: Maybe String
  , unionByteSize :: Word
  , unionDecl :: Decl
  , unionMembers :: [Boxed (Member (Maybe Dwarf.DW_OP))]
  } deriving (Eq, Ord, Show)

parseUnionType :: [DIE] -> AttrGetterT M UnionType
parseUnionType children =
  UnionType
  <$> getMName
  <*> getByteSize
  <*> getDecl
  <*> mapM (lift . parseMember getLoc) children
  where
    getLoc reader =
      fmap (Dwarf.parseDW_OP reader) <$>
      AttrGetter.findAttr DW_AT_data_member_location _ATVAL_BLOB

-- DW_AT_name=(DW_ATVAL_STRING "_SC_ARG_MAX")
-- DW_AT_const_value=(DW_ATVAL_INT 0)
data Enumerator = Enumerator
  { enumeratorName :: String
  , enumeratorConstValue :: Int64
  } deriving (Eq, Ord, Show)

parseEnumerator :: DIE -> M (Boxed Enumerator)
parseEnumerator die =
  box DW_TAG_enumerator die $
  Enumerator
  <$> getName
  <*> AttrGetter.getAttr DW_AT_const_value _ATVAL_INT

-- DW_AT_byte_size=(DW_ATVAL_UINT 4)
-- DW_AT_decl_file=(DW_ATVAL_UINT 11)
-- DW_AT_decl_line=(DW_ATVAL_UINT 74)
data EnumerationType = EnumerationType
  { enumName :: Maybe String
  , enumDecl :: Decl
  , enumByteSize :: Word
  , enumEnumerators :: [Boxed Enumerator]
  } deriving (Eq, Ord, Show)

parseEnumerationType :: [DIE] -> AttrGetterT M EnumerationType
parseEnumerationType children =
  EnumerationType
  <$> getMName
  <*> getDecl
  <*> getByteSize
  <*> mapM (lift . parseEnumerator) children

-- DW_AT_type=(DW_ATVAL_REF (DieID 119))
data FormalParameter = FormalParameter
  { formalParamName :: Maybe String
  , formalParamDecl :: Decl
  , formalParamLocation :: Maybe Loc
  , formalParamType :: TypeRef
  } deriving (Eq, Ord, Show)

parseLoc :: Dwarf.Reader -> DW_ATVAL -> Loc
parseLoc reader (DW_ATVAL_BLOB blob) = LocOp $ Dwarf.parseDW_OP reader blob
parseLoc _ (DW_ATVAL_UINT uint) = LocUINT uint
parseLoc _ x =
  error $
  "Expected DW_ATVAL_BLOB or DW_ATVAL_UINT for DW_AT_location field of variable, got: " ++
  show x

parseFormalParameter :: DIE -> M (Boxed FormalParameter)
parseFormalParameter die =
  box DW_TAG_formal_parameter die $
  FormalParameter
  <$> getMName
  <*> getDecl
  <*> (fmap (parseLoc (dieReader die)) <$> AttrGetter.findAttrVal DW_AT_location)
  <*> parseTypeRef

-- DW_AT_prototyped=(DW_ATVAL_BOOL True)
-- DW_AT_type=(DW_ATVAL_REF (DieID 62))
data SubroutineType = SubroutineType
  { subrPrototyped :: Bool
  , subrRetType :: TypeRef
    -- TODO: Reduce duplication with subprogram formal params
  , subrFormalParameters :: [Boxed FormalParameter]
  , subrHaveUnspecified :: Bool
  } deriving (Eq, Ord, Show)

getPrototyped :: AttrGetterT M Bool
getPrototyped = fromMaybe False <$> AttrGetter.findAttr DW_AT_prototyped _ATVAL_BOOL

parseSubroutineType :: [DIE] -> AttrGetterT M SubroutineType
parseSubroutineType children = do
  (params, haveUnspecified) <- lift (parseParameters children)
  SubroutineType
    <$> getPrototyped
    <*> parseTypeRef
    <*> pure params
    <*> pure haveUnspecified
  where
    parseParameters [] = pure ([], False)
    parseParameters [die] | dieTag die == DW_TAG_unspecified_parameters = pure ([], True)
    parseParameters (die:dies) = do
      param <- parseFormalParameter die
      parseParameters dies <&> _1 %~ (param :)

getLowPC :: AttrGetterT M Word64
getLowPC = AttrGetter.getAttr DW_AT_low_pc _ATVAL_UINT

getMLowPC :: AttrGetterT M (Maybe Word64)
getMLowPC = AttrGetter.findAttr DW_AT_low_pc _ATVAL_UINT

getMHighPC :: AttrGetterT M (Maybe Word64)
getMHighPC = AttrGetter.findAttr DW_AT_high_pc _ATVAL_UINT

-- DW_AT_name=(DW_ATVAL_STRING "sfs")
-- DW_AT_decl_file=(DW_ATVAL_UINT 1)
-- DW_AT_decl_line=(DW_ATVAL_UINT 135)
-- DW_AT_type=(DW_ATVAL_REF (DieID 2639))
-- DW_AT_location=(DW_ATVAL_BLOB "\145\168\DEL")
data Variable name = Variable
  { varName :: name
  , varDecl :: Decl
  , varLoc :: Maybe Loc
  , varExternal :: Bool
  , varDeclaration :: Bool
  , varType :: TypeRef
  } deriving (Eq, Ord, Show)

parseVariable :: Dwarf.Reader -> AttrGetterT M name -> AttrGetterT M (Variable name)
parseVariable reader getVarName =
  Variable
  <$> getVarName
  <*> getDecl
  <*> (fmap (parseLoc reader) <$> AttrGetter.findAttrVal DW_AT_location)
  <*> getExternal
  <*> getDeclaration
  <*> parseTypeRef

-- DW_AT_name=(DW_ATVAL_STRING "selinux_enabled_check")
-- DW_AT_decl_file=(DW_ATVAL_UINT 1)
-- DW_AT_decl_line=(DW_ATVAL_UINT 133)
-- DW_AT_prototyped=(DW_ATVAL_BOOL True)
-- DW_AT_type=(DW_ATVAL_REF (DieID 62))
-- DW_AT_low_pc=(DW_ATVAL_UINT 135801260)
-- DW_AT_high_pc=(DW_ATVAL_UINT 135801563)
-- DW_AT_frame_base=(DW_ATVAL_UINT 0)
data Subprogram = Subprogram
  { subprogName :: String
  , subprogDecl :: Decl
  , subprogPrototyped :: Bool
  , subprogExternal :: Bool
  , subprogLowPC :: Maybe Word64
  , subprogHighPC :: Maybe Word64
  , subprogFrameBase :: Maybe Loc
  , subprogFormalParameters :: [Boxed FormalParameter]
  , subprogUnspecifiedParameters :: Bool
  , subprogVariables :: [Boxed (Variable (Maybe String))]
  , subprogType :: TypeRef
  } deriving (Eq, Ord, Show)

data SubprogramChild
  = SubprogramChildFormalParameter (Boxed FormalParameter)
  | SubprogramChildVariable (Boxed (Variable (Maybe String)))
  | SubprogramChildIgnored
  | SubprogramChildUnspecifiedParameters
  deriving (Eq)

noChildren :: DIE -> a -> a
noChildren     DIE{dieChildren=[]} = id
noChildren die@DIE{dieChildren=cs} = error $ "Unexpected children: " ++ show cs ++ " in " ++ show die

getExternal :: AttrGetterT M Bool
getExternal = fromMaybe False <$> AttrGetter.findAttr DW_AT_external _ATVAL_BOOL

parseSubprogram :: Dwarf.Reader -> [DIE] -> AttrGetterT M Subprogram
parseSubprogram reader children = do
  parsedChildren <- mapM (lift . parseChild) children
  Subprogram
    <$> getName
    <*> getDecl
    <*> getPrototyped
    <*> getExternal
    <*> getMLowPC
    <*> getMHighPC
    <*> (fmap (parseLoc reader) <$> AttrGetter.findAttrVal DW_AT_frame_base)
    <*> pure [x | SubprogramChildFormalParameter x <- parsedChildren]
    <*> pure (SubprogramChildUnspecifiedParameters `elem` parsedChildren)
    <*> pure [x | SubprogramChildVariable x <- parsedChildren]
    <*> parseTypeRef
  where
    parseChild child =
      case dieTag child of
      DW_TAG_formal_parameter ->
        SubprogramChildFormalParameter <$> parseFormalParameter child
      DW_TAG_lexical_block -> pure SubprogramChildIgnored -- TODO: Parse content?
      DW_TAG_label -> pure SubprogramChildIgnored
      DW_TAG_variable ->
        noChildren child $
        SubprogramChildVariable <$> mkBox child (parseVariable reader getMName)
      DW_TAG_inlined_subroutine -> pure SubprogramChildIgnored
      DW_TAG_user 137 -> pure SubprogramChildIgnored -- GNU extensions, safe to ignore here
      DW_TAG_unspecified_parameters -> pure SubprogramChildUnspecifiedParameters
      DW_TAG_structure_type -> pure SubprogramChildIgnored
      DW_TAG_union_type -> pure SubprogramChildIgnored
      _ -> error $ "unsupported child tag in child: " ++ show child

data DefType
  = DefBaseType BaseType
  | DefTypedef Typedef
  | DefPtrType PtrType
  | DefConstType ConstType
  | DefVolatileType VolatileType
  | DefStructureType StructureType
  | DefArrayType ArrayType
  | DefUnionType UnionType
  | DefEnumerationType EnumerationType
  | DefSubroutineType SubroutineType
  deriving (Eq, Ord, Show)

data Def
  = DefType DefType
  | DefSubprogram Subprogram
  | DefVariable (Variable String)
  deriving (Eq, Ord, Show)

parseDefTypeI :: DIE -> M (Boxed DefType)
parseDefTypeI die =
  mkBox die $
  case dieTag die of
  DW_TAG_base_type        -> noChildren die $ DefBaseType     <$> parseBaseType
  DW_TAG_typedef          -> noChildren die $ DefTypedef      <$> parseTypedef
  DW_TAG_pointer_type     -> noChildren die $ DefPtrType      <$> parsePtrType
  DW_TAG_const_type       -> noChildren die $ DefConstType    <$> parseConstType
  DW_TAG_volatile_type    -> noChildren die $ DefVolatileType <$> parseVolatileType
  DW_TAG_structure_type   -> DefStructureType   <$> parseStructureType (dieChildren die)
  DW_TAG_array_type       -> DefArrayType       <$> parseArrayType (dieChildren die)
  DW_TAG_union_type       -> DefUnionType       <$> parseUnionType (dieChildren die)
  DW_TAG_enumeration_type -> DefEnumerationType <$> parseEnumerationType (dieChildren die)
  DW_TAG_subroutine_type  -> DefSubroutineType  <$> parseSubroutineType (dieChildren die)
  _ -> error $ "unsupported def type: " ++ show die

parseDefI :: DIE -> M (Boxed Def)
parseDefI die =
  case dieTag die of
  DW_TAG_variable ->
    mkBox die . noChildren die $
    DefVariable <$> parseVariable (dieReader die) getName
  DW_TAG_subprogram ->
    mkBox die $
    DefSubprogram <$> parseSubprogram (dieReader die) (dieChildren die)
  _ ->
    (fmap . fmap) DefType .
    cachedMake (dieId die) $
    parseDefTypeI die

parseDef :: DIE -> M (Boxed Def)
parseDef die = parseDefI die

-- DW_AT_producer=(DW_ATVAL_STRING "GNU C 4.4.5")
-- DW_AT_language=(DW_ATVAL_UINT 1)
-- DW_AT_name=(DW_ATVAL_STRING "../src/closures.c")
-- DW_AT_comp_dir=(DW_ATVAL_STRING "/home/ian/zz/ghc-7.4.1/libffi/build/i386-unknown-linux-gnu")
-- DW_AT_low_pc=(DW_ATVAL_UINT 135625548)
-- DW_AT_high_pc=(DW_ATVAL_UINT 135646754)
-- DW_AT_stmt_list=(DW_ATVAL_UINT 0)
data CompilationUnit = CompilationUnit
  { cuProducer :: String
  , cuLanguage :: Dwarf.DW_LANG
  , cuName :: String
  , cuCompDir :: String
  , cuLowPc :: Word64
  , cuHighPc :: Maybe Word64
  , cuStmtList :: Word64 -- TODO: Parse this further
--  , cuLineNumInfo :: ([String], [Dwarf.DW_LNE])
  , cuDefs :: [Boxed Def]
  } deriving (Show)

parseCU :: DIEMap -> DIE -> Writer [Warning] (Boxed CompilationUnit)
parseCU dieMap die =
  runM dieMap .
  box DW_TAG_compile_unit die $
  CompilationUnit
  <$> AttrGetter.getAttr DW_AT_producer _ATVAL_STRING
  <*> (Dwarf.dw_lang <$> AttrGetter.getAttr DW_AT_language _ATVAL_UINT)
  <*> getName
  <*> AttrGetter.getAttr DW_AT_comp_dir _ATVAL_STRING
  <*> getLowPC
  <*> getMHighPC
  <*> AttrGetter.getAttr DW_AT_stmt_list _ATVAL_UINT
  -- lineNumInfo
  <*> mapM (lift . parseDef) (dieChildren die)

fromDie :: DIEMap -> DIE -> (Boxed CompilationUnit, [Warning])
fromDie dieMap die = runWriter $ parseCU dieMap die

newtype Dwarf = Dwarf
  { dwarfCompilationUnits :: [Boxed CompilationUnit]
  }

fromDies :: DIEMap -> [DIE] -> (Dwarf, [Warning])
fromDies dieMap dies = runWriter $ Dwarf <$> mapM (parseCU dieMap) dies

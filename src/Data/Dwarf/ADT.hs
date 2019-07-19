{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Data.Dwarf.ADT
  ( Warning(..)
  , Dwarf(..), fromDies
  , Boxed(..), CompilationUnit(..), fromDie
  , Decl(..)
  , Def(..), DefType(..)
  , TypeRef(..)
  , BaseType(..)
  , Loc(..)
  , Typedef(..)
  , PtrType(..)
  , ConstType(..)
  , VolatileType(..)
  , FormalParameters(..)
  , MemberLocation(..), Member(..), StructureType(..), UnionType(..)
  , SubrangeType(..), ArrayType(..)
  , EnumerationType(..), Enumerator(..)
  , SubroutineType(..), FormalParameter(..)
  , InlineType(..)
  , InlinedSubroutine(..)
  , LexicalBlock(..)
  , SubprogramChild(..)
  , Subprogram(..), subprogramDefs
  , Variable(..)
  ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Monad (when)
import           Control.Monad.Fix (MonadFix, mfix)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as Reader
import           Control.Monad.Trans.State (StateT, evalStateT)
import qualified Control.Monad.Trans.State as State
import           Control.Monad.Trans.Writer (Writer, runWriter)
import qualified Control.Monad.Trans.Writer as Writer
import           Data.Dwarf (DieID, DIEMap, DIE(..), DW_TAG(..), DW_AT(..), DW_ATVAL(..))
import qualified Data.Dwarf as Dwarf
import           Data.Dwarf.AttrGetter (AttrGetterT)
import qualified Data.Dwarf.AttrGetter as AttrGetter
import           Data.Dwarf.Matchers (_ATVAL_INT, _ATVAL_UINT, _ATVAL_REF, _ATVAL_STRING, _ATVAL_BOOL)
import           Data.Int (Int64)
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import           Data.Text (Text)
import           Data.Traversable (traverse)
import           Data.Word (Word, Word64)
import           Control.Arrow

getName :: Monad m => AttrGetterT m Text
getName = AttrGetter.getAttr DW_AT_name _ATVAL_STRING

getMName :: Monad m => AttrGetterT m (Maybe Text)
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

getDecl :: Monad m => AttrGetterT m Decl
getDecl =
  Decl
  <$> getUINT DW_AT_decl_file
  <*> (fmap fromIntegral <$> getUINT DW_AT_decl_line)
  <*> (fmap fromIntegral <$> getUINT DW_AT_decl_column)
  where
    getUINT = (`AttrGetter.findAttr` _ATVAL_UINT)

getByteSize :: Monad m => AttrGetterT m Word
getByteSize = fromIntegral <$> AttrGetter.getAttr DW_AT_byte_size _ATVAL_UINT

getMByteSize :: Monad m => AttrGetterT m (Maybe Word)
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
    error $ "Expected DIE with tag: " ++ show tag ++ " but found: " ++ show die

-- DW_AT_byte_size=(DW_ATVAL_UINT 4)
-- DW_AT_encoding=(DW_ATVAL_UINT 7)
-- DW_AT_name=(DW_ATVAL_STRING "long unsigned int")
data BaseType = BaseType
  { btByteSize :: Word
  , btEncoding :: Dwarf.DW_ATE
  , btName :: Maybe Text
  } deriving (Eq, Ord, Show)

parseBaseType :: Monad m => AttrGetterT m BaseType
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
  { tdName :: Text
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

data MemberLocation
  = MemberLocationConstant Word64
  | MemberLocationExpression Dwarf.DW_OP
  deriving (Eq, Ord, Show)

-- DW_AT_name=(DW_ATVAL_STRING "__val")
-- DW_AT_decl_file=(DW_ATVAL_UINT 4)
-- DW_AT_decl_line=(DW_ATVAL_UINT 144)
-- DW_AT_type=(DW_ATVAL_REF (DieID 221))
-- DW_AT_data_member_location=(DW_ATVAL_BLOB "#\NUL")
data Member loc = Member
  { membName :: Maybe Text
  , membDecl :: Decl
  , membLoc :: loc
  , membType :: TypeRef
  , membByteSize :: Maybe Word64
  , membBitSize :: Maybe Word64
  , membBitOffset :: Maybe Word64
  } deriving (Eq, Ord, Show, Functor)

parseMember :: (Dwarf.Reader -> AttrGetterT M loc) -> DIE -> M (Boxed (Member loc))
parseMember getMemberLocation die =
  box DW_TAG_member die $
  Member
  <$> getMName
  <*> getDecl
  <*> getMemberLocation (dieReader die)
  <*> parseTypeRef
  <*> AttrGetter.findAttr DW_AT_byte_size _ATVAL_UINT
  <*> AttrGetter.findAttr DW_AT_bit_size _ATVAL_UINT
  <*> AttrGetter.findAttr DW_AT_bit_offset _ATVAL_UINT

-- DW_AT_name=(DW_ATVAL_STRING "__pthread_mutex_s")
-- DW_AT_byte_size=(DW_ATVAL_UINT 24)
-- DW_AT_decl_file=(DW_ATVAL_UINT 6)
-- DW_AT_decl_line=(DW_ATVAL_UINT 79)
data StructureType = StructureType
  { stName :: Maybe Text
  , stByteSize :: Maybe Word -- Does not exist for forward-declarations
  , stDecl :: Decl
  , stIsDeclaration :: Bool -- is forward-declaration
  , stMembers :: [Boxed (Member MemberLocation)]
  } deriving (Eq, Ord, Show)

flag :: DW_AT -> AttrGetterT M Bool
flag atId = ((Just True ==) <$> AttrGetter.findAttr atId _ATVAL_BOOL)

parseMemberLocation :: Dwarf.Reader -> DW_ATVAL -> MemberLocation
parseMemberLocation reader attrVal =
  case attrVal of
    Dwarf.DW_ATVAL_BLOB opStr -> MemberLocationExpression $ Dwarf.parseDW_OP reader opStr
    Dwarf.DW_ATVAL_UINT uint -> MemberLocationConstant uint
    _ ->
      -- TODO: Use function from AttrGetter to get better error
      error $ "member location of unknown type: " ++ show attrVal

parseStructureType :: [DIE] -> AttrGetterT M StructureType
parseStructureType children =
  StructureType
  <$> getMName
  <*> getMByteSize
  <*> getDecl
  <*> flag DW_AT_declaration
  <*> mapM (lift . parseMember getStructMemberLocation) children
  where
    getStructMemberLocation reader = do
      mAttrVal <- AttrGetter.findAttrVal DW_AT_data_member_location
      return $ case mAttrVal of
        Nothing ->
          -- TODO: Use function from AttrGetter to get better error
          error "StructureType must have a member location"
        Just attrVal -> parseMemberLocation reader attrVal

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
  { unionName :: Maybe Text
  , unionByteSize :: Word
  , unionDecl :: Decl
  , unionMembers :: [Boxed (Member (Maybe MemberLocation))]
  } deriving (Eq, Ord, Show)

parseUnionType :: [DIE] -> AttrGetterT M UnionType
parseUnionType children =
  UnionType
  <$> getMName
  <*> getByteSize
  <*> getDecl
  <*> mapM (lift . parseMember getUnionMemberLocation) children
  where
    getUnionMemberLocation reader = do
      mAttr <- AttrGetter.findAttrVal DW_AT_data_member_location
      return $ fmap (parseMemberLocation reader) mAttr

-- DW_AT_name=(DW_ATVAL_STRING "_SC_ARG_MAX")
-- DW_AT_const_value=(DW_ATVAL_INT 0)
data Enumerator = Enumerator
  { enumeratorName :: Text
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
  { enumName :: Maybe Text
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
  { formalParamName :: Maybe Text
  , formalParamDecl :: Decl
  , formalParamLocation :: Maybe Loc
  , formalParamType :: TypeRef
  , formalParamConstVal :: Maybe Word64
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
  box DW_TAG_formal_parameter die $ do
    -- for now, ignore abstract origins
    _ <- AttrGetter.findAttrVal DW_AT_abstract_origin
    FormalParameter
      <$> getMName
      <*> getDecl
      <*> (fmap (parseLoc (dieReader die)) <$> AttrGetter.findAttrVal DW_AT_location)
      <*> parseTypeRef
      <*> AttrGetter.findAttr DW_AT_const_value _ATVAL_UINT

data FormalParameters = FormalParameters
  { formalParameters :: [Boxed FormalParameter]
  , formalParametersHasUnspecified :: Bool
  } deriving (Eq, Ord, Show)

formalParametersSetter :: ([Boxed FormalParameter] -> [Boxed FormalParameter])
                      -> FormalParameters -> FormalParameters
formalParametersSetter f (FormalParameters pars unspec) = (`FormalParameters` unspec) (f pars)

parseFormalParameters :: [DIE] -> M (FormalParameters, [DIE])
parseFormalParameters = go
  where
    go dies =
      case dies of
      [] -> pure (FormalParameters [] False, [])
      (die:rest)
        | dieTag die == DW_TAG_unspecified_parameters -> pure (FormalParameters [] True, rest)
        | dieTag die == DW_TAG_formal_parameter -> do
            param <- parseFormalParameter die
            (\(a, b) -> (formalParametersSetter (param :) a, b))
              <$> go rest
        | otherwise -> second (die:) <$> go rest

data SubroutineType = SubroutineType
  { subrPrototyped :: Bool
  , subrRetType :: TypeRef
  , subrFormalParameters :: FormalParameters
  } deriving (Eq, Ord, Show)

-- DW_AT_prototyped=(DW_ATVAL_BOOL True)
-- DW_AT_type=(DW_ATVAL_REF (DieID 62))
parseSubroutineType :: [DIE] -> AttrGetterT M SubroutineType
parseSubroutineType children = do
  (params, extraChildren) <- lift $ parseFormalParameters children
  case extraChildren of
    [] ->
      SubroutineType
      <$> flag DW_AT_prototyped
      <*> parseTypeRef
      <*> pure params
    _ -> error $ "Unexpected children of SubroutineType: " ++ show extraChildren

getLowPC :: AttrGetterT M Word64
getLowPC = AttrGetter.getAttr DW_AT_low_pc _ATVAL_UINT

getMRanges :: AttrGetterT M (Maybe Word64)
getMRanges = AttrGetter.findAttr DW_AT_ranges _ATVAL_UINT

getMLowPC :: AttrGetterT M (Maybe Word64)
getMLowPC = AttrGetter.findAttr DW_AT_low_pc _ATVAL_UINT

getMHighPC :: AttrGetterT M (Maybe Word64)
getMHighPC = AttrGetter.findAttr DW_AT_high_pc _ATVAL_UINT

getMFrameBase :: Dwarf.Reader -> AttrGetterT M (Maybe Loc)
getMFrameBase reader =
  fmap (parseLoc reader) <$>
  AttrGetter.findAttrVal DW_AT_frame_base

data InlineType = InlineType
  { inlineRequested :: Bool
  , inlineHappened :: Bool
  } deriving (Eq, Ord, Show)

parseInlineType :: Word64 -> InlineType
parseInlineType 0 = InlineType False False
parseInlineType 1 = InlineType False True
parseInlineType 2 = InlineType True False
parseInlineType 3 = InlineType True True
parseInlineType n = error $ "Unknown inline type: " ++ show n

getInlineType :: AttrGetterT M (Maybe InlineType)
getInlineType =
  fmap parseInlineType <$> AttrGetter.findAttr DW_AT_inline _ATVAL_UINT

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
  , varArtificial :: Bool
  , varType :: TypeRef
  , varConstVal :: Maybe Word64
  } deriving (Eq, Ord, Show)

noChildren :: DIE -> a -> a
noChildren     DIE{dieChildren=[]} = id
noChildren die@DIE{dieChildren=cs} = error $ "Unexpected children: " ++ show cs ++ " in " ++ show die

-- TODO: Use Alternative/MonadPlus instance so that we can combine
-- parsers with <|> rather than test outside if it is DW_TAG_variable
-- and then rely on it here unsafely
parseVariable :: DIE -> AttrGetterT M name -> M (Boxed (Variable name))
parseVariable die getVarName =
  noChildren die $ mkBox die $ do
    -- for now, ignore abstract origins
    _ <- AttrGetter.findAttrVal DW_AT_abstract_origin
    Variable
      <$> getVarName
      <*> getDecl
      <*> (fmap (parseLoc (dieReader die)) <$> AttrGetter.findAttrVal DW_AT_location)
      <*> flag DW_AT_external
      <*> flag DW_AT_declaration
      <*> flag DW_AT_artificial
      <*> parseTypeRef
      <*> AttrGetter.findAttr DW_AT_const_value _ATVAL_UINT

data InlinedSubroutine = InlinedSubroutine
  { inlinedSubroutineCallFile :: Maybe Word64
  , inlinedSubroutineCallLine :: Maybe Word64
  , inlinedSubroutineRanges :: Maybe Word64
  , inlinedSubroutineEntryPC :: Word64
  , inlinedSubroutineSubprogram :: Subprogram
  } deriving (Eq, Ord, Show)

parseInlinedSubroutine :: DIE -> M (Boxed InlinedSubroutine)
parseInlinedSubroutine die =
  mkBox die $ InlinedSubroutine
  <$> AttrGetter.findAttr DW_AT_call_file _ATVAL_UINT
  <*> AttrGetter.findAttr DW_AT_call_line _ATVAL_UINT
  <*> getMRanges
  <*> AttrGetter.getAttr DW_AT_entry_pc  _ATVAL_UINT
  <*> parseSubprogram (dieReader die) (dieChildren die)

data LexicalBlock = LexicalBlock
  { lexicalBlockRanges :: Maybe Word64
  , lexicalBlockLowPC :: Maybe Word64
  , lexicalBlockHighPC :: Maybe Word64
  , lexicalBlockSubprogram :: Subprogram
  } deriving (Eq, Ord, Show)

parseLexicalBlock :: DIE -> M (Boxed LexicalBlock)
parseLexicalBlock die =
  mkBox die $ LexicalBlock
  <$> getMRanges
  <*> getMLowPC
  <*> getMHighPC
  <*> parseSubprogram (dieReader die) (dieChildren die)

data SubprogramChild
  = SubprogramChildDef Def
  | SubprogramChildLexicalBlock LexicalBlock -- TODO: Lexical blocks don't quite have everything a subprogram does
  | SubprogramChildInlinedSubroutine InlinedSubroutine
  | SubprogramChildLocalVariable (Variable (Maybe Text))
  | SubprogramChildLabel -- TODO: Label content
  | SubprogramChildOther DW_TAG
  deriving (Eq, Ord, Show)

subprogramChildDefs :: Boxed SubprogramChild -> [Boxed Def]
subprogramChildDefs (Boxed dId item) =
  case item of
  SubprogramChildDef def -> [Boxed dId def]
  SubprogramChildLexicalBlock x -> subprogramDefs (lexicalBlockSubprogram x)
  SubprogramChildInlinedSubroutine x -> subprogramDefs (inlinedSubroutineSubprogram x)
  SubprogramChildLocalVariable _ -> []
  SubprogramChildLabel -> []
  SubprogramChildOther _ -> []

data Subprogram = Subprogram
  { subprogName :: Maybe Text -- abstract-origin subprograms are anonymous
  , subprogType :: TypeRef
  , subprogFormalParameters :: FormalParameters
  , subprogDecl :: Decl
  , subprogPrototyped :: Bool
  , subprogExternal :: Bool
  , subprogLowPC :: Maybe Word64
  , subprogHighPC :: Maybe Word64
  , subprogFrameBase :: Maybe Loc
  , subprogInline :: Maybe InlineType
  , subprogDeclaration :: Bool
  , subprogArtificial :: Bool
  , subprogLinkageName :: Maybe Text
  , subprogChildren :: [Boxed SubprogramChild]
  } deriving (Eq, Ord, Show)

subprogramDefs :: Subprogram -> [Boxed Def]
subprogramDefs = concatMap subprogramChildDefs . subprogChildren

parseSubprogram :: Dwarf.Reader -> [DIE] -> AttrGetterT M Subprogram
parseSubprogram reader children = do
  (params, extraChildren) <- lift $ parseFormalParameters children
  -- for now, ignore abstract origins & GNU extended attr for now
  _ <- AttrGetter.findAttrVal $ DW_AT_user 0x2117
  _ <- AttrGetter.findAttrVal $ DW_AT_user 0x2116
  _ <- AttrGetter.findAttrVal DW_AT_abstract_origin
  Subprogram
    <$> getMName
    <*> parseTypeRef
    <*> pure params
    <*> getDecl
    <*> flag DW_AT_prototyped
    <*> flag DW_AT_external
    <*> getMLowPC
    <*> getMHighPC
    <*> getMFrameBase reader
    <*> getInlineType
    <*> flag DW_AT_declaration
    <*> flag DW_AT_artificial
    <*> AttrGetter.findAttr DW_AT_linkage_name _ATVAL_STRING
    <*> mapM (lift . parseChild) extraChildren
  where
    fakeBox child = pure . Boxed (dieId child)
    parseChild child =
      case dieTag child of
      DW_TAG_formal_parameter -> error $ "BUG: formal_parameter not captured by parseFormalParameters: " ++ show child
      DW_TAG_unspecified_parameters -> error $ "BUG: unspecified_parameters not captured by parseFormalParameters: " ++ show child
      DW_TAG_lexical_block -> fmap SubprogramChildLexicalBlock <$> parseLexicalBlock child
      DW_TAG_variable -> fmap SubprogramChildLocalVariable <$> parseVariable child getMName
      DW_TAG_label -> fakeBox child $ SubprogramChildLabel
      DW_TAG_inlined_subroutine -> fmap SubprogramChildInlinedSubroutine <$> parseInlinedSubroutine child
      tag | tag `elem`
        [ DW_TAG_base_type
        , DW_TAG_typedef
        , DW_TAG_pointer_type
        , DW_TAG_const_type
        , DW_TAG_volatile_type
        , DW_TAG_structure_type
        , DW_TAG_array_type
        , DW_TAG_union_type
        , DW_TAG_enumeration_type
        , DW_TAG_subroutine_type
        , DW_TAG_variable
        , DW_TAG_subprogram
        ] -> fmap SubprogramChildDef <$> parseDef child
      _ -> fakeBox child $ SubprogramChildOther $ dieTag child -- GNU extensions, safe to ignore here

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
  | DefVariable (Variable Text)
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

parseDef :: DIE -> M (Boxed Def)
parseDef die =
  case dieTag die of
  DW_TAG_variable -> fmap DefVariable <$> parseVariable die getName
  DW_TAG_subprogram -> mkBox die $ DefSubprogram <$> parseSubprogram (dieReader die) (dieChildren die)
  _ ->
    (fmap . fmap) DefType .
    cachedMake (dieId die) $
    parseDefTypeI die

-- DW_AT_producer=(DW_ATVAL_STRING "GNU C 4.4.5")
-- DW_AT_language=(DW_ATVAL_UINT 1)
-- DW_AT_name=(DW_ATVAL_STRING "../src/closures.c")
-- DW_AT_comp_dir=(DW_ATVAL_STRING "/home/ian/zz/ghc-7.4.1/libffi/build/i386-unknown-linux-gnu")
-- DW_AT_low_pc=(DW_ATVAL_UINT 135625548)
-- DW_AT_high_pc=(DW_ATVAL_UINT 135646754)
-- DW_AT_stmt_list=(DW_ATVAL_UINT 0)
data CompilationUnit = CompilationUnit
  { cuProducer :: Text
  , cuLanguage :: Dwarf.DW_LANG
  , cuName :: Text
  , cuCompDir :: Text
  , cuLowPc :: Word64
  , cuHighPc :: Maybe Word64
  , cuMRanges :: Maybe Word64
  , cuStmtList :: Word64 -- TODO: Parse this further
--  , cuLineNumInfo :: ([Text], [Dwarf.DW_LNE])
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
  <*> getMRanges
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

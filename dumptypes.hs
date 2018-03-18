{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Dwarf (DW_TAG(..), DW_AT(..), DW_ATVAL(..))
import qualified Data.Dwarf as Dwarf
import           Data.Dwarf.Elf (loadElfDwarf)
import           Data.Function ((&))
import           Data.Int
import           Data.List (lookup)
import           Data.Map ((!))
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word
import           System.Environment (getArgs)

Lens.makePrisms ''DW_ATVAL

data ArrayInfo = ArrayInfo
    { _aTypeName :: Maybe Text
    , _aTypeSize :: Maybe Word64
    , _aCount :: Maybe Word64
    }

data StructMember = StructMember
    { _smemberName :: Maybe Text
    , _smemberLocation :: Word64
    , _smemberSize :: Maybe Word64
    , _smemberArray :: Maybe ArrayInfo
    }

data UnionMember = UnionMember
    { _umemberName :: Maybe Text
    , _umemberSize :: Maybe Word64
    }

data Enumerator = Enumerator
    { _enumName :: Text
    , _enumVal :: Int64
    }

data TypeContent
    = Struct [StructMember]
    | Union [UnionMember]
    | Enum [Enumerator]

data Type = Type
    { _typeName :: Maybe Text
    , _typeSize :: Maybe Word64
    , _typeContent :: TypeContent
    }

showM :: Show a => Maybe a -> String
showM (Just x) = show x
showM Nothing = "?"

showName :: Maybe Text -> String
showName = maybe "?" (Text.unpack . Text.replace "\n" "\\n")

instance Show StructMember where
    show (StructMember name loc size arrInfo) =
        showName name
        <> " at " <> show loc
        <> " (" <> showM size <> " bytes" <>
        case arrInfo of
        Nothing -> ""
        Just (ArrayInfo elemType elemSize count) ->
            " <<arr of " <> showName elemType <> "(size=" <> showM elemSize <>
            ")[" <> showM count <> "]>>"
        <> ")"

instance Show UnionMember where
    show (UnionMember name size) =
        showName name <> " at " <> " (" <> showM size <> " bytes)"

instance Show Enumerator where
    show (Enumerator name val) = Text.unpack name <> " = " <> show val

instance Show TypeContent where
    show (Struct members) = "Struct" <> show members
    show (Union members) = "Union" <> show members
    show (Enum members) = "Enum" <> show members

instance Show Type where
    show (Type name size content) =
        showName name <> " size=" <> showM size <> " " <>
        show content

assertLookup :: (Show k, Eq k) => Dwarf.DIE -> k -> [(k, v)] -> v
assertLookup die k items =
    lookup k items
    & fromMaybe (error msg)
    where
        msg =
            "Key " <> show k <> " missing, have: " <> show keys <> " in:\n" <>
            show die
        keys = map fst items

mAttr :: Lens.APrism' DW_ATVAL a -> DW_AT -> Dwarf.DIE -> Maybe a
mAttr t k die =
    a ^? Lens._Just . Lens.clonePrism t
    where
        a = lookup k $ Dwarf.dieAttributes die

attr :: Lens.APrism' DW_ATVAL a -> DW_AT -> Dwarf.DIE -> a
attr t k die =
    a ^? Lens.clonePrism t & fromMaybe (error msg)
    where
        msg = "Bad type " ++ show a ++ " for " ++ show k
        a = assertLookup die k $ Dwarf.dieAttributes die

at_name :: Dwarf.DIE -> Maybe Text
at_name = mAttr _DW_ATVAL_STRING DW_AT_name

at_byte_size :: Dwarf.DIE -> Maybe Word64
at_byte_size = mAttr _DW_ATVAL_UINT DW_AT_byte_size

at_data_member_location :: Dwarf.DIE -> Word64
at_data_member_location = attr _DW_ATVAL_UINT DW_AT_data_member_location

at_const_value :: Dwarf.DIE -> Int64
at_const_value = attr _DW_ATVAL_INT DW_AT_const_value

at_type :: Dwarf.DIE -> Dwarf.DieID
at_type = attr _DW_ATVAL_REF DW_AT_type

at_count :: Dwarf.DIE -> Maybe Word64
at_count = mAttr _DW_ATVAL_UINT DW_AT_count

arrayInfo :: Dwarf.DIEMap -> Dwarf.DIE -> Maybe ArrayInfo
arrayInfo dieMap die =
    case Dwarf.dieTag die of
    DW_TAG_array_type ->
        Just ArrayInfo
        { _aTypeName = at_name typeDie
        , _aTypeSize = at_byte_size typeDie
        , _aCount = at_count subrange
        }
    _ -> Nothing
    where
        subrange : _ = Dwarf.dieChildren die
        typeDie = dieMap ! at_type die & Dwarf.dieRefsDIE

structMember :: Dwarf.DIEMap -> Dwarf.DIE -> Maybe StructMember
structMember dieMap die =
    case Dwarf.dieTag die of
    DW_TAG_member -> Just m
    DW_TAG_inheritance -> Nothing
    _ -> error $ "Expected struct member, found: " ++ show die
    where
        m = StructMember (at_name die) (at_data_member_location die)
            (at_byte_size typeDie) (arrayInfo dieMap typeDie)
        typeDie = dieMap ! at_type die & Dwarf.dieRefsDIE

unionMember :: Dwarf.DIEMap -> Dwarf.DIE -> UnionMember
unionMember dieMap die =
    case Dwarf.dieTag die of
    DW_TAG_member ->
        UnionMember (at_name die) $
        at_byte_size typeDie
    _ -> error $ "Expected union member, found: " ++ show die
    where
        typeDie = dieMap ! at_type die & Dwarf.dieRefsDIE

enumerator :: Dwarf.DIE -> Enumerator
enumerator die =
    case Dwarf.dieTag die of
    DW_TAG_enumerator -> Enumerator (attr _DW_ATVAL_STRING DW_AT_name die) (at_const_value die)
    _ -> error $ "Expected enumerator, found: " ++ show die

mType :: Dwarf.DIEMap -> Dwarf.DIE -> Maybe Type
mType dieMap die =
    case Dwarf.dieTag die of
    DW_TAG_structure_type   -> parse Struct (structMember dieMap)
    DW_TAG_union_type       -> parse Union (Just . unionMember dieMap)
    DW_TAG_enumeration_type -> parse Enum (Just . enumerator)
    _ -> Nothing
    where
        parse cons childParse =
            Dwarf.dieChildren die & traverse childParse
            <&> cons
            <&> Type (at_name die) (at_byte_size die)

interestingDieTrees :: Dwarf.DIEMap -> Dwarf.DIE -> [Type]
interestingDieTrees dieMap =
    go
    where
        go die =
            (mType dieMap die ^.. Lens._Just) ++
            concatMap go (Dwarf.dieChildren die)

main :: IO ()
main = do
  [filename] <- getArgs
  (_, (cuDies, dieMap)) <- loadElfDwarf Dwarf.LittleEndian filename
  cuDies
    & concatMap (interestingDieTrees dieMap)
    & mapM_ print

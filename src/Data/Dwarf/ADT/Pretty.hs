{-# LANGUAGE OverloadedStrings #-}
module Data.Dwarf.ADT.Pretty (compilationUnit, dwarf) where

import Data.Dwarf (DW_ATE(..))
import Data.Dwarf.ADT (Boxed(..), Def(..), DefType(..))
import Data.Maybe (mapMaybe)
import Text.PrettyPrint ((<>))
import qualified Data.Dwarf.ADT as ADT
import qualified Data.List as List
import qualified Text.PrettyPrint as PP

showPP :: Show a => a -> PP.Doc
showPP = PP.text . show

-- Dislike drop-prefix-length trickery as it is tightly coupled with
-- the names defined without any type safety about it.
ppATE :: DW_ATE -> PP.Doc
ppATE DW_ATE_address = "address"
ppATE DW_ATE_boolean = "boolean"
ppATE DW_ATE_complex_float = "complex_float"
ppATE DW_ATE_float = "float"
ppATE DW_ATE_signed = "signed"
ppATE DW_ATE_signed_char = "signed_char"
ppATE DW_ATE_unsigned = "unsigned"
ppATE DW_ATE_unsigned_char = "unsigned_char"
ppATE DW_ATE_imaginary_float = "imaginary_float"
ppATE DW_ATE_packed_decimal = "packed_decimal"
ppATE DW_ATE_numeric_string = "numeric_string"
ppATE DW_ATE_edited = "edited"
ppATE DW_ATE_signed_fixed = "signed_fixed"
ppATE DW_ATE_unsigned_fixed = "unsigned_fixed"
ppATE DW_ATE_decimal_float = "decimal_float"

baseTypeName :: ADT.BaseType -> PP.Doc
baseTypeName (ADT.BaseType _ _ (Just name)) = PP.text name
baseTypeName (ADT.BaseType _ encoding Nothing) = ppATE encoding

withName :: PP.Doc -> Maybe String -> PP.Doc
withName prefix Nothing = prefix
withName prefix (Just name) = prefix <> " " <> PP.text name

indent :: PP.Doc -> PP.Doc
indent x = "  " <> x

compositeMembers :: PP.Doc -> Maybe String -> [Boxed (ADT.Member a)] -> PP.Doc
compositeMembers prefix mName members =
  PP.vcat
  [ withName prefix mName <> " {"
  , indent $ PP.vcat (map memberPP members)
  , "}"
  ]
  where
    memberPP Boxed { bData = member } =
      ppType (ADT.membName member) (ADT.membType member) <> ";"

structureType :: ADT.StructureType -> PP.Doc
structureType ADT.StructureType
  { ADT.stName = mName
  , ADT.stMembers = members
  } = compositeMembers "struct" mName members

unionType :: ADT.UnionType -> PP.Doc
unionType ADT.UnionType
  { ADT.unionName = mName
  , ADT.unionMembers = members
  } = compositeMembers "union" mName members

enumerationType :: ADT.EnumerationType -> PP.Doc
enumerationType ADT.EnumerationType
  { ADT.enumName = mName
  , ADT.enumEnumerators = enumerators
  } =
  PP.vcat
  [ withName "enum" mName <> " {"
  , indent $ PP.vcat (map enumeratorPP enumerators)
  , "}"
  ]
  where
    enumeratorPP Boxed { bData = enumerator } = PP.hcat
      [ PP.text $ ADT.enumeratorName enumerator
      , " = "
      , showPP $ ADT.enumeratorConstValue enumerator
      , ","
      ]

data Precedence = Prefix | Postfix

paramList :: [Boxed ADT.FormalParameter] -> PP.Doc
paramList params = "(" <> PP.hcat (List.intersperse ", " (map param params)) <> ")"
  where
    param
      Boxed
      { bData = ADT.FormalParameter
        { ADT.formalParamName = name, ADT.formalParamType = t
        }
      } = ppType name t

ppType :: Maybe String -> ADT.TypeRef -> PP.Doc
ppType mName = result . recurseType
  where
    result (baseType, decl) = baseType <> PP.space <> decl Nothing (nameCont mName)
    nameCont Nothing = id
    nameCont (Just name) = (<> PP.text name)
    addAnnotation onPrecedence f innerDecl outerPrecedence cont =
      innerDecl innerPrecedence $ f . p . cont
      where
        p = case (outerPrecedence, innerPrecedence) of
          (Just Prefix, Just Postfix) -> PP.parens
          _ -> id
        innerPrecedence = onPrecedence outerPrecedence
    annotate onPrecedence f (btn, decl) = (btn, addAnnotation onPrecedence f decl)
    mkBaseType name = (name, const ($ ""))
    subRange ADT.SubrangeType { ADT.subRangeUpperBound = u } = "[" <> maybe "" showPP u <> "]"
    simplePrecedence = const . Just
    recurseType ADT.Void = mkBaseType "void"
    recurseType (ADT.TypeRef Boxed { bData = typ }) =
      case typ of
      DefBaseType x -> mkBaseType $ baseTypeName x
      DefTypedef x -> mkBaseType . PP.text $ ADT.tdName x
      DefStructureType ADT.StructureType { ADT.stName = Just name } ->
        mkBaseType $ "struct " <> PP.text name
      DefStructureType x@ADT.StructureType { ADT.stName = Nothing } ->
        mkBaseType $ structureType x
      DefUnionType ADT.UnionType { ADT.unionName = Just name } ->
        mkBaseType $ "union " <> PP.text name
      DefUnionType x@ADT.UnionType { ADT.unionName = Nothing } ->
        mkBaseType $ unionType x
      DefEnumerationType ADT.EnumerationType { ADT.enumName = Just name } ->
        mkBaseType $ "enum " <> PP.text name
      DefEnumerationType x@ADT.EnumerationType { ADT.enumName = Nothing } ->
        mkBaseType $ enumerationType x

      DefPtrType ADT.PtrType { ADT.ptType = t } ->
        annotate (simplePrecedence Prefix) ("*" <>) $ recurseType t
      DefConstType ADT.ConstType { ADT.ctType = t } ->
        annotate id ("const " <>) $ recurseType t
      DefVolatileType ADT.VolatileType { ADT.vtType = t } ->
        annotate id ("volatile " <>) $ recurseType t
      DefArrayType ADT.ArrayType { ADT.atType = t, ADT.atSubrangeType = r } ->
        annotate (simplePrecedence Postfix) (<> subRange (bData r)) $ recurseType t
      DefSubroutineType ADT.SubroutineType
        { ADT.subrRetType = t, ADT.subrFormalParameters = params } ->
        annotate (simplePrecedence Postfix) (<> paramList params) $ recurseType t

defTypedef :: ADT.Typedef -> PP.Doc
defTypedef (ADT.Typedef name _ typeRef) = "typedef " <> ppType (Just name) typeRef

defStructureType :: ADT.StructureType -> PP.Doc
defStructureType = structureType

defUnionType :: ADT.UnionType -> PP.Doc
defUnionType = unionType

defEnumerationType :: ADT.EnumerationType -> PP.Doc
defEnumerationType = enumerationType

defSubprogram :: ADT.Subprogram -> PP.Doc
defSubprogram ADT.Subprogram
  { ADT.subprogName = name
  , ADT.subprogType = typ
  , ADT.subprogFormalParameters = params
  , ADT.subprogLowPC = lowPC
  , ADT.subprogHighPC = highPC
  } =
  PP.hcat
  [ ppType (Just name) typ, paramList params
  , " at (", m lowPC, ":", m highPC, ")"
  ]
  where
    m = maybe "" showPP

defVariable :: (name -> Maybe String) -> ADT.Variable name -> PP.Doc
defVariable f ADT.Variable
  { ADT.varName = name, ADT.varType = typeRef } = ppType (f name) typeRef

defType :: DefType -> Maybe PP.Doc
defType t = case t of
  DefBaseType _        -> Nothing
  DefPtrType _         -> Nothing
  DefConstType _       -> Nothing
  DefVolatileType _    -> Nothing
  DefArrayType _       -> Nothing
  DefSubroutineType _  -> Nothing
  DefTypedef x         -> Just $ "Typedef: "         <> defTypedef x
  DefStructureType x   -> Just $ "StructureType: "   <> defStructureType x
  DefUnionType x       -> Just $ "UnionType: "       <> defUnionType x
  DefEnumerationType x -> Just $ "EnumerationType: " <> defEnumerationType x

def :: Boxed Def -> Maybe PP.Doc
def Boxed { bDieId = i, bData = d } = fmap ((showPP i <> " " <>) . (<> ";")) $
  case d of
  DefType t            -> defType t
  DefSubprogram x      -> Just $ "Subprogram: "      <> defSubprogram x
  DefVariable x        -> Just $ "Variable: "        <> defVariable Just x

compilationUnit :: Boxed ADT.CompilationUnit -> PP.Doc
compilationUnit
  (Boxed i (ADT.CompilationUnit producer language name compDir lowPc highPc _stmtList defs))
  = PP.vcat
    [ "Compilation unit at " <> showPP i
    , indent $ PP.vcat
      [ "producer = " <> showPP producer
      , "language = " <> showPP language
      , "name     = " <> showPP name
      , "compDir  = " <> showPP compDir
      , "lowPc    = " <> showPP lowPc
      , "highPc   = " <> showPP highPc
      , "defs     = "
      , "  " <> PP.vcat (mapMaybe def defs)
      ]
    ]

dwarf :: ADT.Dwarf -> PP.Doc
dwarf (ADT.Dwarf compilationUnits) =
  PP.vcat $ map compilationUnit compilationUnits

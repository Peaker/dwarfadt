{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Data.Dwarf.Matchers where

import qualified Data.ByteString as BS
import           Data.Dwarf
import           Data.Int (Int64)
import qualified Data.Monoid as Monoid
import           Data.Text (Text)
import           Data.Word (Word64)

{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

type ATVAL_NamedMatcher a = (Text, DW_ATVAL -> Maybe a)


_ATVAL_INT :: ATVAL_NamedMatcher Int64
_ATVAL_INT = ("ATVAL_INT"
             , \case {DW_ATVAL_INT x -> Just x; _ -> Nothing})

_ATVAL_UINT :: ATVAL_NamedMatcher Word64
_ATVAL_UINT = ("ATVAL_UINT"

             , \case {DW_ATVAL_UINT x -> Just x; _ -> Nothing})

_ATVAL_REF :: ATVAL_NamedMatcher DieID
_ATVAL_REF = ("ATVAL_REF"
             , \case {DW_ATVAL_REF x -> Just x; _ -> Nothing})

_ATVAL_STRING :: ATVAL_NamedMatcher Text
_ATVAL_STRING = ("ATVAL_STRING"
             , \case {DW_ATVAL_STRING x -> Just x; _ -> Nothing})

_ATVAL_BLOB :: ATVAL_NamedMatcher BS.ByteString
_ATVAL_BLOB = ("ATVAL_BLOB"
             , \case {DW_ATVAL_BLOB x -> Just x; _ -> Nothing})

_ATVAL_BOOL :: ATVAL_NamedMatcher Bool
_ATVAL_BOOL = ("ATVAL_BOOL"
             , \case {DW_ATVAL_BOOL x -> Just x; _ -> Nothing})

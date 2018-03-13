{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Dwarf.Lens
  ( _DW_ATVAL_INT, _ATVAL_INT
  , _DW_ATVAL_UINT, _ATVAL_UINT
  , _DW_ATVAL_REF, _ATVAL_REF
  , _DW_ATVAL_STRING, _ATVAL_STRING
  , _DW_ATVAL_BLOB, _ATVAL_BLOB
  , _DW_ATVAL_BOOL, _ATVAL_BOOL
  , ATVAL_NamedPrism
  ) where

import           Control.Lens (Getting)
import           Control.Lens.TH (makePrisms)
import qualified Data.ByteString as BS
import           Data.Dwarf (DieID, DW_ATVAL)
import           Data.Int (Int64)
import qualified Data.Monoid as Monoid
import           Data.Text (Text)
import           Data.Word (Word64)

{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

type ATVAL_NamedPrism a = (Text, Getting (Monoid.First a) DW_ATVAL a)

makePrisms ''DW_ATVAL

_ATVAL_INT :: ATVAL_NamedPrism Int64
_ATVAL_INT = ("ATVAL_INT", _DW_ATVAL_INT)

_ATVAL_UINT :: ATVAL_NamedPrism Word64
_ATVAL_UINT = ("ATVAL_UINT", _DW_ATVAL_UINT)

_ATVAL_REF :: ATVAL_NamedPrism DieID
_ATVAL_REF = ("ATVAL_REF", _DW_ATVAL_REF)

_ATVAL_STRING :: ATVAL_NamedPrism Text
_ATVAL_STRING = ("ATVAL_STRING", _DW_ATVAL_STRING)

_ATVAL_BLOB :: ATVAL_NamedPrism BS.ByteString
_ATVAL_BLOB = ("ATVAL_BLOB", _DW_ATVAL_BLOB)

_ATVAL_BOOL :: ATVAL_NamedPrism Bool
_ATVAL_BOOL = ("ATVAL_BOOL", _DW_ATVAL_BOOL)

{-# LANGUAGE TemplateHaskell #-}
module Data.Dwarf.Lens
  ( dW_ATVAL_INT, aTVAL_INT
  , dW_ATVAL_UINT, aTVAL_UINT
  , dW_ATVAL_REF, aTVAL_REF
  , dW_ATVAL_STRING, aTVAL_STRING
  , dW_ATVAL_BLOB, aTVAL_BLOB
  , dW_ATVAL_BOOL, aTVAL_BOOL
  , getATVal, ATVAL_NamedPrism
  ) where

import Control.Lens (Getting, (^?))
import Control.Lens.TH (makePrisms)
import Data.Dwarf (DieID, DW_ATVAL)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import qualified Data.ByteString as BS
import qualified Data.Monoid as Monoid

{-# ANN module "HLint: ignore Use camelCase" #-}

type ATVAL_NamedPrism a = (String, Getting (Monoid.First a) DW_ATVAL DW_ATVAL a a)

makePrisms ''DW_ATVAL

aTVAL_INT :: ATVAL_NamedPrism Int64
aTVAL_INT = ("ATVAL_INT", dW_ATVAL_INT)

aTVAL_UINT :: ATVAL_NamedPrism Word64
aTVAL_UINT = ("ATVAL_UINT", dW_ATVAL_UINT)

aTVAL_REF :: ATVAL_NamedPrism DieID
aTVAL_REF = ("ATVAL_REF", dW_ATVAL_REF)

aTVAL_STRING :: ATVAL_NamedPrism String
aTVAL_STRING = ("ATVAL_STRING", dW_ATVAL_STRING)

aTVAL_BLOB :: ATVAL_NamedPrism BS.ByteString
aTVAL_BLOB = ("ATVAL_BLOB", dW_ATVAL_BLOB)

aTVAL_BOOL :: ATVAL_NamedPrism Bool
aTVAL_BOOL = ("ATVAL_BOOL", dW_ATVAL_BOOL)

getATVal :: String -> ATVAL_NamedPrism a -> DW_ATVAL -> a
getATVal prefix (typName, typ) atval =
  fromMaybe (error msg) $ atval ^? typ
  where
    msg = concat [prefix, " is: ", show atval, " but expected: ", typName]

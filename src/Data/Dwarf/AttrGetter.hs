{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Data.Dwarf.AttrGetter
  ( AttrGetterT, run
  , findAttrVals, findAttrVal
  , findAttrs, findAttr
  , getAttr
  ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Lens ((^?))
import           Control.Monad (liftM)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as Reader
import           Control.Monad.Trans.State (StateT(..))
import qualified Control.Monad.Trans.State as State
import           Data.Dwarf (DIE(..), DW_AT, DW_ATVAL)
import qualified Data.Dwarf.Lens as DwarfLens
import           Data.List (partition)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           TextShow (TextShow(..))

newtype AttrGetterT m a = AttrGetterT (ReaderT Text (StateT [(DW_AT, DW_ATVAL)] m) a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans AttrGetterT where
  lift = AttrGetterT . lift . lift

run :: DIE -> AttrGetterT m a -> m (a, [(DW_AT, DW_ATVAL)])
run die (AttrGetterT act) =
  (`runStateT` dieAttributes die) .
  (`runReaderT` (" in " <> showt die)) $
  act

getSuffix :: Monad m => AttrGetterT m Text
getSuffix = AttrGetterT Reader.ask

findAttrVals :: Monad m => DW_AT -> AttrGetterT m [DW_ATVAL]
findAttrVals at = AttrGetterT . lift $ do
  (matches, mismatches) <- State.gets $ partition ((== at) . fst)
  State.put mismatches
  return $ snd <$> matches

findAttrVal :: Monad m => DW_AT -> AttrGetterT m (Maybe DW_ATVAL)
findAttrVal at = AttrGetterT . lift $ do
  (unmatched, rest) <- State.gets $ break ((at ==) . fst)
  case rest of
    (_, val) : afterMatch -> do
      State.put (unmatched ++ afterMatch)
      return $ Just val
    _ -> return Nothing

getATVal :: Text -> Text -> DwarfLens.ATVAL_NamedPrism a -> DW_ATVAL -> a
getATVal prefix suffix (typName, typ) atval =
  fromMaybe (error msg) $ atval ^? typ
  where
    msg = Text.unpack $ mconcat [prefix, " is: ", showt atval, " but expected: ", typName, suffix]

toVal ::
  (Monad m, Functor f, TextShow a) =>
  (a -> AttrGetterT m (f DW_ATVAL)) ->
  a -> DwarfLens.ATVAL_NamedPrism b -> AttrGetterT m (f b)
toVal finder at prism = do
  suffix <- getSuffix
  (liftM . fmap) (getATVal (showt at) suffix prism) $ finder at

findAttrs :: Monad m => DW_AT -> DwarfLens.ATVAL_NamedPrism a -> AttrGetterT m [a]
findAttrs = toVal findAttrVals

findAttr :: Monad m => DW_AT -> DwarfLens.ATVAL_NamedPrism a -> AttrGetterT m (Maybe a)
findAttr = toVal findAttrVal

getAttr :: Monad m => DW_AT -> DwarfLens.ATVAL_NamedPrism a -> AttrGetterT m a
getAttr at prism = do
    suffix <- getSuffix
    (liftM . fromMaybe . error . Text.unpack)
        ("Could not find " <> showt at <> suffix) $
        findAttr at prism

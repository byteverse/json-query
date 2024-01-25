{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- This module has been disabled. It is in an experimental stage.
-- It may be added back in the future.

-- module Json.Query
--   ( Query(..)
--     -- * Lookup
--   , query
--   ) where

import Control.Monad.ST (runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Foldable (foldlM)
import Data.Kind (Type)
import Data.Primitive (SmallArray)
import Json (Value (Array))
import Json.Path (Path (Index, Nil))

import qualified Data.Primitive as PM

data Query :: Type -> Type where
  QueryOpaque :: (Value -> Maybe a) -> Query a
  QueryArray :: Query a -> Query (SmallArray a)
  QueryObject :: Fields a -> Query a

query :: Query a -> Value -> Either Path a
query = go Nil
 where
  go :: forall b. Path -> Query b -> Value -> Either Path b
  go !p (QueryOpaque f) v = maybe (Left p) Right (f v)
  go !p (QueryArray q) (Array xs) = runST do
    let !len = length xs
    dst <- PM.newSmallArray len errorThunk
    runExceptT $ do
      _ <-
        foldlM
          ( \ix x -> do
              !y <- ExceptT (pure (go (Index ix p) q x))
              lift (PM.writeSmallArray dst ix y)
              pure (ix + 1)
          )
          0
          xs
      lift (PM.unsafeFreezeSmallArray dst)
  go !p (QueryArray q) !_ = Left p

errorThunk :: a
{-# NOINLINE errorThunk #-}
errorThunk = errorWithoutStackTrace "Json.Query: implementation mistake"

data Fields :: Type -> Type

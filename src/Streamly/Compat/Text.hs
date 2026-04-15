{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

-- | Efficient interoperability between
-- <https://hackage.haskell.org/package/streamly streamly> arrays and
-- <https://hackage.haskell.org/package/text text>.
--
-- This module provides zero-overhead conversion between strict 'Text'
-- and streamly’s 'Word8' streams or 'Array' 'Word8' type.
--
-- In streamy we either work directly with 'Word8' streams or 'Array' of
-- 'Word8'. The strict 'Text' type is equivalent to a UTF-8 encoded 'Array
-- Word8' in Streamly, the underlying types are compatible so we just need to
-- rewrap it to interconvert.

module Streamly.Compat.Text
  (
  -- * Construction
    unsafeFromArray
  , unsafeCreate

  -- * Elimination
  , toArray
  , reader
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Word (Word8)
import GHC.Exts (unsafeCoerce#)
import Streamly.Data.Array (Array)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Unfold (Unfold, lmap)

import qualified Data.Text as T
import qualified Data.Text.Array as TArr
import qualified Streamly.Data.Array as Array

-- Internal imports
-- Tracks all the places where we are using internals of Text
import qualified Data.Text.Internal as TextInternal
import qualified Streamly.Internal.Data.Array as ArrayInternal
import qualified Streamly.Internal.Data.MutByteArray as MBArrInternal

import Prelude hiding (read)

-- This module currently only supports text >= 2.
-- For text < 2 we need to consider utf16 encoding.

#if MIN_VERSION_streamly_core(0,2,2)
#define EMPTY MBArrInternal.empty
#else
#define EMPTY MBArrInternal.nil
#endif

#if MIN_VERSION_streamly_core(0,3,0)
#define CREATE_OF Array.createOf
#define CREATE Array.create
#else
#define CREATE_OF Array.writeN
#define CREATE Array.write
#endif

-- | Convert a 'Text' to an array of 'Word8'. It can be done in constant time.
{-# INLINE toArray #-}
toArray :: Text -> Array Word8
toArray (TextInternal.Text (TArr.ByteArray _) _ len)
    | len == 0 = ArrayInternal.Array EMPTY 0 0
toArray (TextInternal.Text (TArr.ByteArray barr#) off8 len8) =
    ArrayInternal.Array (MBArrInternal.MutByteArray (unsafeCoerce# barr#)) off8 (off8 + len8)

-- | Treat an array of 'Word8' as 'Text'.
--
-- This function is unsafe: the caller must ensure that the 'Array' 'Word8' has a
-- valid UTF-8 encoding.
--
-- This function unwraps the 'Array' and wraps it with 'Text' constructors and
-- hence the operation is performed in constant time.
{-# INLINE unsafeFromArray #-}
unsafeFromArray :: Array Word8 -> Text
unsafeFromArray ArrayInternal.Array {..}
    | len8 == 0 = T.empty
    | otherwise = TextInternal.Text (TArr.ByteArray (unsafeCoerce# marr#)) off8 len8

    where

    len8 = arrEnd - arrStart
    off8 = arrStart
    !(MBArrInternal.MutByteArray marr#) = arrContents

-- | Unfold a 'Text' to a stream of Word8.
{-# INLINE reader #-}
reader :: Monad m => Unfold m Text Word8
reader = lmap toArray Array.reader

-- | Fold a stream of Word8 to a 'Text' of given size in bytes.
{-# INLINE _unsafeCreateOf #-}
_unsafeCreateOf :: MonadIO m => Int -> Fold m Word8 Text
_unsafeCreateOf i = unsafeFromArray <$> CREATE_OF i

-- | Fold a stream of Word8 to a 'Text' of appropriate size.
{-# INLINE unsafeCreate #-}
unsafeCreate :: MonadIO m => Fold m Word8 Text
unsafeCreate = unsafeFromArray <$> CREATE

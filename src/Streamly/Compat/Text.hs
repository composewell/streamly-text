{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

-- | Efficient interoperability between
-- <https://hackage.haskell.org/package/streamly streamly> arrays and
-- <https://hackage.haskell.org/package/text text>.
--
-- The strict 'Text' type is equivalent to a UTF-8 encoded 'Array' 'Word8' in
-- streamly. A 'Char' stream can be converted to a UTF-8 encoded 'Word8' stream
-- using 'Streamly.Unicode.Stream.encodeUtf8', which in turn can
-- be written as 'Array' 'Word8'. A stream of UTF-8 encoded 'Word8' or
-- 'Array' 'Word8' can be decoded using 'Streamly.Unicode.Stream.decodeUtf8' or
-- 'Streamly.Unicode.Stream.decodeUtf8Chunks', respectively.
--
-- This module provides zero-overhead conversion between strict 'Text'
-- and streamly’s 'Word8' streams or 'Array' 'Word8'.

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
import Data.Word (Word8)
import GHC.Exts (unsafeCoerce#)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Unfold (Unfold, lmap)

import qualified Data.Text as T
import qualified Data.Text.Array as TArr

-- Internal imports
import Data.Text.Internal (Text(..))
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.MutByteArray (MutByteArray(..))

import qualified Streamly.Data.Array as Array
import qualified Streamly.Internal.Data.MutByteArray as MBArr

import Prelude hiding (read)

-- This module currently only supports text >= 2.
-- For text < 2 we need to consider utf16 encoding.

#if MIN_VERSION_streamly_core(0,2,2)
#define EMPTY MBArr.empty
#else
#define EMPTY MBArr.nil
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
toArray (Text (TArr.ByteArray _) _ len)
    | len == 0 = Array EMPTY 0 0
toArray (Text (TArr.ByteArray barr#) off8 len8) =
    Array (MutByteArray (unsafeCoerce# barr#)) off8 (off8 + len8)

-- | Treat an an array of 'Word8' as 'Text'.
--
-- This function is unsafe: the caller must ensure that the 'Array' 'Word8' is a
-- valid UTF-8 encoding.
--
-- This function unwraps the 'Array' and wraps it with 'Text' constructors and
-- hence the operation is performed in constant time.
{-# INLINE unsafeFromArray #-}
unsafeFromArray :: Array Word8 -> Text
unsafeFromArray Array {..}
    | len8 == 0 = T.empty
    | otherwise = Text (TArr.ByteArray (unsafeCoerce# marr#)) off8 len8

    where

    len8 = arrEnd - arrStart
    off8 = arrStart
    !(MutByteArray marr#) = arrContents

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

{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module Streamly.Compat.Text
  ( toArray
  , unsafeFromArray

  , reader

  -- , unsafeCreateOf
  , unsafeCreate
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

{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module Streamly.External.Text
  ( toArray
  , unsafeFromArray

  , reader

  , unsafeWriteN
  , unsafeWrite
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Word (Word16)
import GHC.Exts (unsafeCoerce#)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Unfold (Unfold, lmap)

import qualified Data.Text as T
import qualified Data.Text.Array as TArr (Array(..))

-- Internal imports
import Data.Text.Internal (Text(..))
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Unboxed (MutableByteArray(..))

import qualified Streamly.Data.Array as Array
import qualified Streamly.Internal.Data.Unboxed as Unboxed (nil)

import Prelude hiding (read)

-- | Convert a 'Text' to an array of 'Word16'. It can be done in constant time.
{-# INLINE toArray #-}
toArray :: Text -> Array Word16
toArray (Text (TArr.Array _) _ len)
    | len == 0 = Array Unboxed.nil 0 0
toArray (Text (TArr.Array barr#) off16 len16) =
    let off8 = off16 * 2
        len8 = len16 * 2
    in Array (MutableByteArray (unsafeCoerce# barr#)) off8 (off8 + len8)

-- | Treat an an array of 'Word16' as 'Text'. This function is unsafe and the
-- onus is on the caller to check the sanity of 'Array' 'Word16'. This function
-- unwraps the 'Array' and wraps it with 'Text' constructors and hence the
-- operation is performed in constant time.
{-# INLINE unsafeFromArray #-}
unsafeFromArray :: Array Word16 -> Text
unsafeFromArray Array {..}
    | len8 == 0 = T.empty
    | otherwise = Text (TArr.Array (unsafeCoerce# marr#)) off16 len16

    where

    len8 = arrEnd - arrStart
    off8 = arrStart
    len16 = len8 `div` 2
    off16 = off8 `div` 2
    !(MutableByteArray marr#) = arrContents

-- | Unfold a 'Text' to a stream of Word16.
{-# INLINE reader #-}
reader :: Monad m => Unfold m Text Word16
reader = lmap toArray Array.reader

-- | Fold a stream of Word16 to a 'Text' of given size in bytes.
{-# INLINE unsafeWriteN #-}
unsafeWriteN :: MonadIO m => Int -> Fold m Word16 Text
unsafeWriteN i = unsafeFromArray <$> Array.writeN i

-- | Fold a stream of Word16 to a 'Text' of appropriate size.
{-# INLINE unsafeWrite #-}
unsafeWrite :: MonadIO m => Fold m Word16 Text
unsafeWrite = unsafeFromArray <$> Array.write

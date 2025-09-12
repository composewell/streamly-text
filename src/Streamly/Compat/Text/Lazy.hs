{-# LANGUAGE CPP #-}

-- | Efficient interoperability between
-- <https://hackage.haskell.org/package/streamly streamly> arrays and
-- <https://hackage.haskell.org/package/text text>.
--
-- The lazy 'Text' type is equivalent to a UTF-8 encoded stream of 'Array
-- Word8' in streamly. A 'Char' stream can be converted to a UTF-8 encoded
-- 'Word8' stream using 'Streamly.Unicode.Stream.encodeUtf8', which in turn can
-- be written as 'Array' 'Word8'. A stream of UTF-8 encoded 'Word8' or 'Array'
-- 'Word8' can be decoded using 'Streamly.Unicode.Stream.decodeUtf8' or
-- 'Streamly.Unicode.Stream.decodeUtf8Chunks', respectively.
--
-- This module provides zero-overhead conversion between lazy 'Text' and
-- streamly’s 'Array Word8' or 'Word8' streams.

module Streamly.Compat.Text.Lazy
  (
  -- * Construction
    unsafeFromChunksIO
  , unsafeFromChunks

  -- * Elimination
  , reader
  , toChunks
  , chunkReader
  )
where

import Data.Word (Word8)
import Streamly.Data.Array (Array)
import System.IO.Unsafe (unsafeInterleaveIO)
import Streamly.Data.Stream (Stream)

-- Internal imports
import Data.Text.Internal.Lazy (Text(..), chunk)
import Streamly.Internal.Data.Stream (Step(..))
import Streamly.Internal.Data.Unfold (Unfold(..))

import qualified Streamly.Compat.Text as Strict
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Data.Stream as Stream

import Prelude hiding (read)

#if MIN_VERSION_streamly_core(0,3,0)
#define UNFOLD_EACH Unfold.unfoldEach
#else
#define UNFOLD_EACH Unfold.many
#endif

-- | Unfold a lazy 'Text' to a stream of 'Array Word8'.
{-# INLINE  chunkReader #-}
chunkReader :: Monad m => Unfold m Text (Array Word8)
chunkReader = Unfold step seed
  where
    seed = return
    step (Chunk bs bl) = return $ Yield (Strict.toArray bs) bl
    step Empty = return Stop

-- | Unfold a lazy 'Text' to a stream of 'Word8'.
{-# INLINE reader #-}
reader :: Monad m => Unfold m Text Word8
reader = UNFOLD_EACH Array.reader chunkReader

-- XXX Should this be called readChunks?
-- | Convert a lazy 'Text' to a stream of 'Array Word8'.
{-# INLINE toChunks #-}
toChunks :: Monad m => Text -> Stream m (Array Word8)
toChunks = Stream.unfold chunkReader

-- | IMPORTANT NOTE: This function is lazy only for lazy monads (e.g.
-- Identity). For strict monads (e.g. /IO/) it consumes the entire input before
-- generating the output. For /IO/ monad use 'unsafeFromChunksIO' instead.
--
-- Convert a stream of 'Array' 'Word8' to a lazy 'Text'.
--
-- Unsafe because the caller must ensure that each 'Array Word8'
-- in the stream is UTF-8 encoded and terminates at Char boundary.
--
-- For strict monads like /IO/ you could create a newtype wrapper to make the
-- monad bind operation lazy and lift the stream to that type using hoist, then
-- you can use this function to generate the text lazily. For example you can
-- wrap the /IO/ type to make the bind lazy like this:
--
-- @
-- newtype LazyIO a = LazyIO { runLazy :: IO a } deriving (Functor, Applicative)
--
-- liftToLazy :: IO a -> LazyIO a
-- liftToLazy = LazyIO
--
-- instance Monad LazyIO where
--   return = pure
--   LazyIO a >>= f = LazyIO (unsafeInterleaveIO a >>= unsafeInterleaveIO . runLazy . f)
-- @
--
-- /unsafeFromChunks/ can then be used as,
--
-- @
-- {-# INLINE unsafeFromChunksIO #-}
-- unsafeFromChunksIO :: Stream IO (Array Word8) -> IO Text
-- unsafeFromChunksIO str = runLazy (unsafeFromChunks (Stream.hoist liftToLazy str))
-- @
{-# INLINE unsafeFromChunks #-}
unsafeFromChunks :: Monad m => Stream m (Array Word8) -> m Text
unsafeFromChunks = Stream.foldr chunk Empty . fmap Strict.unsafeFromArray

-- | Convert a stream of 'Array Word8' to a lazy 'Text' in the
-- /IO/ monad.
--
-- Unsafe because the caller must ensure that each 'Array Word8'
-- in the stream is UTF-8 encoded and terminates at Char boundary.
{-# INLINE unsafeFromChunksIO #-}
unsafeFromChunksIO :: Stream IO (Array Word8) -> IO Text
unsafeFromChunksIO =
    -- Although the /IO/ monad is strict in nature we emulate laziness using
    -- 'unsafeInterleaveIO'.
    Stream.foldrM (\x b -> chunk x <$> unsafeInterleaveIO b) (pure Empty)
        . fmap Strict.unsafeFromArray

module Streamly.External.Text.Lazy
  ( chunkReader
  , reader

  , toChunks
  , unsafeFromChunks
  , unsafeFromChunksIO
  )
where

import Data.Word (Word16)
import Streamly.Data.Array (Array)
import System.IO.Unsafe (unsafeInterleaveIO)
import Streamly.Data.Stream (Stream)

-- Internal imports
import Data.Text.Internal.Lazy (Text(..), chunk)
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import qualified Streamly.External.Text as Strict
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Data.Stream as Stream

import Prelude hiding (read)

-- | Unfold a lazy 'Text' to a stream of 'Array' 'Words'.
{-# INLINE  chunkReader #-}
chunkReader :: Monad m => Unfold m Text (Array Word16)
chunkReader = Unfold step seed
  where
    seed = return
    step (Chunk bs bl) = return $ Yield (Strict.toArray bs) bl
    step Empty = return Stop

-- | Unfold a lazy 'Text' to a stream of Word16
{-# INLINE reader #-}
reader :: Monad m => Unfold m Text Word16
reader = Unfold.many Array.reader chunkReader

-- XXX Should this be called readChunks?
-- | Convert a lazy 'Text' to a serial stream of 'Array' 'Word16'.
{-# INLINE toChunks #-}
toChunks :: Monad m => Text -> Stream m (Array Word16)
toChunks = Stream.unfold chunkReader

-- | Convert a serial stream of 'Array' 'Word16' to a lazy 'Text'.
--
-- This function is unsafe and the onus is on the caller to check the sanity of
-- the stream of 'Array' 'Word16'.
--
-- IMPORTANT NOTE: This function is lazy only for lazy monads
-- (e.g. Identity). For strict monads (e.g. /IO/) it consumes the entire input
-- before generating the output. For /IO/ monad please use unsafeFromChunksIO
-- instead.
--
-- For strict monads like /IO/ you could create a newtype wrapper to make the
-- monad bind operation lazy and lift the stream to that type using hoist, then
-- you can use this function to generate the bytestring lazily. For example you
-- can wrap the /IO/ type to make the bind lazy like this:
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
-- @
-- {-# INLINE unsafeFromChunksIO #-}
-- unsafeFromChunksIO :: Stream IO (Array Word16) -> IO Text
-- unsafeFromChunksIO str = runLazy (unsafeFromChunks (Stream.hoist liftToLazy str))
-- @
{-# INLINE unsafeFromChunks #-}
unsafeFromChunks :: Monad m => Stream m (Array Word16) -> m Text
unsafeFromChunks = Stream.foldr chunk Empty . fmap Strict.unsafeFromArray

-- | Convert a serial stream of 'Array' 'Word16' to a lazy 'Text' in the
-- /IO/ monad.
{-# INLINE unsafeFromChunksIO #-}
unsafeFromChunksIO :: Stream IO (Array Word16) -> IO Text
unsafeFromChunksIO =
    -- Although the /IO/ monad is strict in nature we emulate laziness using
    -- 'unsafeInterleaveIO'.
    Stream.foldrM (\x b -> chunk x <$> unsafeInterleaveIO b) (pure Empty)
        . fmap Strict.unsafeFromArray

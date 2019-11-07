{-# LANGUAGE RankNTypes #-}

module Main (main) where

import System.Random (randomIO)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.Text ()
import System.IO (hClose)
import Data.Functor.Identity (Identity(..))

import qualified Streamly.Internal.Data.Array as Array (castUnsafe)

import qualified Data.Text as BS
import qualified Data.Text.Lazy as BSL
import qualified Streamly.FileSystem.File as File
import qualified Streamly.External.Text as Strict
import qualified Streamly.External.Text.Lazy as Lazy
import qualified Streamly.Data.Stream as Stream

pipeline ::
       (Eq t, Show t)
    => t
    -> (t -> a)
    -> (a -> m t)
    -> (forall b. m b -> IO b)
    -> IO ()
pipeline txt fromTxt toTxt runMonad = do
    txt1 <-
        runMonad $ do
            let a = fromTxt txt
            toTxt a
    txt1 `shouldBe` txt

pipelinePropStrict ::
       String
    -> (BS.Text -> a)
    -> (a -> m BS.Text)
    -> (forall b. m b -> IO b)
    -> Spec
pipelinePropStrict info fromTxt toTxt runMonad =
    prop info $ \txt -> pipeline txt fromTxt toTxt runMonad

pipelinePropLazy ::
       String
    -> (BSL.Text -> a)
    -> (a -> m BSL.Text)
    -> (forall b. m b -> IO b)
    -> Spec
pipelinePropLazy info fromTxt toTxt runMonad =
    prop info $ \txt -> pipeline txt fromTxt toTxt runMonad

writeRead :: Int -> IO ()
writeRead n = do
    str <- sequence $ replicate n (randomIO :: IO Char)
    let txt = BSL.pack str
    withSystemTempFile "temp" $ \fp hdl -> do
        hClose hdl
        let strm = fmap Array.castUnsafe $ Lazy.toChunks txt
        Stream.fold (File.writeChunks fp) strm
        let strm1 = fmap Array.castUnsafe $ File.readChunks fp
        txt1 <- Lazy.unsafeFromChunksIO strm1
        txt1 `shouldBe` txt

main :: IO ()
main = do
    hspec $ do
        describe "IO"
            $ it "write & read (100000)" $ writeRead 100000
        describe "Text tests" $ do
            pipelinePropStrict
                "Strict.unsafeFromArray . Strict.toArray"
                Strict.toArray
                (pure . Strict.unsafeFromArray)
                (pure . runIdentity)
            pipelinePropStrict
                "Strict.unsafeWrite . Strict.reader"
                (Stream.unfold Strict.reader)
                (Stream.fold Strict.unsafeWrite)
                id
            pipelinePropLazy
                "Lazy.unsafeFromChunks . Lazy.toChunks"
                Lazy.toChunks
                Lazy.unsafeFromChunks
                (pure . runIdentity)
            pipelinePropLazy
                "Lazy.unsafeFromChunksIO . Lazy.toChunks"
                Lazy.toChunks
                Lazy.unsafeFromChunksIO
                id
            pipelinePropLazy
                "Lazy.unsafeFromChunks . Lazy.chunkReader"
                (Stream.unfold Lazy.chunkReader)
                Lazy.unsafeFromChunks
                (pure . runIdentity)
            pipelinePropLazy
                "fmap Text.fromStrict . Strict.unsafeWrite . Lazy.reader"
                (Stream.unfold Lazy.reader)
                (fmap BSL.fromStrict . Stream.fold Strict.unsafeWrite)
                id

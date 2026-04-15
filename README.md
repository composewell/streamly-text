# streamly-text

Efficient interoperability between
[streamly](https://hackage.haskell.org/package/streamly) arrays and
[text](https://hackage.haskell.org/package/text).

This library provides **zero-overhead** conversions between the `Text`
type and `streamly` Array types, making it easier to use one in place of
the other. While `text` uses unpinned memory, streamly arrays can use
pinned or unpinned memory and can be intercoverted if required.

The strict `Text` type is equivalent to UTF-8 encoded `Array Word8` in
Streamly, the underlying types are compatible so we just need to rewrap
it to interconvert.

In streamly we work with explicit streams instead of lazy types.
The lazy 'Text' type is equivalent to a stream of UTF-8 encoded 'Array
Word8' in streamly.

When working with streams we can work directly with Char streams or with
streams of encoded chunks of char sequences i.e. `Array Word8`. A `Char`
stream can be converted to UTF-8 encoded `Word8` stream using `encodeUtf8`
from `Streamly.Unicode.Stream` which in turn can be written as `Array
Word8`, and a stream of UTF-8 encoded `Word8` or `Array Word8` can be
decoded using `decodeUtf8` or `decodeUtf8Chunks`.

# streamly-text

Efficient interoperability between
[streamly](https://hackage.haskell.org/package/streamly) arrays and
[text](https://hackage.haskell.org/package/text).

The strict `Text` type is equivalent to UTF-8 encoded `Array Word8` in Streamly
and lazy `Text` type is equivalent to a stream of `Array Word8`.

A `Char` stream can be converted to UTF-8 encoded `Word8` stream using
`encodeUtf8` from `Streamly.Unicode.Stream` which in turn can be written as
`Array Word8`, and a stream of UTF-8 encoded `Word8` or `Array Word8` can be
decoded using `decodeUtf8` or `decodeUtf8Chunks`.

This library provides zero-overhead and streaming conversions between
the `Text` type and `streamly` Array types, making it easier to use
Array and Array stream based functions on `Text`.

## Features

- **Strict `Text` ↔ `Array Word8`**
  Convert between strict `Text` and `streamly`’s `Word8` stream or
  `Array Word8` without any overhead.

- **Lazy `Text` ↔ Stream of `Array Word8`**
  Convert between lazy `Text` and a stream of `Array Word8`.

# streamly-text

Library for streamly and text interoperation.

This library is to enable interoperation of streamly with existing code that
uses `Text`.

The package provides APIs to interconvert between strict `Text` and streamly
`Array Word8` and between lazy `Text` and stream of `Array Word8`.

The interconversion in the case of strict `Text` and streamly `Array Word8` has
no overhead.

module ZlibDecompression (
    DecompressionResult (..),
    DecompressError,
    decompressPartial,
) where

import Codec.Compression.Zlib (defaultDecompressParams)
import Codec.Compression.Zlib.Internal (
    DecompressError,
    DecompressStream (..),
    decompressST,
    zlibFormat,
 )
import Control.Monad.ST.Lazy (ST, runST)
import Data.ByteString.Lazy as BL (ByteString, empty, fromStrict, toStrict)
import Prelude hiding (take)

data DecompressionResult = DecompressionResult
    { decompressedData :: BL.ByteString
    , unconsumedInput :: BL.ByteString
    }

decompressPartial :: BL.ByteString -> Either DecompressError DecompressionResult
decompressPartial input = do
    let decompresser = decompressST zlibFormat defaultDecompressParams
    runST $ decompressLoop BL.empty decompresser
  where
    decompressLoop ::
        BL.ByteString ->
        DecompressStream (ST s) ->
        ST s (Either DecompressError DecompressionResult)
    decompressLoop output (DecompressInputRequired next) = do
        decompressLoop output =<< next (BL.toStrict input)
    decompressLoop output (DecompressOutputAvailable outChunk next) = do
        decompressLoop (output `mappend` BL.fromStrict outChunk) =<< next
    decompressLoop output (DecompressStreamEnd unconsumed) = do
        pure $ Right $ DecompressionResult output (BL.fromStrict unconsumed)
    decompressLoop _ (DecompressStreamError err) =
        pure $ Left err

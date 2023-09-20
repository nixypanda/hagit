{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

module ZlibDecompression (
    decompressParser,
) where

import Codec.Compression.Zlib (defaultDecompressParams)
import Codec.Compression.Zlib.Internal (
    DecompressError,
    DecompressStream (..),
    decompressST,
    zlibFormat,
 )
import Control.Monad.ST.Lazy (ST, runST)
import Data.Attoparsec.ByteString.Lazy (Parser, takeLazyByteString)
import Data.Attoparsec.Internal.Types qualified as AIT
import Data.ByteString.Lazy qualified as BL
import Prelude hiding (take)

decompressParser :: Parser BL.ByteString
decompressParser = do
    everything <- takeLazyByteString
    case decompressPartial everything of
        Left err -> fail $ show err
        Right DecompressionResult{..} -> do
            let goBack = BL.length unconsumedInput
            _ <- goBackParser (fromIntegral goBack)
            pure decompressedData

goBackParser :: Int -> Parser ()
goBackParser n = AIT.Parser $
    \t (AIT.Pos pos_) more _lose success ->
        success t (AIT.Pos $ pos_ - n) more ()

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

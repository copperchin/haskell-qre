module Assembler where

import Codec.Picture (PngSavable (encodePng))
import qualified Codec.QRCode as QR
import qualified Data.ByteString.Lazy as Bytes

import Codec.QRCode.JuicyPixels (toImage)

type Message = String

encode :: QR.ErrorLevel -> Message -> Maybe QR.QRImage
encode errCorrection =
    QR.encode
        (QR.defaultQRCodeOptions errCorrection)
        QR.Iso8859_1OrUtf8WithoutECI

generateImg :: QR.ErrorLevel -> Int -> Message -> Maybe Bytes.ByteString
generateImg errCorrection pxlDim msg = do
    result <- encode errCorrection msg
    let
        border = 4
        scale = pxlDim `div` (QR.qrImageSize result + (2 * border))
    pure (encodePng $ toImage border scale result)

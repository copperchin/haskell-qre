module Main (main) where

import qualified Data.ByteString.Lazy as Bytes
import Options.Applicative (handleParseResult)
import QRE.Assembler (generateImg)
import QRE.CLI (Options (..), argparse)
import qualified QRE.Data.Network as Network
import System.Environment (getArgs)

main :: IO ()
main = do
    setting <- getArgs >>= pure <$> argparse >>= handleParseResult
    let inputData = Network.generateScheme (credentials setting) (hideName setting)
        imgWidth = 360
        -- \^ TODO: option/prompt for output size/scale?
        errCorrection = grade setting
        outpath = output setting

    case generateImg errCorrection imgWidth inputData of
        Nothing -> putStrLn "Failed."
        Just result -> do
            Bytes.writeFile outpath result
            putStrLn ("Success! Generated file: " ++ outpath)

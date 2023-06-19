{-# LANGUAGE OverloadedStrings #-}

module Main where

import Assembler
import Cmd
import qualified Data.ByteString.Lazy as Bytes
import Options.Applicative (handleParseResult)
import System.Environment (getArgs)
import Wifi

main :: IO ()
main = do
    setting <- getArgs >>= pure <$> argparse >>= handleParseResult
    let inputData = generateScheme (credentials setting) (hideName setting)
        imgWidth = 360
        -- \^ TODO: option/prompt for output size/scale?
        errCorrection = grade setting
        outpath = output setting

    case generateImg errCorrection imgWidth inputData of
        Nothing -> putStrLn "Failed."
        Just result -> do
            Bytes.writeFile outpath result
            putStrLn ("Success! Generated file: " ++ outpath)

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module QRE.CLI (argparse, Options (..)) where

import Codec.QRCode (ErrorLevel (..))
import Control.Exception (Exception, throw)
import Data.List (intercalate)
import Options.Applicative
import QRE.Data.Network (Credentials (..), SSID, Security (..))

data ArgParseError = MissingPassword | MissingEncryption deriving (Eq)
instance Exception ArgParseError
instance Show ArgParseError where
    show MissingPassword = "Password argument required."
    show MissingEncryption = "Password provided but security protocol undeclared."

instance Show ErrorLevel where
    show L = "[L]ow correction level (~7%)"
    show M = "[M]edium correction level (~15%)"
    show Q = "[Q]uality correction level (~25%)"
    show H = "[H]igh correction level (~30%)"

data Options = Options
    { hideName :: Bool
    -- ^ is network name hidden?
    , output :: FilePath
    , credentials :: Credentials
    , grade :: ErrorLevel
    }
    deriving (Show, Eq)

defaultOptions :: Options
defaultOptions =
    Options
        { hideName = False
        , output = "wifi-connect-QR.png"
        , credentials = Auth ("network", None :: Security)
        , grade = L
        }

ssid_arg :: Parser SSID
ssid_arg =
    strArgument
        ( metavar "SSID"
            <> help "Name of network to connect to."
        )

secretpass_arg :: Parser (Maybe String)
secretpass_arg =
    optional $
        strArgument
            (metavar "PASSWORD" <> help "Passphrase for network.")

encryptionK_opt :: Parser (Maybe (String -> Security))
encryptionK_opt =
    optional (wpa_flag <|> wep_flag)
  where
    wpa_flag = flag' WPA (long "wpa" <> help "Network uses WPA or WPA2 encryption.")
    wep_flag = flag' WEP (long "wep" <> help "Network uses WEP for security.")

hideName_opt :: Parser Bool
hideName_opt =
    switch
        ( help "file path to save the QRCode as."
            <> long "hidden"
            <> showDefault
            <> help "Whether to display the name of network when joining."
        )

output_opt :: Parser FilePath
output_opt =
    strOption
        ( help "file path to save the QRCode as."
            <> long "output"
            <> short 'o'
            <> metavar "FILEPATH"
            <> showDefault
            <> value (output defaultOptions)
        )

encryption_opt :: Parser Security
encryption_opt = comp <$> encryptionK_opt <*> secretpass_arg
  where
    comp Nothing Nothing = None
    comp (Just protocol) (Just pass) = protocol pass
    comp Nothing _ = throw MissingEncryption
    comp _ Nothing = throw MissingPassword

credP :: Parser Credentials
credP = Auth <$> ((,) <$> ssid_arg <*> encryption_opt)

correctionP :: Parser ErrorLevel
correctionP =
    option
        gradeReader
        ( help ("The level of built-in error correction: " ++ validValues)
            <> long "grade"
            <> short 'g'
            <> showDefault
            <> value (grade defaultOptions)
        )
  where
    validValues = intercalate "\n\t-" $ map show [L, M, Q, H]
    gradeReader :: ReadM ErrorLevel
    gradeReader = eitherReader getGrade
    getGrade "L" = Right L
    getGrade "M" = Right M
    getGrade "Q" = Right Q
    getGrade "H" = Right H
    getGrade _inValid = Left $ "Invalid grade; Expected one of: L, M, Q, H; got " ++ show _inValid

opts :: Parser Options
opts = Options <$> hideName_opt <*> output_opt <*> credP <*> correctionP

argparse :: [String] -> ParserResult Options
argparse =
    execParserPure defaultPrefs $
        info
            (opts <**> helper)
            (fullDesc <> progDesc "Generate a QR code for connecting to a network.")

{-# LANGUAGE QuasiQuotes #-}

module QRE.Data.Network (
    generateScheme,
    SSID,
    Credentials (..),
    Security (..),
) where

import Data.String.Interpolate (i)

type SSID = String

newtype Credentials = Auth (SSID, Security)
    deriving (Eq, Show)

data Security
    = WPA String
    | WEP String
    | None
    deriving (Eq)

instance Show Security where
    show None = "<NoSecurity>"
    show (WPA _) = "<WPA/WPA2 password>: *******"
    show (WEP _) = "<WEP password>: *******"

-- TODO: do we need to escape special chars in ssid & pass?
generateScheme :: Credentials -> Bool -> String
generateScheme (Auth (ssid, security)) hideName =
    case security of
        None ->
            [i|WIFI:T:NOPASS;S:#{ssid};H:#{hideName'};;"|]
        WPA pass ->
            [i|WIFI:T:WPA;S:#{ssid};P:#{pass};H:#{hideName'};;|]
        WEP pass ->
            [i|WIFI:T:WEP;S:#{ssid};P:#{pass};H:#{hideName'};;|]
  where
    hideName' = if hideName then "TRUE" else "FALSE"

-- >>> let security = Auth ("NetworkName", None) in generateScheme security False
-- "WIFI:T:NOPASS;S:NetworkName;H:FALSE;;\""

-- >>> let security = Auth ("MyNetwork", WPA "$ecretpassw0rd") in generateScheme security True
-- "WIFI:T:WPA;S:MyNetwork;P:$ecretpassw0rd;H:TRUE;;"

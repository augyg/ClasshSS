{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_ClasshSS (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "ClasshSS"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Typified Tailwind for Rapid Development"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/augyg/ClasshSS"

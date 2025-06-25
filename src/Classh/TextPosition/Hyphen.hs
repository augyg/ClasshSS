module Classh.TextPosition.Hyphen where

import Classh.Class.ShowTW
import Data.Default
--import qualified Data.Text as T

-- Hyphens
data Hyphen
  = HNone
  | HManual
  | HAuto
  deriving (Show, Eq)

instance Default Hyphen where
  def = HNone

instance ShowTW Hyphen where
  showTW = \case
    HNone   -> "hyphens-none"
    HManual -> "hyphens-manual"
    HAuto   -> "hyphens-auto"


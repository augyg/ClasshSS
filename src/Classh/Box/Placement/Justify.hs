module Classh.Box.Placement.Justify where

import Classh.Class.ShowTW

import Data.Default
import qualified Data.Text as T

instance Default Justify where
  def = J_Start

instance ShowTW Justify where
  showTW = (<>) "justify-items-" . T.toLower . T.drop 2 . T.pack . show


-- | NOTE: although I cant do Start for either, I could create a class like HasStart
-- | Also NOTE: there is no plan to make text align, rather you put it in a span and align it via this mechanism
data Justify
  = J_Start
  | J_End
  | J_Center
  | J_Stretch
  deriving Show

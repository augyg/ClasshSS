{-# LANGUAGE TemplateHaskell #-}

module Classh.Box.Sizing where

import Classh.Class.ShowTW
import Classh.Responsive.WhenTW
import Classh.Internal.TShow
import Classh.Internal.Chain
import Classh.Responsive.ZipScreens

import Classh.Box.TWSize as X

import Control.Lens (makeLenses)
import Data.Default
import qualified Data.Text as T

-- TODO: break this module down

fitToContents :: (WhenTW TWSizeOrFraction, WhenTW TWSizeOrFraction)
fitToContents = (only TWSize_Fit, only TWSize_Fit)

instance Default BoxSizing where
  def = BoxSizing def def -- TWSize_Auto TWSize_Auto

instance Default BoxSizingBand where
  def = BoxSizingBand def def def

instance Default BoxSizingConstraint where
  def = BoxSizingConstraint def def



instance ShowTW BoxSizing where
  showTW cfg = foldr (<&>) mempty
  --showTW (BoxSizing w h) = T.intercalate " " $
    [ renderWhenTW (_width cfg) ((<>) "w-" . showTW)
    , renderWhenTW (_height cfg) ((<>) "h-" . showTW)
    ]

instance ShowTW BoxSizingBand where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_widthC . _maxSize $ cfg) ((<>) "max-w-" . showTW)
    , renderWhenTW (_heightC . _maxSize $ cfg) ((<>) "max-h-" . showTW)
    , renderWhenTW (_widthC . _minSize $ cfg) ((<>) "min-w-" . showTW)
    , renderWhenTW (_heightC . _minSize $ cfg) ((<>) "min-h-" . showTW)
    , showTW $ _size cfg
    ]


data BoxSizingBand = BoxSizingBand
  { _maxSize :: BoxSizingConstraint
  , _minSize :: BoxSizingConstraint 
  , _size :: BoxSizing
  }
  deriving Show

data BoxSizing = BoxSizing
  { _width :: WhenTW TWSizeOrFraction
  , _height :: WhenTW TWSizeOrFraction
  }
  deriving Show

data BoxSizingConstraint = BoxSizingConstraint
  { _widthC :: WhenTW DimensionConstraint
  , _heightC :: WhenTW DimensionConstraint
  }
  deriving Show


type Dimensions = (WhenTW TWSizeOrFraction, WhenTW TWSizeOrFraction)



-- | TODO: this is technically wrong: there are different classes for width vs height but oh well for now
data DimensionConstraint
  = DC_0
  | DC_none
  | DC_xs
  | DC_sm
  | DC_md
  | DC_lg
  | DC_xl
  | DC_2xl
  | DC_3xl
  | DC_4xl
  | DC_5xl
  | DC_6xl
  | DC_7xl
  | DC_full
  | DC_min
  | DC_max
  | DC_fit
  | DC_prose
  | DC_screen_sm
  | DC_screen_md
  | DC_screen_lg
  | DC_screen_xl
  | DC_screen_2xl
  | DC_Custom T.Text
  deriving Show

instance Default DimensionConstraint where
  def = DC_none

instance ShowTW DimensionConstraint where
  showTW = \case
    DC_Custom t -> "[" <> t <> "]"
    x -> T.replace "_" "-" . T.drop 3 . tshow $ x

makeLenses ''BoxSizingBand
makeLenses ''BoxSizing
makeLenses ''BoxSizingConstraint

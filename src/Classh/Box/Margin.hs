{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Classh.Box.Margin where

import Classh.Internal.Chain
import Classh.Class.ShowTW
import Classh.Class.SetSides
import Classh.Responsive.WhenTW

import Classh.Box.TWSize as X 

import Control.Lens hiding ((<&>))
import Data.Default


instance Default BoxMargin where
  def = BoxMargin def def def def

instance ShowTW BoxMargin where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_marginL cfg) ((<>) "ml-" . showTW)
    , renderWhenTW (_marginR cfg) ((<>) "mr-" . showTW)
    , renderWhenTW (_marginT cfg) ((<>) "mt-" . showTW)
    , renderWhenTW (_marginB cfg) ((<>) "mb-" . showTW)
    ]

data BoxMargin = BoxMargin
  { _marginL :: WhenTW TWSize
  , _marginR :: WhenTW TWSize
  , _marginT :: WhenTW TWSize
  , _marginB :: WhenTW TWSize
  } deriving Show

makeLenses ''BoxMargin

-- | This is technically an illegal lens however if you ran 2 setters which overlap so that a /= b
-- | where a and b are the fields associated with respective separate fields, then classh' will
-- | most likely catch the error. Additionally, there is a lens way to access any field anyways
instance SetSides BoxMargin TWSize where
  l = marginL
  r = marginR
  b = marginB
  t = marginT
  x = lens _marginL $ \tw new -> tw { _marginL = new, _marginR = new }
  y =  lens _marginT $ \tw new -> tw { _marginT = new, _marginB = new }
  xy = lens _marginT $ \tw new -> tw { _marginT = new
                                     , _marginB = new
                                     , _marginL = new
                                     , _marginR = new
                                     }


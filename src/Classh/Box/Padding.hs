{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Classh.Box.Padding (module X, module Classh.Box.Padding) where


import Classh.Internal.Chain
import Classh.Class.ShowTW
import Classh.Class.SetSides
import Classh.Class.CompileStyle
import Classh.Responsive.WhenTW

import Classh.Box.TWSize as X 

import Control.Lens hiding ((<&>))
import Data.Default
import qualified Data.Text as T





instance Default BoxPadding where
  def = BoxPadding def def def def

instance ShowTW BoxPadding where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_paddingL cfg) ((<>) "pl-" . showTW)
    , renderWhenTW (_paddingR cfg) ((<>) "pr-" . showTW)
    , renderWhenTW (_paddingT cfg) ((<>) "pt-" . showTW)
    , renderWhenTW (_paddingB cfg) ((<>) "pb-" . showTW)
    ]

-- | For row func
instance CompileStyle BoxPadding where
  compileS = compilePadding

compilePadding :: BoxPadding -> Either T.Text T.Text
compilePadding cfg = pure . foldr (<&>) mempty =<< sequenceA
  [ compileWhenTW (_paddingL cfg) ((<>) "pl-" . showTW)
  , compileWhenTW (_paddingR cfg) ((<>) "pr-" . showTW)
  , compileWhenTW (_paddingT cfg) ((<>) "pt-" . showTW)
  , compileWhenTW (_paddingB cfg) ((<>) "pb-" . showTW)
  ]
  
data BoxPadding = BoxPadding
  { _paddingL :: WhenTW TWSize
  , _paddingR :: WhenTW TWSize
  , _paddingT :: WhenTW TWSize
  , _paddingB :: WhenTW TWSize
  } deriving Show

makeLenses ''BoxPadding


-- | This is technically an illegal lens however if you ran 2 setters which overlap so that a /= b
-- | where a and b are the fields associated with respective separate fields, then classh' will
-- | most likely catch the error. Additionally, there is a lens way to access any field anyways
instance SetSides BoxPadding TWSize where
  l = paddingL
  r = paddingR
  b = paddingB
  t = paddingT
  x = lens _paddingL $ \tw new -> tw { _paddingL = new, _paddingR = new }
  y = lens _paddingR $ \tw new -> tw { _paddingT = new, _paddingB = new }
  xy = lens _paddingT $ \tw new -> tw { _paddingT = new
                                      , _paddingB = new
                                      , _paddingL = new
                                      , _paddingR = new
                                      }


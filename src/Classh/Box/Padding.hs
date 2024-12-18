{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Padding
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind box's padding by a 'TWSize'
--
--  Any field named _someField has an associated lens `someField`
--  see @defaultNameTransform@ from Lens.Family.THCore
--
--  This package aims to avoid forcing the user to know lenses
--
--  Example use:
--
-- @
--  $(classh' [ padding . paddingT .~~ TWSize 8 ])
--  -- or with shorthand
--  $(classh' [ pt .~~ TWSize 8 ]) -- == "pt-8"
-- @
--------------------------------------------------------------------------------


module Classh.Box.Padding
  ( module X
  -- * Config Type 
  , BoxPadding(..)
  -- * Auto Generated Lenses
  , paddingL
  , paddingR
  , paddingB
  , paddingT
  -- * Ignore (to be moved)
  , compilePadding
  ) where


import Classh.Internal.Chain
import Classh.Class.ShowTW
import Classh.Class.SetSides
import Classh.Class.CompileStyle
import Classh.Responsive.WhenTW

import Classh.Box.TWSize as X 

import Control.Lens hiding ((<&>))
import Data.Default
import qualified Data.Text as T




-- | > == BoxPadding [] [] [] []
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

-- | Type representing '_padding' field of 'BoxConfig'.
-- | based on https://tailwindcss.com/docs/padding
data BoxPadding = BoxPadding
  { _paddingL :: WhenTW TWSize
  -- ^ see shorthand: pl
  , _paddingR :: WhenTW TWSize
  -- ^ see shorthand: pr
  , _paddingT :: WhenTW TWSize
  -- ^ see shorthand: pt
  , _paddingB :: WhenTW TWSize
  -- ^ see shorthand: pb
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


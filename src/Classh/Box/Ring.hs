{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Ring
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind ring (focus ring)
--  see https://v3.tailwindcss.com/docs/ring-width
--      https://v3.tailwindcss.com/docs/ring-color
--      https://v3.tailwindcss.com/docs/ring-opacity
--
--  Example use:
--
-- @
--  $(classh' [ ring . ringWidth .~~ Ring_4
--            , ring . ringColor .~~ Purple C500
--            , ring . ringOpacity .~~ 50
--            ])
-- @
--------------------------------------------------------------------------------

module Classh.Box.Ring where

import Classh.Class.ShowTW
import Classh.Internal.TShow
import Classh.Internal.Chain
import Classh.Responsive.WhenTW
import Classh.Color
import Control.Lens (makeLenses)
import Data.Default
import qualified Data.Text as T

-- | Ring width
-- see https://v3.tailwindcss.com/docs/ring-width
data RingWidth
  = Ring_0
  | Ring_1
  | Ring_2
  | Ring
  | Ring_4
  | Ring_8
  | Ring_Inset
  deriving Show

-- | Ring configuration
data RingConfig = RingConfig
  { _ringWidth :: WhenTW RingWidth
  , _ringColor :: WhenTW Color
  , _ringOpacity :: WhenTW Int  -- 0-100
  } deriving Show

makeLenses ''RingConfig

instance Default RingWidth where
  def = Ring_0

instance ShowTW RingWidth where
  showTW = \case
    Ring_0 -> "ring-0"
    Ring -> "ring"
    Ring_Inset -> "ring-inset"
    other -> "ring-" <> (T.drop 5 . tshow $ other)

instance Default RingConfig where
  def = RingConfig def def def

instance ShowTW RingConfig where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_ringWidth cfg) showTW
    , renderWhenTW (_ringColor cfg) ((<>) "ring-" . showTW)
    , renderWhenTW (_ringOpacity cfg) ((<>) "ring-opacity-" . tshow)
    ]

instance Semigroup RingConfig where
  (<>) a b = RingConfig
    { _ringWidth = _ringWidth a <> _ringWidth b
    , _ringColor = _ringColor a <> _ringColor b
    , _ringOpacity = _ringOpacity a <> _ringOpacity b
    }

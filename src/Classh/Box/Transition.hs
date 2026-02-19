{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Transition
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind transitions and animations
--  see https://v3.tailwindcss.com/docs/transition-property
--
--  Example use:
--
-- @
--  $(classh' [ transition . transitionProperty .~~ Transition_All
--            , transition . transitionDuration .~~ Duration_300
--            ])
-- @
--------------------------------------------------------------------------------

module Classh.Box.Transition where

import Classh.Class.ShowTW
import Classh.Internal.TShow
import Classh.Internal.Chain
import Classh.Responsive.WhenTW
import Control.Lens (makeLenses)
import Data.Default
import qualified Data.Text as T

-- | Transition property - which properties to transition
-- see https://v3.tailwindcss.com/docs/transition-property
data TransitionProperty
  = Transition_None       -- ^ transition-none
  | Transition_All        -- ^ transition-all
  | Transition            -- ^ transition (default: background-color, border-color, color, fill, stroke, opacity, box-shadow, transform)
  | Transition_Colors     -- ^ transition-colors
  | Transition_Opacity    -- ^ transition-opacity
  | Transition_Shadow     -- ^ transition-shadow
  | Transition_Transform  -- ^ transition-transform
  | Transition_Custom T.Text  -- ^ Arbitrary transition property
  deriving Show

-- | Transition duration
-- see https://v3.tailwindcss.com/docs/transition-duration
data TransitionDuration
  = Duration_0
  | Duration_75
  | Duration_100
  | Duration_150
  | Duration_200
  | Duration_300
  | Duration_500
  | Duration_700
  | Duration_1000
  | Duration_Custom T.Text  -- ^ e.g., Duration_Custom "2000" for duration-[2000ms]
  deriving Show

-- | Transition timing function (easing)
-- see https://v3.tailwindcss.com/docs/transition-timing-function
data TransitionTimingFunction
  = Ease_Linear
  | Ease_In
  | Ease_Out
  | Ease_InOut
  | Ease_Custom T.Text  -- ^ e.g., Ease_Custom "cubic-bezier(0.4,0,0.2,1)"
  deriving Show

-- | Transition delay
-- see https://v3.tailwindcss.com/docs/transition-delay
data TransitionDelay
  = Delay_0
  | Delay_75
  | Delay_100
  | Delay_150
  | Delay_200
  | Delay_300
  | Delay_500
  | Delay_700
  | Delay_1000
  | Delay_Custom T.Text  -- ^ e.g., Delay_Custom "2000" for delay-[2000ms]
  deriving Show

-- | Transition configuration
data TransitionConfig = TransitionConfig
  { _transitionProperty :: WhenTW TransitionProperty
  , _transitionDuration :: WhenTW TransitionDuration
  , _transitionTiming :: WhenTW TransitionTimingFunction
  , _transitionDelay :: WhenTW TransitionDelay
  } deriving Show

-- Template Haskell splice - must come after data type definitions
makeLenses ''TransitionConfig

-- Instances for TransitionProperty
instance Default TransitionProperty where
  def = Transition_None

instance ShowTW TransitionProperty where
  showTW = \case
    Transition -> "transition"
    Transition_Custom val -> "transition-[" <> val <> "]"
    other -> T.toLower (tshow other)

-- Instances for TransitionDuration
instance Default TransitionDuration where
  def = Duration_0

instance ShowTW TransitionDuration where
  showTW = \case
    Duration_0 -> "duration-0"
    Duration_Custom val -> "duration-[" <> val <> "ms]"
    other -> "duration-" <> (T.drop 9 . tshow $ other)

-- Instances for TransitionTimingFunction
instance Default TransitionTimingFunction where
  def = Ease_Linear

instance ShowTW TransitionTimingFunction where
  showTW = \case
    Ease_Custom val -> "ease-[" <> val <> "]"
    other -> T.toLower (tshow other)

-- Instances for TransitionDelay
instance Default TransitionDelay where
  def = Delay_0

instance ShowTW TransitionDelay where
  showTW = \case
    Delay_0 -> "delay-0"
    Delay_Custom val -> "delay-[" <> val <> "ms]"
    other -> "delay-" <> (T.drop 6 . tshow $ other)

-- Instances for TransitionConfig
instance Default TransitionConfig where
  def = TransitionConfig def def def def

instance ShowTW TransitionConfig where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_transitionProperty cfg) showTW
    , renderWhenTW (_transitionDuration cfg) showTW
    , renderWhenTW (_transitionTiming cfg) showTW
    , renderWhenTW (_transitionDelay cfg) showTW
    ]

instance Semigroup TransitionConfig where
  (<>) a b = TransitionConfig
    { _transitionProperty = _transitionProperty a <> _transitionProperty b
    , _transitionDuration = _transitionDuration a <> _transitionDuration b
    , _transitionTiming = _transitionTiming a <> _transitionTiming b
    , _transitionDelay = _transitionDelay a <> _transitionDelay b
    }

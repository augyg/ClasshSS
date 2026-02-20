{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.WithTransition
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to support CSS transitions bound to specific property values
--
--  Example use:
--
-- @
--  -- Builder pattern
--  bgColor .~^ [ ("def", purple)
--              , ("hover", lavender `withTransition` Duration_300)
--              , ("focus", indigo `withTransition` Duration_300 `withTiming` Ease_InOut)
--              ]
--
--  -- All-at-once
--  bgColor .~^ [ ("def", purple)
--              , ("hover", lavender `withTransitionAll` Duration_300 Ease_InOut Delay_0)
--              ]
-- @
--------------------------------------------------------------------------------

module Classh.WithTransition where

import Classh.Box.Transition
import Classh.Responsive.WhenTW
import Classh.Internal.Chain
import Classh.Internal.TShow (tshow)
import Data.Default
import qualified Data.Text as T

-- | Wraps a value with an optional transition configuration
-- This allows transitions to be bound to specific property values
data WithTransition a = WithTransition
  { _wtValue :: a
  , _wtTransition :: Maybe TransitionConfig
  } deriving (Show, Eq)

-- | Builder pattern: Start with duration, optionally chain timing/delay
-- Example: lavender `withTransition` Duration_300 `withTiming` Ease_InOut
withTransition :: a -> TransitionDuration -> WithTransition a
withTransition val duration = WithTransition val (Just $ TransitionConfig
  { _transitionDuration = duration
  , _transitionTiming = def
  , _transitionDelay = def
  })

-- | Builder: Add timing function to an existing WithTransition
-- Example: ... `withTiming` Ease_InOut
withTiming :: WithTransition a -> TransitionTimingFunction -> WithTransition a
withTiming (WithTransition val Nothing) timing =
  WithTransition val (Just $ TransitionConfig def timing def)
withTiming (WithTransition val (Just cfg)) timing =
  WithTransition val (Just $ cfg { _transitionTiming = timing })

-- | Builder: Add delay to an existing WithTransition
-- Example: ... `withDelay` Delay_100
withDelay :: WithTransition a -> TransitionDelay -> WithTransition a
withDelay (WithTransition val Nothing) delay =
  WithTransition val (Just $ TransitionConfig def def delay)
withDelay (WithTransition val (Just cfg)) delay =
  WithTransition val (Just $ cfg { _transitionDelay = delay })

-- | Create a value with all transition params at once
-- Example: lavender `withTransitionAll` Duration_300 Ease_InOut Delay_100
withTransitionAll :: a -> TransitionDuration -> TransitionTimingFunction -> TransitionDelay -> WithTransition a
withTransitionAll val duration timing delay = WithTransition val (Just $ TransitionConfig duration timing delay)

-- | Create a value with a pre-built transition config
-- Example: lavender `withTransitionFull` myConfig
withTransitionFull :: a -> TransitionConfig -> WithTransition a
withTransitionFull val cfg = WithTransition val (Just cfg)

-- | Create a value without a transition
noTransition :: a -> WithTransition a
noTransition val = WithTransition val Nothing

instance Default a => Default (WithTransition a) where
  def = WithTransition def Nothing

instance Functor WithTransition where
  fmap f (WithTransition val trans) = WithTransition (f val) trans

-- | Helper for rendering WhenTW values wrapped in WithTransition
-- Extracts the value, renders it with the provided function,
-- and adds transition classes if a transition config is present
renderWithTransitionTW :: WhenTW (WithTransition a)
                       -> (a -> T.Text)
                       -> TransitionProperty
                       -> T.Text
renderWithTransitionTW tws construct prop = foldr (<&>) mempty $
  fmap (\(c, WithTransition val mTransCfg) ->
    let prefix = if c == "def" then "" else (c <> ":")
        valueClass = prefix <> construct val
        transitionClasses = case mTransCfg of
          Nothing -> mempty
          Just cfg ->
            let cssProp = transitionPropertyToCSSName prop
                duration = T.drop 9 $ tshow (_transitionDuration cfg)  -- Remove "Duration_" prefix
                timing = transitionTimingToCSSName (_transitionTiming cfg)
                delay = T.drop 6 $ tshow (_transitionDelay cfg)  -- Remove "Delay_" prefix
                -- Format: [transition:property_duration_timing_delay]
                transValue = cssProp <> "_" <> duration <> "ms_" <> timing <> "_" <> delay <> "ms"
            in prefix <> "[transition:" <> transValue <> "]"
    in valueClass <&> transitionClasses
  ) tws

-- | Convert TransitionProperty to CSS property name for arbitrary value syntax
transitionPropertyToCSSName :: TransitionProperty -> T.Text
transitionPropertyToCSSName = \case
  Transition_None -> "none"
  Transition_All -> "all"
  Transition -> "all"  -- Default transition affects all properties
  Transition_Colors -> "background-color,border-color,color,fill,stroke"
  Transition_Opacity -> "opacity"
  Transition_Shadow -> "box-shadow"
  Transition_Transform -> "transform"
  Transition_Custom val -> val

-- | Convert TransitionTimingFunction to CSS timing function name
transitionTimingToCSSName :: TransitionTimingFunction -> T.Text
transitionTimingToCSSName = \case
  Ease_Linear -> "linear"
  Ease_In -> "in"
  Ease_Out -> "out"
  Ease_InOut -> "in-out"
  Ease_Custom val -> val

-- | Helper for compiling WithTransition values (with duplicate checking)
compileWithTransitionTW :: WhenTW (WithTransition a)
                        -> (a -> T.Text)
                        -> TransitionProperty
                        -> Either T.Text T.Text
compileWithTransitionTW tws construct prop = case f $ fmap fst tws of
  Left e -> Left e
  Right () -> Right $ renderWithTransitionTW tws construct prop
  where
    f [] = Right ()
    f (s:ss) =
      if elem s ss
      then Left $ s <> " exists twice"
      else f ss

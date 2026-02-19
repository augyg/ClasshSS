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
import Classh.Class.ShowTW
import Classh.Responsive.WhenTW
import Classh.Internal.Chain
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

-- | Type class for automatic wrapping in WithTransition
-- This enables backwards compatibility for existing operators
class AutoWrap a b where
  autoWrap :: a -> b

-- | Wrap plain values in WithTransition with Nothing
-- This makes existing code work: bgColor .~~ purple
instance AutoWrap a (WithTransition a) where
  autoWrap a = WithTransition a Nothing

-- | Pass through values that don't need wrapping
-- This makes existing code work: colStart .~~ 2
instance {-# OVERLAPPABLE #-} AutoWrap a a where
  autoWrap = id

-- | Pass through values that are already wrapped
instance AutoWrap (WithTransition a) (WithTransition a) where
  autoWrap = id

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
            let transProp = prefix <> showTW prop
                transDur = prefix <> showTW (_transitionDuration cfg)
                transTiming = prefix <> showTW (_transitionTiming cfg)
                transDelay = prefix <> showTW (_transitionDelay cfg)
            in transProp <&> transDur <&> transTiming <&> transDelay
    in valueClass <&> transitionClasses
  ) tws

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

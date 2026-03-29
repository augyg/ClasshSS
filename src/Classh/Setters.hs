{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Setters
--  Description :  Lens setters for BoxConfig and TextConfigTW
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Setter operators for ClasshSS configs
--
--  Any field named _someField has an associated lens `someField`
--  see @defaultNameTransform@ from Lens.Family.THCore
--
--  This package aims to avoid forcing the user to know lenses
--
--  Example uses:
--
-- @
--  $(classh boxcfg [ colSpan .~+ [("hover",1),("2xl",2)] ])
--  $(classh boxcfg [ colSpan .++ ("hover",1) ])
--  $(classh' [ colSpan .~~ 1 ])
--  $(classh' [ colSpan .|~ [1,2,3,4] ])
--  $(classh' [ colSpan .|+ [1,2,3,4] ])
-- @
--------------------------------------------------------------------------------


module Classh.Setters where

import Classh.Responsive.WhenTW as WhenTW
import Classh.Responsive.ZipScreens as ZipScreens
import Classh.WithTransition as WT
import Control.Lens hiding (only)

-- | Append a list to existing WhenTW field of a config (for non-transitionable fields)
infixr 4 .~+
(.~+) :: ASetter s t (WhenTW a) (WhenTW a) -> WhenTW a -> s -> t
someLens .~+ newVals = over someLens (++ newVals)

-- | Append a list to existing WhenTW field of a config (for non-transitionable fields)
infixr 4 .+
(.+) :: ASetter s t (WhenTW a) (WhenTW a) -> WhenTW a -> s -> t
(.+) = (.~+)

-- | Extend existing WhenTW field with single value (for non-transitionable fields)
infixr 4 .++
(.++) :: ASetter s t (WhenTW a) (WhenTW a) -> a -> s -> t
someLens .++ newVals = over someLens (++ (only newVals))

type family UnwrapType a where
  UnwrapType (WithTransition a) = a
  UnwrapType a = a

-- | Set property to a singular constant value
-- For WithTransition fields, takes unwrapped value and wraps with noTransition
-- For plain fields, takes value directly
-- User always provides unwrapped values to (.~~)
infixr 4 .~~
class SetConstant field where
  (.~~) :: ASetter s t c (WhenTW field) -> UnwrapType field -> s -> t

instance {-# OVERLAPPING #-} SetConstant (WithTransition a) where
  someLens .~~ newVals = over someLens (const $ only $ noTransition newVals)

instance {-# OVERLAPPABLE #-} (UnwrapType a ~ a) => SetConstant a where
  someLens .~~ newVals = over someLens (const $ only newVals)

-- | Zip input list with screen sizes to create a responsive property and override
-- Works for both transitionable and non-transitionable fields
infixr 4 .|~
class SetResponsive a where
  (.|~) :: ASetter s t c (WhenTW a) -> [UnwrapType a] -> s -> t

instance SetResponsive (WithTransition a) where
  someLens .|~ newVals = over someLens (const $ zipScreens $ fmap noTransition newVals)

instance {-# OVERLAPPABLE #-} (UnwrapType a ~ a) => SetResponsive a where
  someLens .|~ newVals = over someLens (const $ zipScreens newVals)

-- | Zip input list with screen sizes and add to existing property
-- Works for both transitionable and non-transitionable fields
infixr 4 .|+
class AddResponsive a where
  (.|+) :: ASetter s t (WhenTW a) (WhenTW a) -> [UnwrapType a] -> s -> t

instance AddResponsive (WithTransition a) where
  someLens .|+ newVals = over someLens (++ (zipScreens $ fmap noTransition newVals))

instance {-# OVERLAPPABLE #-} (UnwrapType a ~ a) => AddResponsive a where
  someLens .|+ newVals = over someLens (++ (zipScreens newVals))



-- | Both are functions from Classh with changed infix precedence to work with <>
infixr 7 .-
(.-) :: SetConstant field => ASetter s t c (WhenTW field) -> UnwrapType field -> s -> t
(.-) = (.~~)

infixr 7 .|<~
(.|<~) :: SetResponsive a => ASetter s t c (WhenTW a) -> [UnwrapType a] -> s -> t
(.|<~) = (.|~)

-- | Set property with explicit transition support
-- This operator allows you to specify transitions per-condition
--
-- Example:
-- @
--   bgColor .~^ [ ("def", purple)
--               , ("hover", lavender `withTransition` Duration_300)
--               ]
-- @
infixr 4 .~^
(.~^) :: ASetter s t c (WhenTW (WithTransition a)) -> [(TWCondition, WithTransition a)] -> s -> t
someLens .~^ newVals = over someLens (const newVals)


-- .:|

--   4 .:| 5 .:|

-- infixl 4 .:|
-- (.:|) :: ASetter s t (WhenTW a) (WhenTW a) -> a -> s -> t
-- someLens .:| newVal = over someLens (\initial -> initial
--                               <> (zip (drop (length initial) sizes) [newVal])
--                             )
  -- where
  --   twCondsLeft = drop (length initial) sizes
  --   maybeMore = zip twCondsLeft [newVal]

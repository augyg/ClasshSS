--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Border
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind box's border config of 'BoxConfig'  
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

import Classh.Responsive.WhenTW
import Classh.Responsive.ZipScreens
import Classh.WithTransition
import Control.Lens hiding (only)

-- | Append a list to existing WhenTW field of a config
infixr 4 .~+
(.~+) :: ASetter s t [a] [a] -> [a] -> s -> t
someLens .~+ newVals = over someLens (++ newVals)

-- | Append a list to existing WhenTW field of a config
infixr 4 .+
(.+) :: ASetter s t [a] [a] -> [a] -> s -> t
(.+) = (.~+)

-- | Extend existing WhenTW field of a config with new value at end of input list
infixr 4 .++
(.++) :: AutoWrap a b => ASetter s t (WhenTW b) (WhenTW b) -> a -> s -> t
someLens .++ newVals = over someLens (++ (only $ autoWrap newVals))

-- | Set property to a singular constant value
-- Uses AutoWrap to automatically wrap values in WithTransition when needed
infixr 4 .~~
(.~~) :: AutoWrap a b => ASetter s t c (WhenTW b) -> a -> s -> t
someLens .~~ newVals = over someLens (const $ only $ autoWrap newVals)

-- | Zip input list with screen sizes to create a responsive property and override
-- Uses AutoWrap to automatically wrap values in WithTransition when needed
infixr 4 .|~
(.|~) :: AutoWrap a b => ASetter s t c (WhenTW b) -> [a] -> s -> t
someLens .|~ newVals = over someLens (const $ zipScreens $ fmap autoWrap newVals)

-- | Zip input list with screen sizes to create a responsive property and add to input property
-- Uses AutoWrap to automatically wrap values in WithTransition when needed
infixr 4 .|+
(.|+) :: AutoWrap a b => ASetter s t (WhenTW b) (WhenTW b) -> [a] -> s -> t
someLens .|+ newVals = over someLens (++ (zipScreens $ fmap autoWrap newVals))



-- | Both are functions from Classh with changed infix precedence to work with <>
-- Uses AutoWrap to automatically wrap values in WithTransition when needed
infixr 7 .-
(.-) :: AutoWrap a b => ASetter s t c (WhenTW b) -> a -> s -> t
someLens .- newVals = over someLens (const $ only $ autoWrap newVals)

infixr 7 .|<~
(.|<~) :: AutoWrap a b => ASetter s t c (WhenTW b) -> [a] -> s -> t
someLens .|<~ newVals = over someLens (const $ zipScreens $ fmap autoWrap newVals)

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

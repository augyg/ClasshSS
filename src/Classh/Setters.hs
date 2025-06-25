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
(.++) :: ASetter s t (WhenTW a) (WhenTW a) -> a -> s -> t
someLens .++ newVals = over someLens (++ (only newVals))

-- | Set property to a singular constant value
infixr 4 .~~
(.~~) :: ASetter s t b (WhenTW a) -> a -> s -> t
someLens .~~ newVals = over someLens (const $ only newVals)

-- | Zip input list with screen sizes to create a responsive property and override
infixr 4 .|~
(.|~) :: ASetter s t b (WhenTW a) -> [a] -> s -> t
someLens .|~ newVals = over someLens (const $ zipScreens newVals)

-- | Zip input list with screen sizes to create a responsive property and add to input property
infixr 4 .|+
(.|+) :: ASetter s t (WhenTW a) (WhenTW a) -> [a] -> s -> t
someLens .|+ newVals = over someLens (++ (zipScreens newVals))



-- | Both are functions from Classh with changed infix precedence to work with <> 
infixr 7 .- 
(.-) :: ASetter s t b (WhenTW a) -> a -> s -> t
someLens .- newVals = over someLens (const $ only newVals)

infixr 7 .|<~
(.|<~) :: ASetter s t b (WhenTW a) -> [a] -> s -> t
someLens .|<~ newVals = over someLens (const $ zipScreens newVals)


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

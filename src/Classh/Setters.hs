module Classh.Setters where

import Classh.Responsive.WhenTW
import Classh.Responsive.ZipScreens
import Control.Lens hiding (only)

infixr 4 .~+
(.~+) :: ASetter s t [a] [a] -> [a] -> s -> t
lens .~+ newVals = over lens (++ newVals)

infixr 4 .+
(.+) :: ASetter s t [a] [a] -> [a] -> s -> t
(.+) = (.~+)

--only_ = (.++)
infixr 4 .++
(.++) :: ASetter s t (WhenTW a) (WhenTW a) -> a -> s -> t
lens .++ newVals = over lens (++ (only newVals))

-- o = (.~~)
-- only = o
infixr 4 .~~
(.~~) :: ASetter s t b (WhenTW a) -> a -> s -> t
lens .~~ newVals = over lens (const $ only newVals)

infixr 4 .|~
(.|~) :: ASetter s t b (WhenTW a) -> [a] -> s -> t
lens .|~ newVals = over lens (const $ zipScreens newVals)

infixr 4 .|+
(.|+) :: ASetter s t (WhenTW a) (WhenTW a) -> [a] -> s -> t
lens .|+ newVals = over lens (++ (zipScreens newVals))

-- .:|

--   4 .:| 5 .:|

-- infixl 4 .:|
-- (.:|) :: ASetter s t (WhenTW a) (WhenTW a) -> a -> s -> t
-- lens .:| newVal = over lens (\initial -> initial
--                               <> (zip (drop (length initial) sizes) [newVal])
--                             )
  -- where
  --   twCondsLeft = drop (length initial) sizes
  --   maybeMore = zip twCondsLeft [newVal]

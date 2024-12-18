--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Responsive.WhenTW 
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Uses 'WhenTW' to represent a responsive set of options
--
--  Example use:
--
-- @
--  $(classh' [ colSpan .~ [("def",1), ("sm", 2), ("md",3), ("lg",4) ] ])
--  -- shortened:
--  $(classh' [ colSpan .|~ [1,2,3,4]])
--  -- because (.|~) and (.|+) produce and set a WhenTW based on 'zipScreens':
--  $(classh' [ colSpan .~ zipScreens [1,2,3,4]])
-- @
--------------------------------------------------------------------------------

module Classh.Responsive.ZipScreens where

import Classh.Responsive.WhenTW as X 
import Classh.Class.ShowTW 
import Data.Default
import qualified Data.Text as T

-- | Eg
--   > $(classh' [colSpan .~ only 1]) == $(classh' [colSpan .~~ 1])
only :: a -> WhenTW a
only p = ("def", p):[]

-- | Synonym to only
always :: a -> WhenTW a 
always = only 

-- | Like only except the singular value is the default one.
-- | This is most likely useful to provide an interface like earlier versions of Classh
-- | which set everything to defaults, unless overrided to another value
onlyDef :: (Default a, ShowTW a) => WhenTW a
onlyDef = ("def", def):[]

-- | String based rep of screen sizes. This could definitely be improved. 
sizes :: [T.Text]
sizes = ["def", "sm", "md", "lg", "xl", "2xl"]

-- | String based rep of screen sizes. This could definitely be improved. 
sm, md, lg, xl, _2xl :: T.Text 
sm = "sm"
md = "md"
lg = "lg"
xl = "xl"
_2xl = "2xl"

-- | Zip a list of some Classh construct with the screen sizes in order from mobile to 2xl 
zipScreens :: [a] -> WhenTW a
zipScreens xs = zip sizes xs

-- | Zip a list of some Classh construct with the screen sizes in order from mobile to 2xl 
zipS :: [a] -> WhenTW a
zipS = zipScreens

-- | Zip a list of some Classh construct with the screen sizes in order from mobile to 2xl, with
-- | some arbitrary function
zipScreensWith :: (a -> b) -> [a] -> WhenTW b
zipScreensWith f xs = zip sizes (f <$> xs)


-- data TWCondition_V2
--   = S_Mobile
--   | S_SM
--   | S_MD
--   | S_LG
--   | S_XL
--   | S_2XL
--   | On T.Text
--   deriving (Eq, Read, Ord, Show, Enum, Bounded)

-- zipScreens_v2 :: [a] -> WhenTW a
-- zipScreens_v2 xs =
--   let
--     sizes :: TWCondition_V2
--     sizes = take 6 [minBound .. maxBound] -- ["def", "sm", "md", "lg", "xl", "2xl"]
--   in zip sizes xs

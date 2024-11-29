module Classh.Responsive.ZipScreens where

import Classh.Responsive.WhenTW as X 
import Classh.Class.ShowTW 
import Data.Default
import qualified Data.Text as T

only :: a -> WhenTW a
only p = ("def", p):[]

-- | Synonym to only
always :: a -> WhenTW a 
always = only 

onlyDef :: (Default a, ShowTW a) => WhenTW a
onlyDef = ("def", def):[]

sizes :: [T.Text]
sizes = ["def", "sm", "md", "lg", "xl", "2xl"]

sm, md, lg, xl, _2xl :: T.Text 
sm = "sm"
md = "md"
lg = "lg"
xl = "xl"
_2xl = "2xl"

zipScreens :: [a] -> WhenTW a
zipScreens xs = zip sizes xs

zipS :: [a] -> WhenTW a
zipS = zipScreens

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

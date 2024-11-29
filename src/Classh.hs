{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Classh (module X, module Classh) where

-- import Classh.Border
-- import Classh.ShowTW
-- import Classh.WhenTW
import Classh.Box as X
import Classh.Text as X
import Classh.Grid as X
--import Classh.Cursor as X
--import Classh.Color as X
--import Classh.Responsive.WhenTW as X
import Classh.Responsive.ZipScreens as X
import Classh.Setters as X
import Classh.Shorthand as X
import Classh.Class.ShowTW as X
import Classh.Class.CompileStyle as X
import Classh.Class.HasCSSSize as X
import Classh.Class.HasCustom as X
import Classh.Class.IsCSS as X
import Classh.Class.SetSides as X
import Classh.Internal.Chain as X
import Classh.Internal.TShow as X
import Classh.Internal.Utils as X

import Data.Default
import Language.Haskell.TH
import qualified Data.Text as T


{-
GOALS
-> Cut down the surface area of what is needed from CSS
-> Make an easy to integrate system through this limited CSS
-> Eliminate messing with typos and repeats
-> Encourage non-naive responsive code
-> Have more organized code for when an attribute is highly responsive
   Non eg. "py-2 ......... sm:py-4 ............... lg:pt-9"
-> Helper functions with a definite limited scope for the goal of self-documenting / easy to visualize DomBuilders

-}




{-
TODOS
-------------------------------------
->BoxConfig by Media Query
f :: [BySize] -> BoxConfig
         ^INTERNAL^
  [BoxConfig -> BoxConfig]

md = [ w = 4 , h = 9 , color = marigold ]
-------------------------------------
-> VelvetBox: a config type to describe a flex box like component
   -> Min-width
   -> Min-height  -- w+h mins so that content

Items }} Box }} GridLayout

Items: text, images, image content, radiobutton<--with-->label
Box: components, which contain items
GridLayout: Highly variably-sized element with tons of purposeful white-space 

widthTotal <- element widthTotal $ do 
  mkItem (w = 9)
  mkItem (w = 9)

|__<-- item -->____<--  item  -->__|  // Derive the min width to propogate upwards

mkItem :: WriterT WidthTotal

tell (w = 9)

-}



classh :: CompileStyle s => s -> [(s -> s)] -> Q Exp
classh base muts = case compileS $ foldl (\acc f -> f acc) base muts of
  Left e -> fail $ T.unpack e
  Right styleString -> [| styleString |]

classh' :: (Default s, CompileStyle s) => [(s -> s)] -> Q Exp
classh' muts = case compileS $ foldl (\acc f -> f acc) def muts of
  Left e -> fail $ T.unpack e
  Right styleString -> [| styleString |]

-- | Doesn't use TemplateHaskell, this is meant for making lib functions since we need args
-- | from outside the would-be TH context
classhUnsafe :: (Default a, ShowTW a) => [a -> a] -> T.Text
classhUnsafe muts = showTW $ def `applyFs` muts

-- | Util for dealing with existing, complex and masive CSS strings but adding on in Classh DSL
boxCSS :: BoxConfig -> T.Text
boxCSS = showTW 


alsoF :: T.Text -> [BoxConfig -> BoxConfig] -> T.Text
alsoF s cfgMuts = s <> boxCSS (def `applyFs` cfgMuts) 

also :: T.Text -> BoxConfig -> T.Text
also s cfg = s <> boxCSS cfg

applyFs :: a -> [a -> a] -> a
applyFs b fs = foldl (\acc f -> f acc) b fs



-- END OF MODULE 




-- | A future research item for this library is how to work with accessibility
  -- do we need prerender ?
  -- is this gonna be reading in some config?
  -- is this a native thing?
  -- aria?
  -- will this mean scaling from same base accessibility based value?
  -- what features does this affect directly? indirectly?


-- | TODO:
  -- use template haskell to autogenerate types from a CSS file eg if classes blah and blah2 exist
  -- data UserCSS = Blah | Blah2




-------------------thought bubble ----------------------------------------------------------


-- module MyTWConfig where

-- def = TW.def & text_color .~~ aceBlue

-- then

-- module MyModule

-- import MyTWConfig
-- f = elTW def blank

-- ---

-- we could also have a defDerivative which functions just like V1 tailwindlib like so:
--   BoxConfig def:[]

--   where def here should be ("def", def)

--   and note we can make TWstring which could have an instance of IsString so that it can be written as a string literal

--   which helps cuz then TWString can have a default of "def"


-- Also for conceptual understanding:

--   fmap (,X) (enum screenSizes)  == ("def", X)
--   fmap (,X) (enum allConditions) == ("def", X)
--   fmap (,X) (enum allEvents) : ("def",X) : [] == ("def", X)
--   fmap (,def) (enum allEvents) == []


------------------------------------------------------------------------------------------------------
-- TODO:

-- CSS and Tailwind semigroups for folding into one CSSConfig OR TailwindClassConfig
-- where both render to a string

  -- Each config is only allowed to be set once -> alerting the dev if they asked for something like "red" and "blue"
  -- for the background color or similar illogical configurations

  -- Would be extra ideal to have this made clear at compile time


-- def
--   & leftPadding .~ 3
--   & styling .~ (def
--                 &  bg_color .~ Color_Custom (Hex "")
--                )


-- data BoxConfig = BoxConfig
--   { leftPadding :: T.Text
--   , bottomPadding :: T.Text
--   }



-- | Idea: elDynTW where we can update using the TW lib based on events and prev state

-- | Idea: for align and justify -> we have them as one setting of the datatype Position = TR -- TopRight | BL -- Bottom Left

-- | THIS DOESNT WORK: Grid causes other issues/effects - maybe we can negate this through defaults (eg. two styled texts begin to stack
-- | instead of go beside each other


-- data RowDivisor = RD1 | RD2 | RD3 | RD4 | RD6 | RD12


-- data Dimension
--   = DNum Float
--   | DFrac Int DivInt
--   | Full
--   | Screen
--   | Min
--   | Max
--   | Fit
--   | Dim_Custom T.Text

-- -- | todo: can we merge with gridCol Col1-12?
-- data IntGrid
--   = IG1
--   | IG2
--   | IG3
--   | IG4
--   | IG5
--   | IG6
--   | IG7
--   | IG8
--   | IG9
--   | IG10
--   | IG11
--   | IG12


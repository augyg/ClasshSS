{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Classh where


--import Common.Types (tshow)
import Data.Char (toLower, isUpper)
--import Reflex.Dom.Core

import Language.Haskell.TH
import Control.Lens (makeLenses, Lens',lens, ASetter, over)
import Data.Default
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


tshow :: Show a => a -> T.Text
tshow = T.pack . show
-- | A future research item for this library is how to work with accessibility
  -- do we need prerender ?
  -- is this gonna be reading in some config?
  -- is this a native thing?
  -- aria?
  -- will this mean scaling from same base accessibility based value?
  -- what features does this affect directly? indirectly?


toKebabCase :: String -> String
toKebabCase [] = []
toKebabCase (x:xs) = toLower x : go xs
  where
    go [] = []
    go (y:ys)
      | isUpper y = '-' : toLower y : go ys
      | otherwise = y : go ys



--instance ShowTW (WhenTW a)

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


















-- | Eg tic tac toe, except we use to describe position of element
data Matrix33
  = UpL
  | UpM
  | UpR
  | MidL
  | MidM
  | MidR
  | DownL
  | DownM
  | DownR



-- HELPFUL CONSTANTS

topLeft, middleLeft, bottomLeft, topCenter, centered, bottomCenter, topRight, middleRight, bottomRight
  :: (Justify, Align)
topLeft = (J_Start, A_Start)
middleLeft = (J_Start, A_Center)
bottomLeft = (J_Start, A_End)

topCenter = (J_Center, A_Start)
centered = (J_Center, A_Center)
bottomCenter = (J_Center, A_End)

topRight = (J_End, A_Start)
middleRight = (J_End, A_Center)
bottomRight = (J_End, A_End)


fitToContents :: (WhenTW TWSizeOrFraction, WhenTW TWSizeOrFraction)
fitToContents = (only TWSize_Fit, only TWSize_Fit)

centeredOnly :: WhenTW (Justify, Align)
centeredOnly = only centered

--------------------
defaultClasses :: T.Text
defaultClasses = "" -- "grid"






-- :: [ [ 
-- [ [w .~ 1, h .~ 1] , [ w .~ 5, h .~ 4 ] ] 
--   classh' [w .~: "md" 5, h .|~ [1,4] ]



data RowDivisor = RD1 | RD2 | RD3 | RD4 | RD6 | RD12


applyFs :: a -> [a -> a] -> a
applyFs b fs = foldl (\acc f -> f acc) b fs

type CompiledS = T.Text
-- newtype CompiledS a = CompiledS T.Text
-- newtype CompiledS = CompiledS T.Text
-- type CompiledS a = T.Text
-- rowC :: DomBuilder t m => CompiledS BoxPadding -> m a -> m a
-- rowC padding = elClass "div" ( "" <&> padding )


------ Helpers
infixr 0 <&>
(<&>) :: T.Text -> T.Text -> T.Text
a <&> b
  | a == "" = b
  | b == "" = a
  | otherwise = a <> " " <> b

sm, md, lg, xl, _2xl :: T.Text 
sm = "sm"
md = "md"
lg = "lg"
xl = "xl"
_2xl = "2xl"

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

sizes :: [T.Text]
sizes = ["def", "sm", "md", "lg", "xl", "2xl"]

zipScreens :: [a] -> WhenTW a
zipScreens xs = zip sizes xs

zipS :: [a] -> WhenTW a
zipS = zipScreens

zipScreensWith :: (a -> b) -> [a] -> WhenTW b
zipScreensWith f xs = zip sizes (f <$> xs)

-- (:.) :: a -> [a]
-- (:.) a = [a]




-- | it would be awesome to have a map like function that could be used like config for text size
-- | mappy = Map.fromList [(Heading, _), (Maintext, _) ... ]
-- |
-- | and effectively this is a model of the branding which will allow for consistency
  -- | AND!!! because the new model, models responsive design we can make scaling and brand design expressable as a Map

autoScalePx :: Int -> WhenTW TWSize
autoScalePx pixels =
  [ ("def", scalePx pixels 1.0)
  , ("sm", scalePx pixels smFactor)
  , ("md", scalePx pixels mdFactor)
  , ("lg", scalePx pixels lgFactor)
  , ("xl", scalePx pixels xlFactor)
  , ("2xl", scalePx pixels s_2xlFactor)
  ]
  where
    mobileEstimatedWidth_px = 390
    -- Screen sizes per: https://tailwindcss.com/docs/responsive-design
    smFactor = 640 / mobileEstimatedWidth_px
    mdFactor = 768 / mobileEstimatedWidth_px
    lgFactor = 1024 / mobileEstimatedWidth_px
    xlFactor = 1280 / mobileEstimatedWidth_px
    s_2xlFactor = 1536 / mobileEstimatedWidth_px
    scalePx :: Int -> Float -> TWSize
    scalePx p factor = pix . round $ fromIntegral p * factor


-- | autoScalePx = autoScalePx' 1.0
autoScalePx' :: Float -> Int -> WhenTW TWSize
autoScalePx' userOpt pixels =
  [ ("def", scalePx pixels 1.0)
  , ("sm", scalePx pixels smFactor)
  , ("md", scalePx pixels mdFactor)
  , ("lg", scalePx pixels lgFactor)
  , ("xl", scalePx pixels xlFactor)
  , ("2xl", scalePx pixels s_2xlFactor)
  ]
  where
    mobileEstimatedWidth_px = 390
    -- Screen sizes per: https://tailwindcss.com/docs/responsive-design
    smFactor = userOpt * 640 / mobileEstimatedWidth_px
    mdFactor = userOpt * 768 / mobileEstimatedWidth_px
    lgFactor = userOpt * 1024 / mobileEstimatedWidth_px
    xlFactor = userOpt * 1280 / mobileEstimatedWidth_px
    s_2xlFactor = userOpt * 1536 / mobileEstimatedWidth_px
    scalePx :: Int -> Float -> TWSize
    scalePx p factor = pix . round $ fromIntegral p * factor



-- | GOAL:
-- autoScalePx :: Int -> WhenTW TWSize
-- autoScalePx px =
--   [ (S_Mobl, px)
--   , (S_SM, scalePx px smFactor)
--   , (S_MD, scalePx px mdFactor)
--   , (S_LG, scalePx px lgFactor)
--   , (S_XL, scalePx px 1.4)
--   , (S_2XL, scalePx px 1.5)
--   ]
--   where
--     mobileEstimatedWidth_px = 390
--     -- Screen sizes per: https://tailwindcss.com/docs/responsive-design
--     smFactor = 640 / mobileEstimatedWidth_px
--     mdFactor = 768 / mobileEstimatedWidth_px
--     lgFactor = 1024 / mobileEstimatedWidth_px
--     xlFactor = 1280 / mobileEstimatedWidth_px
--     s_2xlFactor = 1536 / mobileEstimatedWidth_px
--
--     scalePx :: Int -> Float -> Int
--     scalePx p factor = round $ fromIntegral p * factor

-- autoScale :: TWSize -> WhenTW TWSize
-- autoScale mobile = [(S_Mobl, x), (S_SM, scaleSize x 1.1), (S_MD, scaleSize x 1.2), (S_LG, scaleSize x 1.3), (S_XL, scaleSize x 1.4), (S_2XL, scaleSize x 1.5)]
--   where
--     scaleSize :: TWSize -> Float -> TWSize
--     scaleSize (TWSize_Custom (Pixel p)) factor = TWSize_Custom (Pixel (round $ fromIntegral p * factor))
--     scaleSize (TWSize n) factor = TWSize (n * factor)
--     -- Add more cases for other CSSSize constructors if needed
--     scaleSize size _ = size  -- Fallback for non-scalable sizes or units


only :: a -> WhenTW a
only p = ("def", p):[]

onlyDef :: (Default a, ShowTW a) => WhenTW a
onlyDef = ("def", def):[]

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



renderWhenTW :: WhenTW a -> (a -> T.Text) -> T.Text
renderWhenTW tws construct = foldr (<&>) mempty $
  fmap (\(c,p) -> (if c == "def" then "" else (c <> ":")) <> construct p) $ tws

compileWhenTW :: WhenTW a -> (a -> T.Text) -> Either T.Text T.Text
compileWhenTW tws construct = case f $ fmap fst tws of
  Right () -> Right $ renderWhenTW tws construct
  Left e -> Left e
  where
    f [] = Right ()
    f (s:ss) =
      if elem s ss
      then Left $ s <> " exists twice"
      else f ss



--foldr ((<&>) . (\(c,p) -> (if c == "def" then "" else (c <> ":")) <> construct p)) mempty



-- | Checkers on the lib user's input to ensure no *unintended repetition
joinUnique :: WhenTW a -> WhenTW a -> WhenTW a
joinUnique = undefined

addUnique :: (TWCondition, a) -> WhenTW a -> WhenTW a
addUnique = undefined

-- checkStyle ::  -> Q Exp
-- checkStyle styleString =
--   let
--     xs = T.split styleString
--     f [] = False
--     f (s:ss) = elem s ss || f ss
--   in
--     if not $ f xs
--     then [| styleString |]
--     else fail
---------------

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


instance CompileStyle BoxConfig where
  compileS cfg = do
    pure . foldr (<&>) mempty =<< sequenceA
      [ compilePos (_position cfg)
      , compileWhenTW (_colStart cfg) ((<>) "col-start-" . tshow)
      , compileWhenTW (_colSpan cfg) ((<>) "col-span-" . tshow)
      , compileBorder (_border cfg)
      , compileSizingBand (_sizingBand cfg)
      , compilePadding (_padding cfg)
      , compileMargin (_margin cfg)
      , compileWhenTW (_bgColor cfg) ((<>) "bg-" . showTW)
      , compileWhenTW (_bgOpacity cfg) ((<>) "bg-opacity-" . tshow)
      , Right $ _box_custom cfg
      ]
      where
        compileBorder cfg = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWhenTW (_bStyle cfg) ((<>) "border-" . showTW)
          , compileBorderRadius (_radius cfg)
          , compileBorderWidth (_bWidth cfg)
          , compileBorderColor (_bColor cfg)
          ]

        compileBorderRadius cfg = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWhenTW (_borderRadius_tr cfg) ((<>) "rounded-tr" . showTW)
          , compileWhenTW (_borderRadius_tl cfg) ((<>) "rounded-tl" . showTW)
          , compileWhenTW (_borderRadius_br cfg) ((<>) "rounded-br" . showTW)
          , compileWhenTW (_borderRadius_bl cfg) ((<>) "rounded-bl" . showTW)
          ]

        compileBorderWidth cfg = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWhenTW (_borderWidth_l cfg) ((<>) "border-l" . showTW)
          , compileWhenTW (_borderWidth_r cfg) ((<>) "border-r" . showTW)
          , compileWhenTW (_borderWidth_t cfg) ((<>) "border-t" . showTW)
          , compileWhenTW (_borderWidth_b cfg) ((<>) "border-b" . showTW)
          ]

        compileBorderColor cfg = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWhenTW (_borderColor_l cfg) ((<>) "border-l-" . showTW)
          , compileWhenTW (_borderColor_r cfg) ((<>) "border-r-" . showTW)
          , compileWhenTW (_borderColor_t cfg) ((<>) "border-t-" . showTW)
          , compileWhenTW (_borderColor_b cfg) ((<>) "border-b-" . showTW)
          ]

        compileSizingBand cfg = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWhenTW (_widthC . _maxSize $ cfg) ((<>) "max-w-" . showTW)
          , compileWhenTW (_heightC . _maxSize $ cfg) ((<>) "max-h-" . showTW)
          , compileWhenTW (_widthC . _minSize $ cfg) ((<>) "min-w-" . showTW)
          , compileWhenTW (_heightC . _minSize $ cfg) ((<>) "min-h-" . showTW)
          , compileWhenTW (_width . _size $ cfg) ((<>) "w-" . showTW)
          , compileWhenTW (_height . _size $ cfg) ((<>) "h-" . showTW)
          ]


        compileMargin cfg = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWhenTW (_marginL cfg) ((<>) "ml-" . showTW)
          , compileWhenTW (_marginR cfg) ((<>) "mr-" . showTW)
          , compileWhenTW (_marginT cfg) ((<>) "mt-" . showTW)
          , compileWhenTW (_marginB cfg) ((<>) "mb-" . showTW)
          ]

        compilePos posCfg = case f $ fmap fst posCfg of
          Left e -> Left e
          Right () -> Right $ foldr (<&>) mempty $ fmap
            (\(c,(jus,align)) ->
               let pre = if c == "def" then "" else (c <> ":")
               in
                 pre <> "grid" <&> pre <> (showTW jus) <&> pre <> (showTW align)
            ) $ posCfg
          where
            f [] = Right ()
            f (s:ss) =
              if elem s ss
              then Left $ s <> " exists twice"
              else f ss

-- | For row func
instance CompileStyle BoxPadding where
  compileS = compilePadding

compilePadding :: BoxPadding -> Either T.Text T.Text
compilePadding cfg = pure . foldr (<&>) mempty =<< sequenceA
  [ compileWhenTW (_paddingL cfg) ((<>) "pl-" . showTW)
  , compileWhenTW (_paddingR cfg) ((<>) "pr-" . showTW)
  , compileWhenTW (_paddingT cfg) ((<>) "pt-" . showTW)
  , compileWhenTW (_paddingB cfg) ((<>) "pb-" . showTW)
  ]



instance CompileStyle TextConfigTW where
  compileS cfg = pure . foldr (<&>) mempty =<< sequenceA
    [ compileWhenTW (_text_size cfg) showTW
    , compileWhenTW (_text_weight cfg) showTW
    , compileWhenTW (_text_font cfg) showTW
    , compileWhenTW (_text_style cfg) showTW
    , compileWhenTW (_text_cursor cfg) showTW
    , compileWhenTW (_text_color cfg) ((<>) "text-" . showTW)
    , compileDecoration (_text_decoration cfg)
    , Right $ _text_custom cfg
    ]
    where
      compileDecoration cfg = pure . foldr (<&>) mempty =<< sequenceA
        [ compileWhenTW (_textDec_line cfg) showTW
        , compileWhenTW (_textDec_color cfg) ((<>) "decoration-" . showTW)
        , compileWhenTW (_textDec_style cfg) showTW
        , compileWhenTW (_textDec_thickness cfg) showTW
        , compileWhenTW (_textDec_offset cfg) showTW
        ]






class HasCSSSize tw where
  pix :: Int -> tw --Lens' tw Int
  pct :: Int -> tw
  vh :: Int -> tw
  vw :: Int -> tw
  rem :: Float -> tw

instance HasCSSSize TWSize where
  pix = TWSize_Custom . Pixel
  pct = TWSize_Custom . Percent
  vh = TWSize_Custom . Vh
  vw = TWSize_Custom . Vw
  rem = TWSize_Custom . Rem

instance HasCSSSize TWSizeOrFraction where
  pix = TWSize' . TWSize_Custom . Pixel --px
  pct = TWSize' . TWSize_Custom . Percent --pct
  vh = TWSize' . TWSize_Custom . Vh --vh
  vw = TWSize' . TWSize_Custom . Vw--vw
  rem = TWSize' . TWSize_Custom . Rem -- Classh.rem

class HasCustom tw where
  custom :: Lens' tw T.Text

class CompileStyle tw where
  compileS :: tw -> Either T.Text T.Text


class IsCSS css where
  renderCSS :: css -> T.Text

class ShowTW tw where
  showTW :: tw -> T.Text

class SetSides tw a where
  x :: Lens' tw (WhenTW a)
  y :: Lens' tw (WhenTW a)
  xy :: Lens' tw (WhenTW a)
  allS :: Lens' tw (WhenTW a)
  allS = xy
  l :: Lens' tw (WhenTW a)
  r :: Lens' tw (WhenTW a)
  b :: Lens' tw (WhenTW a)
  t :: Lens' tw (WhenTW a)

type TWCondition = T.Text
type WhenTW a = [(TWCondition, a)]
newtype WhenTW' a = WhenTW' { unWhenTW :: [(TWCondition, a)] }

-- Useful for when you want config relative to another one.
-- eg. width2 = width1 + 1px
instance Functor WhenTW' where
  fmap f whenTW = WhenTW' $ fmap (\(c,a) -> (c, f a)) $ unWhenTW whenTW


data BoxConfig = BoxConfig
  { _colStart :: WhenTW Int
  , _colSpan :: WhenTW Int
  , _bgColor :: WhenTW Color
  , _bgOpacity :: WhenTW Int -- 1 5 10 .. 100 -- def == 519
  , _padding :: BoxPadding
  , _margin :: BoxMargin
  , _sizingBand :: BoxSizingBand
  , _border :: BorderConfig -- { rounded, thickness, etc .. }
  , _position :: WhenTW (Justify, Align)
  --, _text_align :: Align ... or should we set == position.align
  , _box_custom :: T.Text
  }
  deriving Show

data BoxPadding = BoxPadding
  { _paddingL :: WhenTW TWSize
  , _paddingR :: WhenTW TWSize
  , _paddingT :: WhenTW TWSize
  , _paddingB :: WhenTW TWSize
  } deriving Show

-- TODO: lenses for y,x and all
data BoxMargin = BoxMargin
  { _marginL :: WhenTW TWSize
  , _marginR :: WhenTW TWSize
  , _marginT :: WhenTW TWSize
  , _marginB :: WhenTW TWSize
  } deriving Show

data BoxSizingBand = BoxSizingBand
  { _maxSize :: BoxSizingConstraint --(DimensionConstraint, DimensionConstraint)
  , _minSize :: BoxSizingConstraint -- (DimensionConstraint, DimensionConstraint)
  , _size :: BoxSizing
  }
  deriving Show

data BoxSizing = BoxSizing
  { _width :: WhenTW TWSizeOrFraction
  , _height :: WhenTW TWSizeOrFraction
  }
  deriving Show

data BoxSizingConstraint = BoxSizingConstraint
  { _widthC :: WhenTW DimensionConstraint
  , _heightC :: WhenTW DimensionConstraint
  }
  deriving Show

data BorderConfig = BorderConfig
  { _bStyle :: WhenTW BorderStyle
  , _bColor :: BorderColorSides
  , _bWidth :: BorderWidthSides
  , _radius :: BorderRadiusCorners
  } deriving Show

data BorderColorSides = BorderColorSides
  { _borderColor_l :: WhenTW Color
  , _borderColor_r :: WhenTW Color
  , _borderColor_t :: WhenTW Color
  , _borderColor_b :: WhenTW Color
  } deriving Show

data BorderRadiusCorners = BorderRadiusCorners
  { _borderRadius_tr :: WhenTW BorderRadius'
  , _borderRadius_tl :: WhenTW BorderRadius'
  , _borderRadius_br :: WhenTW BorderRadius'
  , _borderRadius_bl :: WhenTW BorderRadius'
  } deriving Show

data BorderWidthSides = BorderWidthSides
  { _borderWidth_l :: WhenTW BorderWidth
  , _borderWidth_r :: WhenTW BorderWidth
  , _borderWidth_t :: WhenTW BorderWidth
  , _borderWidth_b :: WhenTW BorderWidth
  } deriving Show


data TextConfigTW = TextConfigTW
  { _text_size :: WhenTW TextSize
  , _text_weight :: WhenTW TextWeight
  , _text_font :: WhenTW Font -- many options -- EG. Sarabun -> font-[Sarabun]
  , _text_color :: WhenTW Color
  , _text_decoration :: TextDecorationTW
  , _text_style :: WhenTW FontStyle
  , _text_cursor :: WhenTW CursorStyle
  , _text_custom :: T.Text
  }

data TextDecorationTW = TextDecorationTW
  { _textDec_line :: WhenTW TextDecLineType
  , _textDec_color :: WhenTW Color
  , _textDec_style :: WhenTW TextDecStyle
  , _textDec_thickness :: WhenTW TextDecThickness -- TODO: is this a standard system: Thickness?
  , _textDec_offset :: WhenTW TextDecOffset -- and is this a system? (the numbers chosen 0,1,2,4,8)
  }


------------  Defaults of Records

instance Default BoxConfig where
  def = BoxConfig def def def def def def def def def ""

instance Default BoxPadding where
  def = BoxPadding def def def def -- (TWSize 0) (TWSize 0) (TWSize 0) (TWSize 0)

instance Default BoxMargin where
  def = BoxMargin def def def def -- (TWSize 0) (TWSize 0) (TWSize 0) (TWSize 0)

instance Default TextConfigTW where
  def = TextConfigTW def def def def def def def ""

instance Default TextDecorationTW where
  def = TextDecorationTW def def def def def

instance Default BorderConfig where
  def = BorderConfig def def def def

instance Default BorderRadiusCorners where
  def = BorderRadiusCorners def def def def

instance Default BorderWidthSides where
  def = BorderWidthSides def def def def

instance Default BorderColorSides where
  def = BorderColorSides def def def def

instance Default BoxSizing where
  def = BoxSizing def def -- TWSize_Auto TWSize_Auto

instance Default BoxSizingBand where
  def = BoxSizingBand def def def

instance Default BoxSizingConstraint where
  def = BoxSizingConstraint def def -- DC_none DC_none


------------  END: Defaults of Records

------------  ShowTW of Records

--compileS


instance ShowTW TextConfigTW where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_text_size cfg) showTW
    , renderWhenTW (_text_weight cfg) showTW
    , renderWhenTW (_text_font cfg) showTW
    , renderWhenTW (_text_style cfg) showTW
    , renderWhenTW (_text_cursor cfg) showTW
    , renderWhenTW (_text_color cfg) ((<>) "text-" . showTW)
    , showTW (_text_decoration cfg)
--    , "align-left"
    --, "text-" <> showTW align
    , _text_custom cfg
    ]




instance ShowTW TextDecorationTW where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_textDec_line cfg) showTW
    , renderWhenTW (_textDec_color cfg) ((<>) "decoration-" . showTW)
    , renderWhenTW (_textDec_style cfg) showTW
    , renderWhenTW (_textDec_thickness cfg) showTW
    , renderWhenTW (_textDec_offset cfg) showTW
    ]

instance ShowTW BoxConfig where
  showTW cfg = foldr (<&>) mempty
   [ renderWhenTW (_colStart cfg) ((<>) "col-start-" . tshow)
   , renderWhenTW (_colSpan cfg) ((<>) "col-span-" . tshow)
   , renderWhenTW (_bgColor cfg) ((<>) "bg-" . showTW)
   , renderWhenTW (_bgOpacity cfg) ((<>) "bg-opacity-" . tshow)
   , showTW . _border $ cfg
   , showTW . _sizingBand $ cfg
   , showTW . _padding $ cfg
   , showTW . _margin $ cfg
   , foldr (<&>) mempty $ fmap
     (\(c,(jus,align)) ->
        let pre = if c == "def" then "" else (c <> ":")
        in pre <> "grid" <&> pre <> (showTW jus) <&> pre <> (showTW align)
     ) $ _position cfg
   --, renderWhenTW (_position cfg) $ \(j,a) -> "grid " <> showTW j <> " " <> showTW a
   , _box_custom cfg
   ]

instance ShowTW BorderConfig where
  showTW cfg = foldr (<&>) mempty
    [ showTW . _radius $ cfg
    , showTW . _bWidth $ cfg
    , showTW . _bColor $ cfg
    , renderWhenTW (_bStyle cfg) ((<>) "border-" . showTW)
    ]

instance ShowTW BoxSizing where
  showTW cfg = foldr (<&>) mempty
  --showTW (BoxSizing w h) = T.intercalate " " $
    [ renderWhenTW (_width cfg) ((<>) "w-" . showTW)
    , renderWhenTW (_height cfg) ((<>) "h-" . showTW)
    ]

instance ShowTW BoxSizingBand where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_widthC . _maxSize $ cfg) ((<>) "max-w-" . showTW)
    , renderWhenTW (_heightC . _maxSize $ cfg) ((<>) "max-h-" . showTW)
    , renderWhenTW (_widthC . _minSize $ cfg) ((<>) "min-w-" . showTW)
    , renderWhenTW (_heightC . _minSize $ cfg) ((<>) "min-h-" . showTW)
    , showTW $ _size cfg
    ]

instance ShowTW BoxMargin where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_marginL cfg) ((<>) "ml-" . showTW)
    , renderWhenTW (_marginR cfg) ((<>) "mr-" . showTW)
    , renderWhenTW (_marginT cfg) ((<>) "mt-" . showTW)
    , renderWhenTW (_marginB cfg) ((<>) "mb-" . showTW)
    ]


instance ShowTW BoxPadding where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_paddingL cfg) ((<>) "pl-" . showTW)
    , renderWhenTW (_paddingR cfg) ((<>) "pr-" . showTW)
    , renderWhenTW (_paddingT cfg) ((<>) "pt-" . showTW)
    , renderWhenTW (_paddingB cfg) ((<>) "pb-" . showTW)
    ]

-- TODO: stop overlaps through conditionals
instance ShowTW BorderRadiusCorners where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_borderRadius_tr cfg) ((<>) "rounded-tr" . showTW)
    , renderWhenTW (_borderRadius_tl cfg) ((<>) "rounded-tl" . showTW)
    , renderWhenTW (_borderRadius_br cfg) ((<>) "rounded-br" . showTW)
    , renderWhenTW (_borderRadius_bl cfg) ((<>) "rounded-bl" . showTW)
    ]

instance ShowTW BorderWidthSides where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_borderWidth_l cfg) ((<>) "border-l" . showTW)
    , renderWhenTW (_borderWidth_r cfg) ((<>) "border-r" . showTW)
    , renderWhenTW (_borderWidth_t cfg) ((<>) "border-t" . showTW)
    , renderWhenTW (_borderWidth_b cfg) ((<>) "border-b" . showTW)
    ]

instance ShowTW BorderColorSides where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_borderColor_l cfg) ((<>) "border-l-" . showTW)
    , renderWhenTW (_borderColor_r cfg) ((<>) "border-r-" . showTW)
    , renderWhenTW (_borderColor_t cfg) ((<>) "border-t-" . showTW)
    , renderWhenTW (_borderColor_b cfg) ((<>) "border-b-" . showTW)
    ]

------------  END: ShowTW of Records


------------  Lenses
-- | Lenses will need 2 extensions:
  -- `only`
  -- puts (WhenTW t)

borderRadius_l, borderRadius_r, borderRadius_t, borderRadius_b :: Lens' BorderRadiusCorners (WhenTW BorderRadius')
borderRadius_l = lens undefined $ \tw new -> tw { _borderRadius_tl = new, _borderRadius_bl = new }
borderRadius_r = lens undefined $ \tw new -> tw { _borderRadius_tr = new, _borderRadius_br = new }
borderRadius_t = lens undefined $ \tw new -> tw { _borderRadius_tl = new, _borderRadius_tr = new }
borderRadius_b = lens undefined $ \tw new -> tw { _borderRadius_bl = new, _borderRadius_br = new }


------------  END: Lenses

    -- the corners override the sides
    -- [ "rounded-l" <> showTW l
    -- , "rounded-r" <> showTW r
    -- , "rounded-t" <> showTW t
    -- , "rounded-b" <> showTW b
    -- ]

data BorderRadius'
  = R_None
  | R_SM
  | R_Normal
  | R_Md
  | R_Lg
  | R_Xl
  | R_2Xl
  | R_3Xl
  | R_Full
  | R_Custom CSSSize
  deriving Show

instance Default BorderRadius' where
  def = R_None

instance ShowTW BorderRadius' where
  showTW = \case
    R_Custom cssSize -> "-[" <> renderCSS cssSize <> "]"
    R_Normal -> ""
    other -> "-" <> (T.toLower . T.drop 2 . tshow $ other)


data BorderWidth
  = B0
  | B1
  | B2
  | B4
  | B8
  | BW_Custom CSSSize
  deriving Show

instance Default BorderWidth where
  def = B0

instance ShowTW BorderWidth where
  showTW = \case
    B1 -> ""
    BW_Custom cssSize -> "-[" <> renderCSS cssSize <> "]"
    other -> "-" <> (T.drop 1 . tshow $ other)


data BorderStyle
  = BSolid
  | BDashed
  | BDotted
  | BDouble
  | BHidden
  | BNone
  deriving Show

instance Default BorderStyle where
  def = BSolid

instance ShowTW BorderStyle where
  showTW = T.toLower . T.drop 1 . tshow





data CursorStyle
  = CursorAuto
  | CursorDefault
  | CursorPointer
  | CursorWait
  | CursorText
  | CursorMove
  | CursorHelp
  | CursorNotAllowed
  | CursorNone
  | CursorContextMenu
  | CursorProgress
  | CursorCell
  | CursorCrosshair
  | CursorVerticalText
  | CursorAlias
  | CursorCopy
  | CursorNoDrop
  | CursorGrab
  | CursorGrabbing
  | CursorAllScroll
  | CursorColResize
  | CursorRowResize
  | CursorNResize
  | CursorEResize
  | CursorSResize
  | CursorWResize
  | CursorNEResize
  | CursorNWResize
  | CursorSEResize
  | CursorSWResize
  | CursorEWResize
  | CursorNSResize
  | CursorNESWResize
  | CursorNWSEResize
  | CursorZoomIn
  | CursorZoomOut
  deriving Show

instance Default CursorStyle where
  def = CursorDefault

instance ShowTW CursorStyle where
  showTW cursor = T.pack $ toKebabCase (show cursor)


-- | TODO: Isolate in own module since only used for gridCol
data ColInt
  = Col1
  | Col2
  | Col3
  | Col4
  | Col5
  | Col6
  | Col7
  | Col8
  | Col9
  | Col10
  | Col11
  | Col12
  deriving Show

instance ShowTW ColInt where
  showTW = T.drop 3 . T.pack . show

instance Default ColInt where
  def = Col1

data Dimension
  = DNum Float
  | DFrac Int DivInt
  | Full
  | Screen
  | Min
  | Max
  | Fit
  | Dim_Custom T.Text
--  | None

-- there's TWSize and then TWSizeOrFraction
data TWSize
  = TWSize Float
  | TWSize_Custom CSSSize
  deriving Show

instance ShowTW TWSize where
  showTW = \case
    TWSize float ->
      if fromIntegral (truncate float :: Int) == float
      then tshow $ (truncate float :: Int)
      else tshow float
    TWSize_Custom c -> "[" <> renderCSS c <> "]"

data TWSizeOrFraction
  = TWSize' TWSize
  | TWFraction Int DivInt
  | TWSize_Full
  | TWSize_Screen
  | TWSize_Min
  | TWSize_Max
  | TWSize_Fit
  | TWSize_Auto
  deriving Show

instance ShowTW TWSizeOrFraction where
  showTW = \case
    TWSize' s -> showTW s
    TWFraction n d -> tshow n <> "/" <> showTW d
    class' -> T.toLower . T.drop 7 . tshow $ class'

instance Default TWSizeOrFraction where
  def = TWSize_Auto

data DivInt
  = D2
  | D3
  | D4
  | D5
  | D6
  | D12
  deriving Show

instance ShowTW DivInt where
  showTW = T.drop 1 . tshow

instance Default DivInt where
  def = D12 -- baby

-- | NOTE: although I cant do Start for either, I could create a class like HasStart
-- | Also NOTE: there is no plan to make text align, rather you put it in a span and align it via this mechanism
data Justify
  = J_Start
  | J_End
  | J_Center
  | J_Stretch
  deriving Show

data Align
  = A_Start
  | A_End
  | A_Center
  | A_Baseline
  deriving Show

instance Default Justify where
  def = J_Start
instance Default Align where
  def = A_Start


instance ShowTW Justify where
  showTW = (<>) "justify-items-" . T.toLower . T.drop 2 . T.pack . show
instance ShowTW Align where

  showTW = (<>) "content-" . T.toLower . T.drop 2 . T.pack . show

-- | TODO: this is technically wrong: there are different classes for width vs height but oh well for now
data DimensionConstraint
  = DC_0
  | DC_none
  | DC_xs
  | DC_sm
  | DC_md
  | DC_lg
  | DC_xl
  | DC_2xl
  | DC_3xl
  | DC_4xl
  | DC_5xl
  | DC_6xl
  | DC_7xl
  | DC_full
  | DC_min
  | DC_max
  | DC_fit
  | DC_prose
  | DC_screen_sm
  | DC_screen_md
  | DC_screen_lg
  | DC_screen_xl
  | DC_screen_2xl
  | DC_Custom T.Text
  deriving Show

instance Default DimensionConstraint where
  def = DC_none

instance ShowTW DimensionConstraint where
  showTW = \case
    DC_Custom t -> "[" <> t <> "]"
    x -> T.replace "_" "-" . T.drop 3 . tshow $ x




-- DEPRECATED: use SetSides model
-- | TODO: move to SetSides lens model (t,b,r,l) all valid
data BorderRadius
  = None
  | RoundedSM Corner
  | Rounded Corner
  | RoundedMd Corner
  | RoundedLg Corner
  | RoundedXl Corner
  | Rounded2Xl Corner
  | Rounded3Xl Corner
  | RoundedFull Corner
  deriving Show



data Corner
  = All -- -> ""
  | S
  | E
  | T
  | R
  | B
  | L
  | SS
  | SE
  | EE
  | ES
  | TL
  | TR
  | BR
  | BL
  deriving Show

tshowCorner :: Corner -> T.Text
tshowCorner = \case
  All -> ""
  x -> ((<>) "-") . T.toLower . tshow $ x

instance Default BorderRadius where
  def = None

-- DEPRECATED
instance ShowTW BorderRadius where
  showTW = ((<>) "rounded") . \case
    None -> "-none"
    Rounded c -> tshowCorner c --"-" <> (T.toLower . tshow $ c) <> "-" <> "sm"
    RoundedSM c -> tshowCorner c <> "-" <> "sm"
    RoundedMd c -> tshowCorner c <> "-" <> "md"
    RoundedLg c -> tshowCorner c <> "-" <> "lg"
    RoundedXl c -> tshowCorner c <> "-" <> "xl"
    Rounded2Xl c -> tshowCorner c <> "-" <> "2xl"
    Rounded3Xl c -> tshowCorner c <> "-" <> "3xl"
    RoundedFull c -> tshowCorner c <> "-" <> "full"


-- | todo: can we merge with gridCol Col1-12?
data IntGrid
  = IG1
  | IG2
  | IG3
  | IG4
  | IG5
  | IG6
  | IG7
  | IG8
  | IG9
  | IG10
  | IG11
  | IG12


-- -- | Could also just do variants where they are evenly spaced and such
-- boxInGrid :: BoxConfig -> (IntGrid, IntGrid) -> m ()
-- boxInGrid = undefined



-- data BoxConfig = BoxConfig
--   { leftPadding :: T.Text
--   , bottomPadding :: T.Text
--   }

-- type Test = TWConfig BoxConfig


-- | Right now this is just for custom uses in Tailwind
data CSSSize
  = Pixel Int
  | Percent Int
  | Vh Int
  | Vw Int
  | Rem Float
  deriving Show

instance IsCSS CSSSize where
  renderCSS = \case
    Pixel int -> tshow int <> "px"
    Percent int -> tshow int <> "%"
    Vh int -> tshow int <> "vh"
    Vw int -> tshow int <> "vw"
    Rem float -> tshow float <> "rem"


-- -- | TODO(galen): remove ShowTW a constraint
-- data TWConfig a = ShowTW a => TWConfig { unTWConfig :: [(Maybe TWCondition, a)] }

-- instance (ShowTW a, Default a) => Default (TWConfig a) where
--   def = TWConfig [(Nothing, def)]

-- instance ShowTW (TWConfig a) where
--   showTW (TWConfig ts) = f ts
--     where
--       f [] = ""
--       f ((c,cfg):cs) = case c of
--         Just c' -> showTWWhen c' cfg <> (f cs)
--         Nothing -> showTW cfg <> (f cs)

--   showTWWhen = \_ -> showTW

-- -- All of these should also take a custom except when it's datatype is T.Text
-- data TextConfigTW = TextConfigTW
--   { _text_size :: WhenTW TextSize
--   , _text_weight :: WhenTW TextWeight
--   , _text_font :: WhenTW Font -- many options -- EG. Sarabun -> font-[Sarabun]
--   , _text_color :: WhenTW Color
--   , _text_decoration :: TextDecorationTW
--   --, _text_align :: Align
--   , _text_custom :: T.Text
--   }

data Font
  = Sans
  | Serif
  | Mono
  | Font_Custom T.Text
  deriving Show

instance Default Font where
  def = Sans

instance ShowTW Font where
  showTW = \case
    Font_Custom t -> "font-[" <> t <> "]"
    x -> ((<>) "font-") . T.toLower . tshow $ x



--  showTWWhen c (TextConfigTW size weight font color dec custom) = T.intercalate " " $
--    [ showTWWhen c size
--    , showTWWhen c weight
--    , showTWWhen c font
--    , c <> ":text-" <> showTW color
--    , showTWWhen c dec
    --, c <> ":text-" <> showTW align
--    , custom
--    ]

  --showTWWhen c tw

-- | This is used for many things
-- | DEPRECATED
-- data Align
--   = Align_Left
--   | Align_Center
--   | Align_Right
--   | Align_Justify
--   | Align_Start
--   | Align_End
--   deriving Show

-- instance Default Align where
--   def = Align_Left

-- instance ShowTW Align where
--   showTW = T.drop 6 . T.toLower . tshow
--   showTWWhen c tw = c <> ":" <> (showTW tw)

-- data TextDecorationTW = TextDecorationTW
--   { _textDec_line :: WhenTW TextDecLineType
--   , _textDec_color :: WhenTW Color
--   , _textDec_style :: WhenTW TextDecStyle
--   , _textDec_thickness :: WhenTW TextDecThickness -- TODO: is this a standard system: Thickness?
--   , _textDec_offset :: WhenTW TextDecOffset -- and is this a system? (the numbers chosen 0,1,2,4,8)
--   }

--  showTWWhen c (TextDecorationTW line color style thickness offset) = T.intercalate " " $
--    [ showTWWhen c line
--    , c <> ":decoration-" <> showTW color
--    , showTWWhen c style
--    , showTWWhen c thickness
--    , showTWWhen c offset
--    ]

data TextDecLineType
  = Underline
  | Overline
  | LineThrough
  | NoUnderline
  deriving Show

instance Default TextDecLineType where
  def = NoUnderline

instance ShowTW TextDecLineType where
  showTW = \case
    LineThrough -> "line-through"
    NoUnderline -> "no-underline"
    x -> T.toLower . tshow $ x
--  showTWWhen c tw = c <> ":" <> (showTW tw)

data TextDecStyle
  = Solid
  | Double
  | Dotted
  | Dashed
  | Wavy
  deriving Show

-- Note that we may need to make this a Maybe TextDecStyle
instance Default TextDecStyle where
  def = Solid

instance ShowTW TextDecStyle where
  showTW = ((<>) "decoration-") . T.toLower . tshow
  --showTWWhen c tw = c <> ":" <> (showTW tw)

data TextDecThickness
  = FromFont
  | TextDecThickness TWNum
  | TextDecThickness_Custom CSSSize -- TODO: is this just size??
  deriving Show

instance Default TextDecThickness where
  def = TextDecThickness def

instance ShowTW TextDecThickness where
  showTW thk = "decoration-" <> ( case thk of
                                    FromFont -> "from-font"
                                    TextDecThickness twnum -> showTW twnum
                                    TextDecThickness_Custom cssSize -> "[" <> (renderCSS cssSize) <> "]"
                                )
--  showTWWhen c tw = c <> ":" <> (showTW tw)

data TWNum
  = Auto
  | TW0
  | TW1
  | TW2
  | TW4
  | TW8
  deriving Show

instance Default TWNum where
  def = TW1

-- | TODO: see note about color / showColor ; showTWNum
instance ShowTW TWNum where
  showTW Auto = "auto"
  showTW x = T.drop 2 $ tshow x


data TextDecOffset
  = TextDecOffset TWNum
  | TextDecOffset_Custom CSSSize
  deriving Show

instance Default TextDecOffset where
  def = TextDecOffset def

instance ShowTW TextDecOffset where
  showTW (TextDecOffset tnum) = "underline-offset-" <> (showTW tnum)
  showTW (TextDecOffset_Custom cssS) = "underline-offset-[" <> (renderCSS cssS) <> "]"
  --showTWWhen c tw = c <> ":" <> (showTW tw)


data FontStyle
  = Italic
  | NotItalic
  deriving Show

instance Default FontStyle where
  def = NotItalic

instance ShowTW FontStyle where
  showTW font = T.pack $ toKebabCase (show font)

data TextSize
  = XS
  | SM
  | Base
  | LG
  | XL
  | XL2 -- According to testing this is the default size `\_o_/`
  | XL3
  | XL4
  | XL5
  | XL6
  | XL7
  | XL8
  | XL9
  | TextSize_Custom T.Text
  deriving Show

-- | TODO: when this inevitably becomes its own package, make it easy to not import Defaults and make your own
instance Default TextSize where
  def = XL2 -- According to testing this is the default size `\_o_/'

instance ShowTW TextSize where
  showTW (TextSize_Custom t) = "text-[" <> t <> "]"
  showTW XS = "text-xs"
  showTW SM = "text-sm"
  showTW Base = "text-base"
  showTW LG = "text-lg"
  showTW XL = "text-xl"
  showTW XL2 = "text-2xl"
  showTW XL3 = "text-3xl"
  showTW XL4 = "text-4xl"
  showTW XL5 = "text-5xl"
  showTW XL6 = "text-6xl"
  showTW XL7 = "text-7xl"
  showTW XL8 = "text-8xl"
  showTW XL9 = "text-9xl"

--  showTWWhen c tw = c <> ":" <> (showTW tw)

data TextWeight
  = Thin
  | Extralight
  | Light
  | Normal
  | Medium
  | Semibold
  | Bold
  | Extrabold
  | Black_TW
  | TextWeight_Custom T.Text
  deriving Show

instance Default TextWeight where
  def = Normal

instance ShowTW TextWeight where
  showTW (TextWeight_Custom t) = "font-[" <> t <> "]"
  showTW (Black_TW) = "font-black"
  showTW x = "font-" <> (T.toLower . tshow $ x )
  --showTWWhen c tw = c <> ":" <> (showTW tw)

-- | TODO: myColor :: MyColor -> Color where MyColor is a typified Design Choice
-- | Note that this can be used for either background or text
data Color
  = Inherit
  | Current
  | Transparent
  | Black
  | White
  | Slate ColorNum
  | Gray ColorNum
  | Zinc ColorNum
  | Neutral ColorNum
  | Stone ColorNum
  | Red ColorNum
  | Orange ColorNum
  | Amber ColorNum
  | Yellow ColorNum
  | Lime ColorNum
  | Green ColorNum
  | Emerald ColorNum
  | Teal ColorNum
  | Cyan ColorNum
  | Sky ColorNum
  | Blue ColorNum
  | Indigo ColorNum
  | Violet ColorNum
  | Purple ColorNum
  | Fuchsia ColorNum
  | Pink ColorNum
  | Rose ColorNum
  | Color_Custom Hex
  deriving (Show, Eq)

-- Sky (ColorNum_Custom x)

hex :: T.Text -> Color
hex = Color_Custom . Hex
newtype Hex = Hex { unHex :: T.Text } deriving (Eq, Show)

-- TODO: own defaults
instance Default Color where
  def = Black

data ColorNum
 = C50
 | C100
 | C200
 | C300
 | C400
 | C500
 | C600
 | C700
 | C800
 | C900
 | C950
 deriving (Show, Eq)

instance Default ColorNum where
  def = C950


-- | There is no showTW for ColorNum since this will never be without an encompassing Color
-- | TODO(galen): Should we just make this its own special function? and then its called by showTW; showColor
-- | Because this is never used on its own either
instance ShowTW Color where
  showTW (Color_Custom (Hex h)) = "[#" <> h <> "]"
  showTW Inherit = "inherit"
  showTW Current = "current"
  showTW Transparent = "transparent"
  showTW Black = "black"
  showTW White = "white"
  showTW color = case T.words $ tshow color of
    c:(mag):[] -> (T.toLower c) <> "-" <> (T.drop 1 mag) -- T.words $ tshow color
    _ -> "ClasshSS: failed on input" <> (tshow color)
          
    -- in (T.toLower c) <> "-" <> (T.drop 1 mag)

--  showTWWhen c tw = showTW tw
  --

type BottomPadding = TWSize

-- tshow :: Show a => a -> T.Text
-- tshow = T.pack . show

makeLenses ''TextConfigTW
makeLenses ''TextDecorationTW

makeLenses ''BoxConfig
makeLenses ''BoxPadding
makeLenses ''BoxMargin
makeLenses ''BoxSizingBand
makeLenses ''BoxSizing
makeLenses ''BoxSizingConstraint
makeLenses ''BorderConfig
makeLenses ''BorderRadiusCorners
makeLenses ''BorderWidthSides
makeLenses ''BorderColorSides


-- | This is technically an illegal lens however if you ran 2 setters which overlap so that a /= b
-- | where a and b are the fields associated with respective separate fields, then classh' will
-- | most likely catch the error. Additionally, there is a lens way to access any field anyways
instance SetSides BoxMargin TWSize where
  l = marginL
  r = marginR
  b = marginB
  t = marginT
  x = lens _marginL $ \tw new -> tw { _marginL = new, _marginR = new }
  y =  lens _marginT $ \tw new -> tw { _marginT = new, _marginB = new }
  xy = lens _marginT $ \tw new -> tw { _marginT = new
                                     , _marginB = new
                                     , _marginL = new
                                     , _marginR = new
                                     }


-- | This is technically an illegal lens however if you ran 2 setters which overlap so that a /= b
-- | where a and b are the fields associated with respective separate fields, then classh' will
-- | most likely catch the error. Additionally, there is a lens way to access any field anyways
instance SetSides BoxPadding TWSize where
  l = paddingL
  r = paddingR
  b = paddingB
  t = paddingT
  x = lens _paddingL $ \tw new -> tw { _paddingL = new, _paddingR = new }
  y = lens _paddingR $ \tw new -> tw { _paddingT = new, _paddingB = new }
  xy = lens _paddingT $ \tw new -> tw { _paddingT = new
                                      , _paddingB = new
                                      , _paddingL = new
                                      , _paddingR = new
                                      }

instance SetSides BorderRadiusCorners BorderRadius' where
  l = borderRadius_l
  r = borderRadius_r
  b = borderRadius_b
  t = borderRadius_t
  xy = lens _borderRadius_tl $ \tw new -> tw { _borderRadius_tl = new
                                             , _borderRadius_bl = new
                                             , _borderRadius_tr = new
                                             , _borderRadius_br = new
                                             }
  -- Effectively, due to corners;
  y = xy
  x = xy

instance SetSides BorderWidthSides BorderWidth where
  l = borderWidth_l
  r = borderWidth_r
  t = borderWidth_t
  b = borderWidth_b
  x = lens _borderWidth_l $ \tw new -> tw { _borderWidth_l = new, _borderWidth_r = new }
  y = lens _borderWidth_b $ \tw new -> tw { _borderWidth_b = new, _borderWidth_t = new }
  xy = lens _borderWidth_b $ \tw new -> tw { _borderWidth_b = new
                                           , _borderWidth_t = new
                                           , _borderWidth_l = new
                                           , _borderWidth_r = new
                                           }

instance SetSides BorderColorSides Color where
  l = borderColor_l
  r = borderColor_r
  t = borderColor_t
  b = borderColor_b
  x = lens _borderColor_l $ \tw new -> tw { _borderColor_l = new, _borderColor_r = new }
  y = lens _borderColor_t $ \tw new -> tw { _borderColor_t = new, _borderColor_b = new }
  xy = lens _borderColor_t $ \tw new -> tw { _borderColor_t = new
                                           , _borderColor_b = new
                                           , _borderColor_l = new
                                           , _borderColor_r = new
                                           }


instance HasCustom BoxConfig where
  custom = box_custom

instance HasCustom TextConfigTW where
  custom = text_custom


-- deprecated?
-- | TODO: make a setter that takes both the color and opacity as args
-- | We need this cuz the Opacity defaults to 0
-- bgColor' :: Lens' BoxConfig Color
-- bgColor' = lens undefined $ \tw new -> tw { _bgColor = new , _bgOpacity = 100 }


pt, pl, pr, pb, px, py, p :: Lens' BoxConfig (WhenTW TWSize)
pt = padding . t
pb = padding . b
pl = padding . l
pr = padding . r
px = padding . x
py = padding . y
p = padding . allS

mt, ml, mr, mb, mx, my, m :: Lens' BoxConfig (WhenTW TWSize)
mt = margin . t
mb = margin . b
ml = margin . l
mr = margin . r
mx = margin . x
my = margin . y
m = margin . allS


-- | If I want to make this not undefined, then I need pos to be an
-- | actual field, which contains Justify and Align as its own record fields
pos :: Lens' BoxConfig (WhenTW (Justify, Align))
pos = position --lens undefined $ \cfg new -> cfg { _position =  new }

-- pos :: Lens' BoxConfig (Justify, Align)
-- pos = position . _Just
width', height', w, h :: Lens' BoxConfig (WhenTW TWSizeOrFraction)
width' = sizingBand . size . width
height' = sizingBand . size . height
w = width'
h = height'

br_r, br_l, br_t, br_b, br_y, br_x, br :: Lens' BoxConfig (WhenTW BorderRadius')
br_r = border . radius . r 
br_l = border . radius . l
br_t = border . radius . t
br_b = border . radius . b
br_y = border . radius . y
br_x = border . radius . x
br = border . radius . allS 


bw_r, bw_l, bw_t, bw_b, bw_y, bw_x, bw :: Lens' BoxConfig (WhenTW BorderWidth)
bw_r = border . bWidth . r 
bw_l = border . bWidth . l
bw_t = border . bWidth . t
bw_b = border . bWidth . b
bw_y = border . bWidth . y
bw_x = border . bWidth . x
bw = border . bWidth . allS 


bc_r, bc_l, bc_t, bc_b, bc_y, bc_x, bc :: Lens' BoxConfig (WhenTW Color)
bc_r = border . bColor . r 
bc_l = border . bColor . l
bc_t = border . bColor . t
bc_b = border . bColor . b
bc_y = border . bColor . y
bc_x = border . bColor . x
bc = border . bColor . allS 


t_italic :: TextConfigTW -> TextConfigTW 
t_italic = text_style .~~ Italic

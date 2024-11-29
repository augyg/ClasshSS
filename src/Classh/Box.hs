{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Classh.Box (module X, module Classh.Box) where

-- Our goto module
import Classh.Class.HasCustom
import Classh.Class.ShowTW
import Classh.Class.CompileStyle
import Classh.Internal.Chain
import Classh.Internal.TShow

import Classh.Internal.TWNum as X
import Classh.Responsive.WhenTW as X
import Classh.Color as X
import Classh.Box.TWSize as X
import Classh.Box.Padding as X
import Classh.Box.Margin as X
import Classh.Box.Sizing as X
import Classh.Box.Placement as X
import Classh.Box.Border as X

import Control.Lens hiding ((<&>))
import Data.Default
import qualified Data.Text as T

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


makeLenses ''BoxConfig


------------  Defaults of Records

instance Default BoxConfig where
  def = BoxConfig def def def def def def def def def ""


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

instance HasCustom BoxConfig where
  custom = box_custom

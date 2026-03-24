-- |
-- Module      : Classh.TextPosition
-- Description : Text position and layout properties
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.TextPosition
  ( module Clamp
  , module Content
  , module Hyphen
  , module Lead
  , module TAlign
  , module VAlign
  , module TOverflow
  , module Track
  , module WhiteSpace
  , module WordBreak
  , module Wrap
  , TextPosition(..)
  , tp_track
  , tp_clamp
  , tp_lineHeight
  , tp_position
  , tp_overflow
  , tp_wrap
  , tp_indent
  , tp_wordBreak
  , tp_whiteSpace
  , tp_hyphen
  , tp_content
  , tp_custom
  )
  where

import Classh.TextPosition.Clamp as Clamp
import Classh.TextPosition.Content as Content
import Classh.TextPosition.Hyphen as Hyphen
import Classh.TextPosition.Lead as Lead
import Classh.TextPosition.TAlign as TAlign
import Classh.TextPosition.VAlign as VAlign
import Classh.TextPosition.TOverflow as TOverflow
import Classh.TextPosition.Track as Track
import Classh.TextPosition.WhiteSpace as WhiteSpace
import Classh.TextPosition.WordBreak as WordBreak
import Classh.TextPosition.Wrap as Wrap

import Classh.Class.CompileStyle as CompileStyle
import Classh.Class.ShowTW (ShowTW(..))
import Classh.Internal.Chain ((<&>))
import Classh.Box.TWSize as TWSize
import Classh.Responsive.WhenTW as WhenTW
import Control.Lens hiding ((<&>))
import Data.Default (Default(..))
import qualified Data.Text as T 

data TextPosition = TextPosition
  { _tp_track        :: WhenTW Track
  , _tp_clamp        :: WhenTW Clamp
  , _tp_lineHeight   :: WhenTW Lead
  , _tp_position     :: WhenTW (TAlign, VAlign)
  -- , _tp_align        :: WhenTW TAlign
  -- , _tp_vertical     :: WhenTW VAlign
  , _tp_overflow     :: WhenTW TOverflow
  , _tp_wrap         :: WhenTW Wrap
  , _tp_indent       :: WhenTW TWSize
  , _tp_wordBreak    :: WhenTW WordBreak
  , _tp_whiteSpace   :: WhenTW WhiteSpace
  , _tp_hyphen       :: WhenTW Hyphen
  , _tp_content      :: WhenTW Content
  , _tp_custom       :: T.Text
  } deriving (Show)

makeLenses ''TextPosition

instance Default TextPosition where
  def = TextPosition
    { _tp_track       = def
    , _tp_clamp       = def
    , _tp_lineHeight  = def
    , _tp_position    = def
    -- , _tp_align     = def  -- commented out in your code
    -- , _tp_vertical  = def  -- commented out in your code
    , _tp_overflow    = def
    , _tp_wrap        = def
    , _tp_indent      = def
    , _tp_wordBreak   = def
    , _tp_whiteSpace  = def
    , _tp_hyphen      = def
    , _tp_content     = def
    , _tp_custom      = ""
    }

instance CompileStyle TextPosition where
  compileS cfg = pure . foldr (<&>) mempty =<< sequenceA
    [ compileWhenTW (_tp_track cfg) showTW
    , compileWhenTW (_tp_clamp cfg) showTW
    , compileWhenTW (_tp_lineHeight cfg) showTW
    , compilePos (_tp_position cfg)
    , compileWhenTW (_tp_overflow cfg) showTW
    , compileWhenTW (_tp_wrap cfg) showTW
    , compileWhenTW (_tp_indent cfg) showTW
    , compileWhenTW (_tp_wordBreak cfg) showTW
    , compileWhenTW (_tp_whiteSpace cfg) showTW
    , compileWhenTW (_tp_hyphen cfg) showTW
    , compileWhenTW (_tp_content cfg) showTW
    ]
    where
    compilePos posCfg = case f $ fmap fst posCfg of
      Left e -> Left e
      Right () -> Right $ foldr (<&>) mempty $ fmap
        (\(c,(jus,align)) ->
           let prefix = if c == "def" then "" else (c <> ":")
           in
             prefix <> "grid" <&> prefix <> (showTW jus) <&> prefix <> (showTW align)
        ) $ posCfg
      where
        f [] = Right ()
        f (s:ss) =
          if elem s ss
          then Left $ s <> " exists twice"
          else f ss


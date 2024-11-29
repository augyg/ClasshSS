module Classh.Responsive.WhenTW where

import Classh.Internal.Chain
import Classh.Class.ShowTW
import qualified Data.Text as T

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
      
renderWhenTW :: WhenTW a -> (a -> T.Text) -> T.Text
renderWhenTW tws construct = foldr (<&>) mempty $
  fmap (\(c,p) -> (if c == "def" then "" else (c <> ":")) <> construct p) $ tws

-- | This only ever makes sense for edge cases
-- | 2 is because this is a rank2 HOF 
renderWhenTWRank2 :: WhenTW a -> (TWCondition -> a -> T.Text) -> T.Text
renderWhenTWRank2 tws construct = foldr (<&>) mempty $ fmap (\(c,p) -> construct c p) $ tws

-- | Simple Builder 
twWhenText :: TWCondition -> T.Text -> T.Text
twWhenText c sty = mkConditionPrefix c <> sty

twWhen :: ShowTW a => TWCondition -> a -> T.Text
twWhen c sty = mkConditionPrefix c <> showTW sty

--fmap (\(c,p) -> (if c == "def" then "" else (c <> ":")) <> construct c p) $ tws
mkConditionPrefix :: T.Text -> T.Text 
mkConditionPrefix c = if c == "def" then "" else (c <> ":")

type TWCondition = T.Text
type WhenTW a = [(TWCondition, a)]
newtype WhenTW' a = WhenTW' { unWhenTW :: [(TWCondition, a)] }

-- Useful for when you want config relative to another one.
-- eg. width2 = width1 + 1px
instance Functor WhenTW' where
  fmap f whenTW = WhenTW' $ fmap (\(c,a) -> (c, f a)) $ unWhenTW whenTW
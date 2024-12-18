module Classh.Class.HasCSSSize where


-- | Represents the ability to use raw CSS Sizing in a given instance/context
class HasCSSSize tw where
  pix :: Int -> tw 
  pct :: Int -> tw
  vh :: Int -> tw
  vw :: Int -> tw
  rem :: Float -> tw

module Classh.Class.HasCSSSize where


class HasCSSSize tw where
  pix :: Int -> tw 
  pct :: Int -> tw
  vh :: Int -> tw
  vw :: Int -> tw
  rem :: Float -> tw

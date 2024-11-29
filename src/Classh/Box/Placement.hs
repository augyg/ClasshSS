module Classh.Box.Placement
  ( module X
  , Matrix33(..)
  , topLeft
  , middleLeft
  , bottomLeft
  , topCenter
  , centered
  , bottomCenter
  , topRight
  , middleRight
  , bottomRight
  , centeredOnly
  ) where

import Classh.Box.Placement.Justify as X 
import Classh.Box.Placement.Align as X

import Classh.Responsive.WhenTW
import Classh.Responsive.ZipScreens 


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

centeredOnly :: WhenTW (Justify, Align)
centeredOnly = only centered


-- TODO: create showTW which calls showTW on align and justify to make maintaining easier 

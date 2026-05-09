{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Algorithm.Control.Draw where

import Algorithm.Control.Type
import Algorithm.Dubins.Draw (drawDubinsGraphs)
import Algorithm.Dubins.Type
import Data.List.Split (divvy)
import Diagrams.Backend.Cairo
import Diagrams.Prelude hiding (position)

drawVehiclePose :: FilePath -> String -> [DubinsPath] -> [StanleyControllerState] -> IO ()
drawVehiclePose folderpath plotname dpaths poses =
  let vehiclePoses = drawGraphs (map newConfiguration poses)
      dubinspathsdiag = drawDubinsGraphs dpaths
   in renderCairo (folderpath ++ plotname ++ ".png") (dims (V2 800 800)) (bg white $ (vehiclePoses <> dubinspathsdiag) # centerXY # padX 1.3 # padY 1.1)

drawGraphs :: [Configuration] -> Diagram B
drawGraphs configs = mconcat $ map (\[c1, c2] -> drawGraph c1 c2) $ divvy 2 1 configs

drawGraph :: Configuration -> Configuration -> Diagram B
drawGraph startConfig endConfig = drawConfigurations startConfig endConfig

drawConfigurations :: Configuration -> Configuration -> Diagram B
drawConfigurations config1 config2 =
  let V2 x1 y1 = position config1
      V2 x2 y2 = position config2
   in p2 (x1, y1) ~~ p2 (x2, y2) # lc red # lw thin

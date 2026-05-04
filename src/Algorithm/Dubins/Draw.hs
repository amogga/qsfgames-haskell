{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Algorithm.Dubins.Draw (createDubinsDiagramSequence, createDubinsDiagram) where

import Algorithm.Dubins.Dubins
import Algorithm.Dubins.Type
import Data.List.Split (divvy)
import Data.Matrix
import Diagrams.Backend.Cairo
import Diagrams.Prelude hiding (position)

createDubinsDiagramSequence :: MinimumRadius -> [Configuration] -> IO ()
createDubinsDiagramSequence r configs =
  renderCairo
    ("plots/dubins/dplot_sequence.pdf")
    (dims (V2 400 400))
    (bg white $ drawGraphs configs (computeDubinsPaths r configs) # centerXY # padX 1.3 # padY 1.1)

createDubinsDiagram :: MinimumRadius -> Configuration -> Configuration -> IO ()
createDubinsDiagram r config1 config2 = createDiagram config1 config2 dubinsPath
  where
    dubinsPath = computeDubinsPath r config1 config2

createDiagram :: Configuration -> Configuration -> DubinsPath -> IO ()
createDiagram config1 config2 dubinsPath = renderCairo ("plots/dubins/dplot.png") (dims (V2 400 400)) (bg white $ drawGraph config1 config2 dubinsPath # centerXY # padX 1.3 # padY 1.1)

drawGraphs :: [Configuration] -> [DubinsPath] -> Diagram B
drawGraphs configs dubinsPath =
  let centersDiagram = mconcat $ map drawCenters dubinsPath
      configsDiagrams = mconcat $ map drawOneConfiguration configs
      pathDiagram = mconcat $ map (\([c1, c2], dinfo) -> drawPath c1 c2 dinfo) $ zip (divvy 2 1 configs) dubinsPath
   in configsDiagrams <> pathDiagram <> centersDiagram

drawGraph :: Configuration -> Configuration -> DubinsPath -> Diagram B
drawGraph startConfig endConfig dubinsPath =
  let centers = drawCenters dubinsPath
      configs = drawConfigurations startConfig endConfig
      pathDiagram = drawPath startConfig endConfig dubinsPath
   in configs <> pathDiagram <> centers

drawPath :: Configuration -> Configuration -> DubinsPath -> Diagram B
drawPath startConfig endConfig (DubinsPath path params) =
  let [x1, y1] = toList $ position startConfig
      [x2, y2] = toList $ position endConfig
      [cs1, cs2] = toList $ startCenter path
      [ce1, ce2] = toList $ endCenter path
      [z11, z12] = toList $ z1 params
      [z21, z22] = toList $ z2 params
      r = minimumRadius path
      dir1 = direction (r2 (x1 - cs1, y1 - cs2))
      dir2 = direction (r2 (z11 - cs1, z12 - cs2))
      dir3 = direction (r2 (z21 - ce1, z22 - ce2))
      dir4 = direction (r2 (x2 - ce1, y2 - ce2))
      (f1, f2) = arcFunctions (pathType path)
   in p2 (z11, z12) ~~ p2 (z21, z22) # lc green # lw thin
        <> f1 dir1 dir2 # lc green # lw thin # scale r # translate (r2 (cs1, cs2))
        <> f2 dir3 dir4 # lc green # lw thin # scale r # translate (r2 (ce1, ce2))

drawOneConfiguration :: Configuration -> Diagram B
drawOneConfiguration config =
  let [x, y] = toList $ position config
   in circle 0.1 # fc red # lw none # translate (r2 (x, y))

drawConfigurations :: Configuration -> Configuration -> Diagram B
drawConfigurations config1 config2 =
  let [x1, y1] = toList $ position config1
      [x2, y2] = toList $ position config2
   in circle 0.1 # fc red # lw none # translate (r2 (x1, y1))
        <> circle 0.1 # fc blue # lw none # translate (r2 (x2, y2))

drawCenters :: DubinsPath -> Diagram B
drawCenters (DubinsPath path _) =
  let [cs1, cs2] = toList $ startCenter path
      [ce1, ce2] = toList $ endCenter path
      r = minimumRadius path
   in circle r # lc gray # lw thin # translate (r2 (cs1, cs2)) # dashingG [0.2, 0.1] 0
        <> circle r # lc gray # lw thin # translate (r2 (ce1, ce2)) # dashingG [0.2, 0.1] 0

arcFunctions :: DubinsPathType -> (Direction V2 Double -> Direction V2 Double -> Diagram B, Direction V2 Double -> Direction V2 Double -> Diagram B)
arcFunctions RSR = (arcCW, arcCW)
arcFunctions RSL = (arcCW, arcCCW)
arcFunctions LSR = (arcCCW, arcCW)
arcFunctions LSL = (arcCCW, arcCCW)

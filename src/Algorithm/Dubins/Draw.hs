{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Algorithm.Dubins.Draw where

import Algorithm.Dubins.Dubins
import Algorithm.Dubins.Type
import Data.Foldable (toList)
import Data.List (nub)
import Data.List.Split (divvy)
import Diagrams.Backend.Cairo
import Diagrams.Prelude hiding (position)

createDubinsDiagramSequence :: [DubinsPath] -> IO ()
createDubinsDiagramSequence dubinspaths =
  renderCairo
    ("plots/dubins/dplot_sequence.pdf")
    (dims (V2 400 400))
    (bg white $ drawDubinsGraphs dubinspaths # centerXY # padX 1.3 # padY 1.1)

createDiagram :: DubinsPath -> IO ()
createDiagram dubinsPath = renderCairo ("plots/dubins/dplot.png") (dims (V2 400 400)) (bg white $ drawDubinsGraph dubinsPath # centerXY # padX 1.3 # padY 1.1)

drawDubinsGraphs :: [DubinsPath] -> Diagram B
drawDubinsGraphs dubinspaths =
  let configs = nub $ concatMap (\(DubinsPath _ _ confgs) -> [startConfig confgs, endConfig confgs]) dubinspaths
      centersDiagram = mconcat $ map drawCenters dubinspaths
      configsDiagrams = mconcat $ map drawOneConfiguration configs
      pathDiagram = mconcat $ map (\(_, dinfo) -> drawPath dinfo) $ zip (divvy 2 1 configs) dubinspaths
   in configsDiagrams <> pathDiagram <> centersDiagram

drawDubinsGraph :: DubinsPath -> Diagram B
drawDubinsGraph dubinsPath@(DubinsPath _ _ configs) =
  let centers = drawCenters dubinsPath
      configsD = drawConfigurations (startConfig configs) (endConfig configs)
      pathDiagram = drawPath dubinsPath
   in configsD <> pathDiagram <> centers

drawPath :: DubinsPath -> Diagram B
drawPath (DubinsPath path params configs) =
  let [x1, y1] = toList $ position (startConfig configs)
      [x2, y2] = toList $ position (endConfig configs)
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
drawCenters (DubinsPath path _ _) =
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

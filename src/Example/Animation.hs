module Example.Animation where

import Algorithm.Control.Type
import Algorithm.Dubins.Type
import Control.Monad (forM_)
import Diagrams.Backend.Cairo
import Diagrams.Prelude hiding (position)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath ((</>))
import System.Process (callProcess)

makeAnimation :: Int -> [StanleyControllerState] -> [StanleyControllerState] -> IO ()
makeAnimation exampleNumber scont1 scont2 =
  let cars = zipWith createCars scont1 scont2
      backgr = rect 6 42 # translate (r2 (4, 19))
      traj = createTrajectory scont1 scont2
      alldiag i = backgr <> cars !! i <> traj i <> createGoals (Configuration (V2 6 40) (pi / 2)) (Configuration (V2 6 40) (pi / 2))
      doAnim i = renderCairo ("examples/Example" ++ (show exampleNumber) ++ "/plots/iters/iter" ++ show i ++ ".png") (dims (V2 800 800)) (bg white $ alldiag i # centerXY # padX 1.3 # padY 1.1)
   in mapM_ doAnim [0 .. (length cars) - 1]

createAnimation :: Int -> [StanleyControllerState] -> [StanleyControllerState] -> IO ()
createAnimation exampleNumber scont1 scont2 = do
  let basepath = "./examples/Example" ++ (show exampleNumber) ++ "/plots"
  let outputpath = basepath ++ "/simulation.gif"
  deleteAllFilesInDir $ basepath ++ "/iters"
  let iterationCount = length scont1
  makeAnimation exampleNumber scont1 scont2

  let inputFiles = [basepath ++ "/iters/iter" ++ show n ++ ".png" | n <- [0 .. iterationCount - 1]]
  let command = "magick"
  let args = ["-delay", "2"] ++ inputFiles ++ [outputpath]
  callProcess command args

createTrajectory :: [StanleyControllerState] -> [StanleyControllerState] -> Int -> Diagram B
createTrajectory scont1 scont2 inx =
  mconcat $
    map
      ( \ii ->
          let V2 x1 y1 = position (newConfiguration $ scont1 !! ii)
              V2 x1' y1' = position (newConfiguration $ scont1 !! (ii + 1))
              V2 x2 y2 = position (newConfiguration $ scont2 !! ii)
              V2 x2' y2' = position (newConfiguration $ scont2 !! (ii + 1))
           in ( p2 (x1, y1) ~~ p2 (x1', y1') # lc red # lw thin
                  <> p2 (x2, y2) ~~ p2 (x2', y2') # lc green # lw thin
              ) ::
                Diagram B
      )
      [0 .. inx - 1]

createCars :: StanleyControllerState -> StanleyControllerState -> Diagram B
createCars config1 config2 =
  let V2 x1 y1 = position $ newConfiguration config1
      V2 x2 y2 = position $ newConfiguration config2
   in roundedRect 2.726 1.4 0.1
        # lw none
        # fc (sRGB24read "#cd5c5c")
        # rotate ((orientation $ newConfiguration config1) @@ rad)
        # translate (r2 (x1, y1))
        <> roundedRect 2.726 1.4 0.1
          # lw none
          # fc (sRGB24read "#40826D")
          # rotate ((orientation $ newConfiguration config2) @@ rad)
          # translate (r2 (x2, y2))

createGoals :: Configuration -> Configuration -> Diagram B
createGoals config1 config2 =
  let V2 x1 y1 = position config1
      goal2 = position config2
   in circle 0.1 # translate (r2 (x1, y1))

ensureDirectoryExists :: FilePath -> IO ()
ensureDirectoryExists = createDirectoryIfMissing True

deleteAllFilesInDir :: FilePath -> IO ()
deleteAllFilesInDir dir = do
  files <- listDirectory dir
  forM_ files $ \file -> do
    let filePath = dir </> file
    removeFile filePath

    -- let mmm = fromLists [[9%2,8%3],[10%4,7%4]] :: Matrix (Ratio Integer)
    -- let rat = 10%3

    -- let res = V.zipWith (-) (getCol 1 mmm) (getCol 2 mmm)

    -- print $ res
    -- let m = fromLists [[0, -1, 2],
    --          [2, 0, -2],
    --          [-1, 1, 0]]
    -- let q = col [-3, 6, -1]

    -- let res = lemke (LCPProblem m q)

    -- print res

    -- let mm = fromLists [[-1, 0, 2, -1],
    --           [0, 1, 1, 4],
    --           [-2, -1, 0, 0],
    --           [1, -4, 0, 0]]
    -- let qq = col [1/2,- (1/2), 6,6]

    -- let res2 = lemke (LCPProblem mm qq)

    -- print res2

    -- let m2 = fromLists [[1, -1],
    --                     [-1, 1]]
    -- let q2 = col [1, -1]

    -- let res3 = lemke (LCPProblem m2 q2)

    -- print res3


    -- let m3 = fromLists [[-1, -1, 1],
    --                     [1, 1, 0],
    --                     [1, 1, 1]]
    -- let q3 = col [0, -2, -3]

    -- let res3 = lemke (LCPProblem m3 q3)

    -- print res3

    -- gen <- newStdGen
    -- let (betaL, gen1) = randomR (0.1, 0.9) gen

    -- let betaR = 1.0 - betaL
    -- betaL <- getStdRandom (randomR (0.1,0.9))

    -- let ar = M.fromLists [[0, 0, 0],
    --                    [0, 0, 0],
    --                    [1-5, 0, 0],
    --                    [0, negate 5, 2-5],
    --                    [0, 2-5, negate 5]] :: M.Matrix Rational

    -- print $ M.getRow 1 ar

    let a = fromLists [[0, 0, 0],
                       [0, 0, 0],
                       [1-5, 0, 0],
                       [0, negate 5, 2-5],
                       [0, 2-5, negate 5]] :: Matrix Double
    let b = fromLists [[0, 0, 0],
                       [0, 0, 0],
                       [1-5, 0, 0],
                       [0, negate 5, 4-5],
                       [0, 4-5, negate 5]] :: Matrix Double
    let e = fromLists [[1, 0, 0, 0, 0],
                       [-1, 1, 1, 0, 0],
                       [0, -1, 0, 1, 1]] :: Matrix Double
    let f = fromLists [[1, 0, 0],
                       [-1, 1, 1]] :: Matrix Double

    let seqf = SequenceForm2P a b e f

    let lcp = buildLCPProblem seqf

    let result = lemke lcp
    print result
    -- resultPerf <- replicateM 10 (generateAndSolveLCP seqf)

    -- print $ map roundV $ nubBy (\x y -> norm_2 (x - y) < 1e-8) resultPerf

    -- let a = fromLists [[0, 0, 0],
    --                    [0, 0, 0],
    --                    [2-5, 0, 0],
    --                    [0, 4-5, 0-5],
    --                    [0, 1-5, 0-5]] :: Matrix Double
    -- let b = fromLists [[0, 0, 0],
    --                    [0, 0, 0],
    --                    [2-5, 0, 0],
    --                    [0, 1-5, 0-5],
    --                    [0, 0-5, 1-5]] :: Matrix Double
    -- let e = fromLists [[1, 0, 0, 0, 0],
    --                    [-1, 1, 1, 0, 0],
    --                    [0, -1, 0, 1, 1]] :: Matrix Double
    -- let f = fromLists [[1, 0, 0],
    --                    [-1, 1, 1]] :: Matrix Double

    -- let seqf = SequenceForm2P a b e f

    -- let lcp = buildLCPProblem seqf

    -- let result = lemke lcp

    -- resultPerf <- replicateM 10 (generateAndSolveLCP seqf)

    -- print $ map roundV $ nubBy (\x y -> norm_2 (x - y) < 1e-8) resultPerf


    -- let vec12 = [1, 1/2, 1/2, 1/4, 1/4] :: [Double]
    -- let vec22 = [1, 1/5, 4/5] :: [Double]



    -- print $ M.getRow 1 ar

    -- let a = fromLists [[0, 0, 0, 0, 0],
    --                    [0, 11-13, 3-13, 0, 0],
    --                    [0, 0, 0, 0, 0],
    --                    [0, 0-13, 0-13, 0-13, 12-13],
    --                    [0, 0, 0, 6-13, 0-13]] :: Matrix Double
    -- let b = fromLists [[0, 0, 0, 0, 0],
    --                    [0, 3-13, 0-13, 0, 0],
    --                    [0, 0, 0, 0, 0],
    --                    [0, 0-13, 5-13, 2-13, 0-13],
    --                    [0, 0, 0, 0-13, 1-13]] :: Matrix Double
    -- let e = fromLists [[1, 0, 0, 0, 0],
    --                    [-1, 1, 1, 0, 0],
    --                    [0, 0, -1, 1, 1]] :: Matrix Double
    -- let f = fromLists [[1, 0, 0, 0, 0],
    --                    [-1, 1, 1, 0, 0],
    --                    [-1, 0, 0, 1, 1]] :: Matrix Double

    -- let seqf = SequenceForm2P a b e f

    -- let lcp = buildLCPProblem seqf

    -- let result = lemke lcp

---- Latest
    -- let a = fromLists [[0, 0, 0],
    --                     [0, 0, 0],
    --                     [0, 0, 0],
    --                     [0, 277.056 - 1300, 277.056 - 1300],
    --                     [0, 1229.44 - 1300, 1229.44 - 1300],
    --                     [0, 1133.23 - 1300, 1133.23 - 1300],
    --                     [0, 180.852 - 1300, 180.852 - 1300]] :: Matrix Double
    -- let b = fromLists [[0, 0, 0],
    --                    [0, 0, 0],
    --                    [0, 0, 0],
    --                    [0, 220.238 - 1300, 172.161 - 1300],
    --                    [0, 220.238 - 1300, 172.161 - 1300],
    --                    [0, 220.238 - 1300, 172.161 - 1300],
    --                    [0, 220.238 - 1300, 172.161 - 1300]] :: Matrix Double
    -- let e = fromLists [[1, 0, 0, 0, 0, 0, 0],
    --                    [-1, 1, 1, 0, 0, 0, 0],
    --                    [0, -1, 0, 1, 1, 0, 0],
    --                     [0, 0, -1, 0, 0, 1, 1]] :: Matrix Double
    -- let f = fromLists [[1, 0, 0],
    --                    [-1, 1, 1]] :: Matrix Double

    -- let seqf = SequenceForm2P a b e f

    -- -- lcpss <- generateAndSolveLCP seqf

    -- let lcp = buildLCPProblem seqf
    -- let result = lemke lcp

    -- print result
    -- -- print lcpss

    -- lcpPerfect <- buildLCPProblemPerfect seqf
    -- print $ lemke lcpPerfect
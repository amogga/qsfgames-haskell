module Example.Equilibrium.Payoffs where
import Example.Type
import Example.Utilities

-- The function computes the state (with probability of being reached) as well as the expected payoffs (for pure and mixed strategies)
-- TODO: make array as tuple to avoid incomplete patterns
incrementStatePayoffGeneral :: MultiRoundConfiguration -> EquilibriumTranslation -> [Rational] -> StatePayoffWithProbability
incrementStatePayoffGeneral (MultiRoundConfiguration _ _ objs stateUpdate) (eqp1,eqp2) st = (allStates, payoffAsDouble totPayoff) 
    where
        pfFromState = payoffFromState (fst objs) (snd objs)
        folRes v1 = let [mm1,mm2] = map fst v1
                        [pr1,pr2] = map snd v1
                    in foldl
                        ( \((state, _), pf) (s1, s2) ->
                            let newState1 = stateUpdate 1 s1 state
                                newState2 = stateUpdate 2 s2 newState1
                                newPayoff1 = probMulTuple3 pr1 pr2 $ pfFromState newState1
                                newPayoff2 = probMulTuple3 pr1 pr2 $ pfFromState newState2
                             in ((newState2, fromRational $ pr1 * pr2), sumTuple3 pf newPayoff1 newPayoff2)
                        )
                        ((st,1), (0, 0)) $ zip mm1 mm2

        -- print$ map snd $ head $ sequence [eqp1,eqp2] 
        stPf = map folRes $ sequence [eqp1,eqp2]

        allStates = map fst stPf
        totPayoff = foldl1 (sumTuple) $ map snd stPf
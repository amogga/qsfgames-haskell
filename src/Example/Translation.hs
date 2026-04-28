module Example.Translation
    ( translateEquilibrium
    , translateEquilibriumCompact
    ) where

import Data.Bifunctor (Bifunctor(bimap))
import Example.Utilities (sequenceCombinations)

-- translate equilibrium from sequence form to strategy notation
translateEquilibrium :: [Char] -> [Char] -> ([Rational],[Rational]) -> ([([Char], Rational)], [([Char], Rational)])
translateEquilibrium player1Strategies player2Strategies (eqp1,eqp2) = (strategySeqs eqp1 player1Strategies, strategySeqs eqp2 player2Strategies)
    where
        strategySeqs eqp strs = filter (\(_,flag) -> not (flag == 0)) $ zip (sequenceCombinations $ repeat strs) (tail eqp)

-- compact translation: returns Right if pure strategy, Left if mixed
translateEquilibriumCompact :: [Char] -> [Char] -> ([Rational],[Rational]) -> Either ([([Char], Rational)], [([Char], Rational)]) ([Char], [Char])
translateEquilibriumCompact player1Strategies player2Strategies eqp = if isAll1 fullForm
                                                                    then Right (bimap (fst . last) (fst . last) fullForm)
                                                                    else Left fullForm
    where
        fullForm = translateEquilibrium player1Strategies player2Strategies eqp
        isAll1 eqp' = all (== 1) $ concatMap (map snd) [fst eqp', snd eqp']

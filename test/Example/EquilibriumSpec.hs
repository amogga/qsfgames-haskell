module Example.EquilibriumSpec where

import Test.Hspec
import Example.Type
import Example.Equilibrium.Payoffs
import Example.Equilibrium.Translation
import Data.Ratio
import Simulation (scenarioConfig)

spec :: Spec
spec = do
    describe "Example.Equilibrium" $ do
        it "imports Example1 scenarioConfig" $ do
            length (moves scenarioConfig) `shouldBe` 2

        it "Equilibrium Translation" $ do
            let eqp = translateEquilibrium ['s','l'] ['s','l'] ([1,1,0,1,0,0,0],[1,0,1,0,0,1,0])
            eqp `shouldBe` ([("ss",1 % 1)],[("ls",1 % 1)])

            let eqp = translateEquilibrium ['s','l'] ['s','l'] ([1,1,0,0,1,0,0],[1,0,1,0,0,0,1])
            eqp `shouldBe` ([("sl",1 % 1)],[("ll",1 % 1)])

            let eqp = translateEquilibrium ['s','l'] ['s','l'] ([1,1,0,1,0,0,0],[1,0,1,0,0,1,0])
            eqp `shouldBe` ([("ss",1 % 1)],[("ls",1 % 1)])

            let eqp = translateEquilibrium ['s','l'] ['s','l'] ([1,1,0,0,1,0,0],[1,1 % 2,1 % 2,1 % 2,1 % 2,1 % 2,1 % 2])
            eqp `shouldBe` ([("sl", 1 % 1)], [("ss", 1 % 2), ("sl", 1 % 2), ("ls", 1 % 2), ("ll", 1 % 2)])

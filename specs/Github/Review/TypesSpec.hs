{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Github.Review.TypesSpec (spec) where


import Control.Error
import Control.Monad (unless)
import Data.Default
import Github.Data
import Github.Review
import Test.Hspec
import Test.HUnit
import Test.QuickCheck


spec :: Spec
spec =
    describe "Github.Review.Types" $ do
        describe "retry" $ do
            it "should retry the action 5 times." $ do
                (out :: Either Error Int, log) <- runGithubInteraction 4 4 False 50 $
                    retry . logIO "logging" $ return . Left $ UserError "nope"
                unless (length log == 5) $
                    assertFailure $ show out ++ "\t" ++ show log



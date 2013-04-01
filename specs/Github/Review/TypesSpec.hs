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

        describe "accum" $ do
            it "should allow the computation to fail 5 times." $ do
                (out :: Either Error Int, log) <- runGithubInteraction 4 4 False 50 $ do
                    accum 1 . logIO "aaa" . return . Left $ UserError "aaa"
                    accum 2 . logIO "bbb" . return . Left $ UserError "bbb"
                    accum 3 . logIO "ccc" . return . Left $ UserError "ccc"
                    accum 4 . logIO "ddd" . return . Left $ UserError "ddd"
                    accum 5 . logIO "eee" . return . Left $ UserError "eee"
                    accum 6 . logIO "fff" . return . Left $ UserError "fff"
                case out of
                    Left (UserError "eee") -> return ()
                    _ -> assertFailure $ show out ++ "\t" ++ show log


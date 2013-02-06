module Github.Review.FiltersSpec (spec) where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Time
import Github.Data
import Github.Review
import Github.Review.Arbitrary ()
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
    describe "Github.Review.Filters" $ do
        describe "sortByCommitDate" $
            it "should sort the earliest item first." $ property $ \cs ->
                case sortByCommitDate cs of
                    (c:_) -> (commitSha c ==)
                           . commitSha
                           $ maximumBy (comparing getCommitDate) cs
                    []    -> True
        describe "offsetByDays" $ do
            it "should subtract one day." $ 
                let from = UTCTime (ModifiedJulianDay 56318)
                                   (secondsToDiffTime 8993)
                    off  = offsetByDays (-1) from
                in  show off == "2013-01-25 02:29:53 UTC"
            it "should substract two days." $
                let from = UTCTime (ModifiedJulianDay 56318)
                                   (secondsToDiffTime 8993)
                    off  = offsetByDays (-2) from
                in  show off == "2013-01-24 02:29:53 UTC"
            it "should add a day." $
                let from = UTCTime (ModifiedJulianDay 56318)
                                   (secondsToDiffTime 8993)
                    off  = offsetByDays 1 from
                in  show off == "2013-01-27 02:29:53 UTC"
            it "should add a week" $
                let from = UTCTime (ModifiedJulianDay 56318)
                                   (secondsToDiffTime 8993)
                    off  = offsetByDays 7 from
                in  show off == "2013-02-02 02:29:53 UTC"


module Github.Review.Filters
    ( sortByCommitDate
    , getCommitDate
    , ascending
    , descending
    , offsetByDays
    , after
    , before
    , spanAfter
    , getAfterOrMinimum
    ) where

import Data.Time
import Github.Data

sortByCommitDate :: [Commit] -> [Commit]
sortByCommitDate = undefined

getCommitDate :: Commit -> UTCTime
getCommitDate = undefined

ascending :: (a -> b -> c) -> a -> b -> c
ascending = undefined

descending :: (a -> b -> c) -> b -> a -> c
descending = undefined

offsetByDays :: Integer -> IO UTCTime
offsetByDays = undefined

after :: UTCTime -> UTCTime -> Bool
after = undefined

before :: UTCTime -> UTCTime -> Bool
before = undefined

spanAfter :: (a -> UTCTime) -> UTCTime -> [a] -> ([a], [a])
spanAfter = undefined

getAfterOrMinimum :: (a -> UTCTime) -> UTCTime -> Int -> [a] -> [a]
getAfterOrMinimum = undefined


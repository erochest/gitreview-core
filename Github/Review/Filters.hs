
module Github.Review.Filters
    ( sortByCommitDate
    , getCommitDate
    , ascending
    , descending
    , offsetByDays
    , spanAfter
    , getAfterOrMinimum
    ) where


import Data.List (sortBy)
import Data.Ord
import Data.Time
import Github.Data
import Github.Review.Types


sortByCommitDate :: [RepoCommit] -> [RepoCommit]
sortByCommitDate = sortBy (descending (comparing (getCommitDate . snd)))

getCommitDate :: Commit -> UTCTime
getCommitDate = fromGithubDate
              . gitUserDate
              . gitCommitCommitter
              . commitGitCommit

ascending :: (a -> b -> c) -> a -> b -> c
ascending = id

descending :: (a -> b -> c) -> b -> a -> c
descending = flip

offsetByDays :: Integer -> UTCTime -> UTCTime
offsetByDays days from =
    fromInteger (days * 60 * 60 * 24) `addUTCTime` from

spanAfter :: (a -> UTCTime) -> UTCTime -> [a] -> ([a], [a])
spanAfter getter breakOn = span ((breakOn <=) . getter)

getAfterOrMinimum :: (a -> UTCTime) -> UTCTime -> Int -> [a] -> [a]
getAfterOrMinimum getter breakOn minLength xs =
        if length after >= minLength
            then after
            else take minLength xs
    where after = fst $ spanAfter getter breakOn xs


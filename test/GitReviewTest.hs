{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Applicative
import           Control.Error
import           Data.Monoid
import qualified Data.Text as T
import           Data.Time
import           Github.Data
import           Github.Review
import           Text.Printf

org :: GithubAccount
org = GithubOrgName "scholarslab"

samplePeriod :: Integer
samplePeriod = 1

shortLine :: Commit -> String
shortLine Commit{..} =
        let GitCommit{..} = commitGitCommit
            sha           = take 8 commitSha
            commitDate    = show
                          . fromGithubDate
                          $ gitUserDate gitCommitCommitter
            commitMessage = fromMaybe "<no commit message>"
                          . listToMaybe
                          $ lines gitCommitMessage
        in  printf "%s [%s] %s <%s>" sha commitDate commitMessage commitUrl

main :: IO ()
main = do
        item <- runGithubInteraction $ do
            repos      <- getAccountRepos org
            limit      <- liftIO $ offsetByDays samplePeriod <$> getCurrentTime
            allCommits <-  sortByCommitDate . concat
                       <$> mapM getRepoCommits repos
            liftIO $ putStrLn "Add commits."
            liftIO $ mapM_ (putStrLn . shortLine) allCommits
            let limited = getAfterOrMinimum getCommitDate limit 10 allCommits
            liftIO $ putStrLn "\nShort list."
            liftIO $ mapM_ (putStrLn . shortLine) limited
            bimapEitherT (UserError . T.unpack) id $ pickRandom limited
        case item of
            Right x -> putStrLn (shortLine x) >> print x
            Left e  -> putStrLn $ "ERROR: " <> show e


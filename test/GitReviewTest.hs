{-# LANGUAGE RecordWildCards #-}

-- | This assumes environment variables for the Github authentication
-- information, `GITHUB_USER` and `GITHUB_PASSWD`.

module Main where

import           Control.Applicative
import           Control.Error
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid
import qualified Data.Text as T
import           Data.Time
import           Github.Api
import           Github.Data
import           Github.Review
import           Network.URI
import           System.Environment
import           Text.Printf

org :: GithubAccount
org = GithubOrgName "scholarslab"

samplePeriod :: Integer
samplePeriod = 1

sampleMin :: Int
sampleMin = 10

shortLine :: RepoCommit -> String
shortLine (Repo{..}, Commit{..}) =
        let GitCommit{..} = commitGitCommit
            sha           = take 8 commitSha
            commitDate    = show
                          . fromGithubDate
                          $ gitUserDate gitCommitCommitter
            commitMessage = fromMaybe "<no commit message>"
                          . listToMaybe
                          $ lines gitCommitMessage
            url           = maybe "<invalid URI>" (show . toGithubUri)
                          $ parseURI commitUrl
        in  printf "%s/%s: %s [%s] %s <%s>"
                   (githubOwnerLogin repoOwner) repoName sha commitDate
                   commitMessage url

main :: IO ()
main = do
        user   <- getEnv "GITHUB_USER"
        passwd <- getEnv "GITHUB_PASSWD"
        let auth       = GithubBasicAuth (BS.pack user) (BS.pack passwd)
            getCommits = getAllRepoCommits' (Just auth)

        item   <- runGithubInteraction $ do
            repos      <- getAccountRepos org
            limit      <- liftIO $ offsetByDays samplePeriod <$> getCurrentTime
            allCommits <-  sortByCommitDate . concat
                       <$> mapM (`getCommits` sampleMin) repos

            liftIO $  putStrLn "Add commits."
                   >> mapM_ (putStrLn . shortLine) allCommits
            let limited = getAfterOrMinimum (getCommitDate . snd) limit
                                            sampleMin allCommits

            liftIO $  putStrLn "\nShort list."
                   >> mapM_ (putStrLn . shortLine) limited
            bimapEitherT (UserError . T.unpack) id $ pickRandom limited

        case item of
            Right x -> putStrLn "" >> putStrLn (shortLine x) >> print x
            Left e  -> putStrLn $ "ERROR: " <> show e


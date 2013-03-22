{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Github.Review.Access
    ( getAccountRepos
    , getRepoCommits
    , getRepoBranches
    , getRepoBranches'
    , branchesFor'
    , getBranchCommits
    , getBranchCommits'
    , getAllRepoCommits
    , getAllRepoCommits'
    , pickRandom
    ) where

import           Data.Hashable
import qualified Data.HashMap.Strict as M
import           Data.Monoid hiding (All)
import qualified Data.Text as T
import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Error
import           Github.Api
import           Github.Data
import           Github.Repos
import           Github.Repos.Commits
import           Github.Review.Types
import           System.Random.MWC


maybePause :: Maybe Int -> IO ()
maybePause = maybe (return ()) threadDelay

getAccountRepos :: GithubAccount -> GithubInteraction [Repo]
getAccountRepos (GithubUserName name) =
        retry (logIO ("userRepos " <> T.pack name) $ userRepos name All)
getAccountRepos (GithubOrgName name)  =
        retry (logIO ("orgnaizationRepos " <> T.pack name) $ organizationRepos name)

getRepoCommits :: Repo -> GithubInteraction [Commit]
getRepoCommits (Repo{..}) =
        retry (logIO task $ commitsFor (githubOwnerLogin repoOwner) repoName)
        where task = "commitsFor " <> T.pack repoName

getRepoBranches :: Maybe Int -> Repo -> GithubInteraction [Branch]
getRepoBranches = getRepoBranches' Nothing

getRepoBranches' :: Maybe GithubAuth -> Maybe Int -> Repo -> GithubInteraction [Branch]
getRepoBranches' auth pause Repo{..} =
        retry (logIO task $ branchesFor' auth pause (githubOwnerLogin repoOwner) repoName)
        where task = "branchesFor " <> T.pack repoName

branchesFor' :: Maybe GithubAuth
             -> Maybe Int
             -> String
             -> String
             -> IO (Either Error [Branch])
branchesFor' auth pause userName repoName =
        githubGet' auth ["repos", userName, repoName, "branches"] <* maybePause pause

getBranchCommits :: Maybe Int -> Repo -> Branch -> Int -> GithubInteraction [Commit]
getBranchCommits = getBranchCommits' Nothing

getBranchCommits' :: Maybe GithubAuth
                  -> Maybe Int
                  -> Repo
                  -> Branch
                  -> Int
                  -> GithubInteraction [Commit]
getBranchCommits' auth pause Repo{..} Branch{..} pageSize =
        retry (logIO task (getCommits <* maybePause pause))
        where user       = githubOwnerLogin repoOwner
              getCommits = githubGetWithQueryString'
                                    auth ["repos", user, repoName, "commits"]
                         $    "sha=" <> branchCommitSha branchCommit
                           <> "&per_page=" <> show pageSize
              task       = "getCommits " <> T.pack repoName <> "/" <> T.pack branchName

getAllRepoCommits :: Maybe Int -> Repo -> Int -> GithubInteraction [RepoCommit]
getAllRepoCommits = getAllRepoCommits' Nothing

getAllRepoCommits' :: Maybe GithubAuth
                   -> Maybe Int
                   -> Repo
                   -> Int
                   -> GithubInteraction [RepoCommit]
getAllRepoCommits' auth pause repo branchPageSize =
        map (repo,) . uniquifyOn commitSha . concat <$>
                    (mapM getbc =<< getRepoBranches' auth pause repo)
        where getbc = flip (getBranchCommits' auth pause repo) branchPageSize

uniquifyOn :: (Eq k, Hashable k) => (a -> k) -> [a] -> [a]
uniquifyOn keyFn xs = M.elems . M.fromList $ map toPair xs
        where toPair x = (keyFn x, x)

pickRandom :: [a] -> EitherT T.Text IO a
pickRandom [] = left "No items to pick a random element from."
pickRandom xs = do
        n <- fmapLT textShow
           . tryIO . withSystemRandom . asGenST
           $ uniformR (0, length xs - 1)
        hoistEither $ atErr ("Invalid random: " <> textShow n) xs n

textShow :: Show a => a -> T.Text
textShow = T.pack . show


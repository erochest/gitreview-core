{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Control.Error
import           Github.Api
import           Github.Data
import           Github.Repos
import           Github.Repos.Commits
import           Github.Review.Types
import           System.Random.MWC

getAccountRepos :: GithubAccount -> GithubInteraction [Repo]
getAccountRepos (GithubUserName name) = hoistGH $ userRepos name All
getAccountRepos (GithubOrgName name)  = hoistGH $ organizationRepos name

getRepoCommits :: Repo -> GithubInteraction [Commit]
getRepoCommits (Repo{..}) =
        hoistGH $ commitsFor (githubOwnerLogin repoOwner) repoName

getRepoBranches :: Repo -> GithubInteraction [Branch]
getRepoBranches = getRepoBranches' Nothing

getRepoBranches' :: Maybe GithubAuth -> Repo -> GithubInteraction [Branch]
getRepoBranches' auth Repo{..} =
        hoistGH $ branchesFor' auth (githubOwnerLogin repoOwner) repoName

branchesFor' :: Maybe GithubAuth
             -> String
             -> String
             -> IO (Either Error [Branch])
branchesFor' auth userName repoName =
        githubGet' auth ["repos", userName, repoName, "branches"]

getBranchCommits :: Repo -> Branch -> Int -> GithubInteraction [Commit]
getBranchCommits = getBranchCommits' Nothing

getBranchCommits' :: Maybe GithubAuth
                  -> Repo
                  -> Branch
                  -> Int
                  -> GithubInteraction [Commit]
getBranchCommits' auth Repo{..} Branch{..} pageSize =
        hoistGH . githubGetWithQueryString'
                            auth ["repos", user, repoName, "commits"]
                $    "sha=" <> branchCommitSha branchCommit
                  <> "&per_page=" <> show pageSize
        where user = githubOwnerLogin repoOwner

getAllRepoCommits :: Repo -> Int -> GithubInteraction [Commit]
getAllRepoCommits = getAllRepoCommits' Nothing

getAllRepoCommits' :: Maybe GithubAuth
                   -> Repo
                   -> Int
                   -> GithubInteraction [Commit]
getAllRepoCommits' auth repo branchPageSize =
        uniquifyOn commitSha . concat <$>
                    (mapM getbc =<< getRepoBranches' auth repo)
        where getbc = flip (getBranchCommits' auth repo) branchPageSize

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


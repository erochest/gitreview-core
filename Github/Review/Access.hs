{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Github.Review.Access
    ( getAccountRepos
    , getRepoCommits
    , getRepoBranches
    , getBranchCommits
    , getAllRepoCommits
    , pickRandom
    ) where

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
getRepoBranches Repo{..} =
        hoistGH $ branchesFor (githubOwnerLogin repoOwner) repoName

getBranchCommits :: Repo -> Branch -> Int -> GithubInteraction [Commit]
getBranchCommits Repo{..} Branch{..} pageSize =
        hoistGH . githubGetWithQueryString ["repos", user, repoName, "commits"]
                $    "sha=" <> branchCommitSha branchCommit
                  <> "&per_page=" <> show pageSize
        where user = githubOwnerLogin repoOwner

getAllRepoCommits :: Repo -> Int -> GithubInteraction [Commit]
getAllRepoCommits repo branchPageSize =
        concat <$> (mapM getBranchCommits' =<< getRepoBranches repo)
        where getBranchCommits' = flip (getBranchCommits repo) branchPageSize

pickRandom :: [a] -> EitherT T.Text IO a
pickRandom [] = left "No items to pick a random element from."
pickRandom xs = do
        n <- fmapLT textShow
           . tryIO . withSystemRandom . asGenST
           $ uniformR (0, length xs - 1)
        hoistEither $ atErr ("Invalid random: " <> textShow n) xs n

textShow :: Show a => a -> T.Text
textShow = T.pack . show


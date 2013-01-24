
module Github.Review.Access 
    ( getAccountRepos
    , getRepoCommits
    , pickRandomN
    ) where

import Control.Error
import Github.Data
import Github.Review.Types

getAccountRepos :: GithubAccount -> GithubInteraction [Repo]
getAccountRepos = undefined

getRepoCommits :: Repo -> GithubInteraction [Commit]
getRepoCommits = undefined

pickRandomN :: Int -> [a] -> EitherT String IO [a]
pickRandomN = undefined


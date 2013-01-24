
module Github.Review.Access 
    ( getAccountRepos
    , getRepoCommits
    ) where

import Github.Data
import Github.Review.Types

getAccountRepos :: GithubAccount -> GithubInteraction [Repo]
getAccountRepos = undefined

getRepoCommits :: Repo -> GithubInteraction [Commit]
getRepoCommits = undefined


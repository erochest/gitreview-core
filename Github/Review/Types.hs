
-- | This contains types for pulling a random commit out of Github, based on
-- some simple chronological properties. That's a complicated way of saying
-- that it picks a random, recent commit.

module Github.Review.Types
    ( GithubAccount(..)
    , GithubInteraction
    , RepoCommit
    , hoistGH
    )
    where

import           Control.Error
import           Control.Monad.Trans (liftIO)
import           Github.Data (Error(..), Repo, Commit)

data GithubAccount = GithubUserName String
                   | GithubOrgName String
                   deriving (Show, Eq)

type GithubInteraction = EitherT Error IO

type RepoCommit = (Repo, Commit)

hoistGH :: IO (Either Error a) -> GithubInteraction a
hoistGH = (hoistEither =<<) . liftIO


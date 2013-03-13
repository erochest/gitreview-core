
-- | This contains types for pulling a random commit out of Github, based on
-- some simple chronological properties. That's a complicated way of saying
-- that it picks a random, recent commit.

module Github.Review.Types
    ( GithubAccount(..)
    , GithubInteraction
    , RepoCommit
    , runGithubInteraction
    , hoistGH
    , hoistEitherT
    , lift
    , liftIO
    , logTask
    , logTasks
    , logIO
    )
    where

import           Control.Error
import           Control.Monad.Trans (liftIO)
import           Control.Monad.Writer
import           Data.DList
import qualified Data.Text as T
import           Github.Data (Error(..), Repo, Commit)

data GithubAccount = GithubUserName String
                   | GithubOrgName String
                   deriving (Show, Eq)

type TaskName = T.Text
type TaskList = DList TaskName
type GithubInteraction = EitherT Error (WriterT TaskList IO)

runGithubInteraction :: GithubInteraction a
                     -> IO (Either Error a, TaskList)
runGithubInteraction = runWriterT . runEitherT

type RepoCommit = (Repo, Commit)

hoistGH :: IO (Either Error a) -> GithubInteraction a
hoistGH = (hoistEither =<<) . lift . liftIO

hoistEitherT :: EitherT Error IO a -> GithubInteraction a
hoistEitherT = hoistGH . runEitherT

logTask :: TaskName -> GithubInteraction ()
logTask = lift . tell . singleton

logTasks :: [TaskName] -> GithubInteraction ()
logTasks = lift . tell . fromList

logIO :: TaskName -> IO (Either Error a) -> GithubInteraction a
logIO task a = logTask task >> hoistGH a


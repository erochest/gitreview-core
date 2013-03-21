{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This contains types for pulling a random commit out of Github, based on
-- some simple chronological properties. That's a complicated way of saying
-- that it picks a random, recent commit.

module Github.Review.Types
    ( RetryT
    , runRetryT
    , GithubAccount(..)
    , TaskName
    , TaskList
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

import           Control.Applicative
import           Control.Error
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Control.Retry
import           Data.DList
import           Data.Functor
import qualified Data.Text as T
import           Github.Data (Error(..), Repo, Commit)

data GithubAccount = GithubUserName String
                   | GithubOrgName String
                   deriving (Show, Eq)

type TaskName = T.Text
type TaskList = DList TaskName

-- | This runs the enclosed EitherT and retries according to RetrySettings
-- whenever it returns a Left value.
newtype RetryT e m a = Retrier
                     { runRetryT :: m (Either e a)
                     }

-- TODO: Add ReaderT RetrySettings to the stack.

bindRetry :: (Monad m, MonadIO m)
          => RetryT e m a
          -> (a -> RetryT e m b)
          -> RetryT e m b
x `bindRetry` f =
        Retrier $ do
            y <- retrying undefined isLeft $ runRetryT x
            case y of
                Right r  -> runRetryT $ f r
                (Left l) -> return $ Left l

insertRetry :: Monad m => a -> RetryT e m a
insertRetry = Retrier . return . Right

instance (Monad m, MonadIO m) => Monad (RetryT e m) where
        x >>= f = x `bindRetry` f
        return  = insertRetry

instance (Monad m, MonadIO m) => MonadIO (RetryT e m) where
        liftIO = lift . liftIO

instance MonadTrans (RetryT e) where
        lift = Retrier . (return . Right =<<)

instance Functor m => Functor (RetryT e m) where
        fmap f = Retrier . fmap (fmap f) . runRetryT

instance (Monad m, Functor m) => Applicative (RetryT e m) where
        pure    = Retrier . return . Right
        f <*> x = Retrier $ do
            f' <- runRetryT f
            x' <- runRetryT x
            return $ f' <*> x'

type GithubInteraction = RetryT Error (WriterT TaskList IO)

runGithubInteraction :: GithubInteraction a
                     -> IO (Either Error a, TaskList)
runGithubInteraction = runWriterT . runRetryT

type RepoCommit = (Repo, Commit)

hoistGH :: IO (Either Error a) -> GithubInteraction a
hoistGH = Retrier . lift . liftIO

hoistEitherT :: EitherT Error IO a -> GithubInteraction a
hoistEitherT = hoistGH . runEitherT

logTask :: TaskName -> GithubInteraction ()
logTask = lift . tell . singleton

logTasks :: [TaskName] -> GithubInteraction ()
logTasks = lift . tell . fromList

logIO :: TaskName -> IO (Either Error a) -> GithubInteraction a
logIO task a = logTask task >> hoistGH a


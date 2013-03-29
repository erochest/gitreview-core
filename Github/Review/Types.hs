{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RecordWildCards            #-}

-- | This contains types for pulling a random commit out of Github, based on
-- some simple chronological properties. That's a complicated way of saying
-- that it picks a random, recent commit.

module Github.Review.Types
    ( RetryT
    , runRetryT
    , execRetryT
    , retry
    , (>>=*)
    , (>>*)
    , GithubAccount(..)
    , TaskName
    , TaskList
    , GithubInteraction
    , RepoCommit
    , runGithubInteraction
    , insertEither
    , hoistGH
    , hoistEitherT
    , lift
    , liftIO
    , logTask
    , logTasks
    , logIO
    , ghUserError
    )
    where

import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Writer
import qualified Control.Retry as R
import           Data.DList
import           Data.Functor
import           Data.Monoid
import qualified Data.Text as T
import           Github.Data (Error(..), Repo, Commit)

data GithubAccount = GithubUserName String
                   | GithubOrgName String
                   deriving (Show, Eq)

type TaskName = T.Text
type TaskList = DList TaskName

data RetryTConfig = RetryTC
                  { retryAccumErrors :: Int
                  , retrySettings    :: R.RetrySettings
                  }

accumErrors :: Lens RetryTConfig RetryTConfig Int Int
accumErrors = lens retryAccumErrors $ \r e -> r { retryAccumErrors = e }

numRetries :: Lens RetryTConfig RetryTConfig R.RetryLimit R.RetryLimit
numRetries =
        lens (R.numRetries . retrySettings) $ \r n ->
            r { retrySettings = (retrySettings r) { R.numRetries = n } }

backoff :: Lens RetryTConfig RetryTConfig Bool Bool
backoff =
        lens (R.backoff . retrySettings) $ \r b ->
            r { retrySettings = (retrySettings r) { R.backoff = b } }

baseDelay :: Lens RetryTConfig RetryTConfig Int Int
baseDelay =
        lens (R.baseDelay . retrySettings) $ \r b ->
            r { retrySettings = (retrySettings r) { R.baseDelay = b } }

data AccumErrors e = AccumErrors
                   { accumErrorCount :: Sum Int
                   , accumErrorList  :: DList e
                   }

errorCount :: Lens (AccumErrors e) (AccumErrors e) Int Int
errorCount =
        lens (getSum . accumErrorCount) $ \a c ->
            a { accumErrorCount = Sum c }

errorList :: Lens (AccumErrors e) (AccumErrors e) [e] [e]
errorList =
        lens (toList . accumErrorList) $ \a l ->
            a { accumErrorList = fromList l }

instance Monoid (AccumErrors e) where
        mempty      = AccumErrors mempty mempty
        mappend a b = AccumErrors (accumErrorCount a <> accumErrorCount b)
                                  (accumErrorList a  <> accumErrorList b)

singletonError :: e -> AccumErrors e
singletonError = AccumErrors (Sum 1) . singleton

appendError :: AccumErrors e -> e -> AccumErrors e
appendError AccumErrors{..} e =
        AccumErrors (Sum . (1+) $ getSum accumErrorCount)
                    (Data.DList.snoc accumErrorList e)

-- | This runs the enclosed EitherT and retries according to RetrySettings
-- whenever it returns a Left value.
newtype RetryT e m a = Retrier
                     { runRetryT :: ReaderT RetryTConfig (EitherT e m) a
                     }

execRetryT :: RetryTConfig -> RetryT e m a -> m (Either e a)
execRetryT s = runEitherT . flip runReaderT s . runRetryT

bindRetry :: Monad m
          => RetryT e m a
          -> (a -> RetryT e m b)
          -> RetryT e m b
x `bindRetry` f = Retrier $ runRetryT . f =<< runRetryT x

retry :: (Monad m, MonadIO m) => RetryT e m b -> RetryT e m b
retry m = Retrier $ ReaderT $ \s@RetryTC{..} ->
           EitherT
         . R.retrying retrySettings isLeft
         $ runEitherT (runReaderT (runRetryT m) s)

(>>=*) :: (Monad m, MonadIO m)
       => RetryT e m a -> (a -> RetryT e m b) -> RetryT e m b
x >>=* f =
        Retrier $ ReaderT $ \r@RetryTC{..} -> do
            x' <- runReaderT (runRetryT x) r
            EitherT .
                R.retrying retrySettings isLeft $
                runEitherT (runReaderT (runRetryT (f x')) r)

(>>*) :: (Monad m, MonadIO m) => RetryT e m a -> RetryT e m b -> RetryT e m b
x >>* y = x >>=* const y

insertRetry :: Monad m => a -> RetryT e m a
insertRetry = Retrier . return

insertEither :: (Monad m) => Either e a -> RetryT e m a
insertEither = Retrier . lift . hoistEither

instance (Monad m, MonadIO m) => Monad (RetryT e m) where
        x >>= f = x `bindRetry` f
        return  = insertRetry

instance (Monad m, MonadIO m) => MonadIO (RetryT e m) where
        liftIO = lift . liftIO

instance MonadTrans (RetryT e) where
        lift = Retrier . lift . lift

instance (Functor m, Monad m) => Functor (RetryT e m) where
        fmap f = Retrier . fmap f . runRetryT

instance (Monad m, Functor m) => Applicative (RetryT e m) where
        pure    = Retrier . pure
        f <*> x = Retrier $ do
            f' <- runRetryT f
            x' <- runRetryT x
            return $ f' x'

type GithubInteraction = RetryT Error (WriterT TaskList IO)

runGithubInteraction :: Int -> Int -> Bool -> Int -> GithubInteraction a
                     -> IO (Either Error a, TaskList)
runGithubInteraction accumErrors numRetries backoff baseDelay =
        runWriterT . execRetryT settings
        where settings = RetryTC accumErrors
                                 $ R.RetrySettings (R.limitedRetries numRetries)
                                                   backoff baseDelay

type RepoCommit = (Repo, Commit)

hoistGH :: IO (Either Error a) -> GithubInteraction a
hoistGH action =
        insertEither =<< liftIO action

hoistEitherT :: EitherT Error IO a -> GithubInteraction a
hoistEitherT = hoistGH . runEitherT

logTask :: TaskName -> GithubInteraction ()
logTask = lift . tell . singleton

logTasks :: [TaskName] -> GithubInteraction ()
logTasks = lift . tell . fromList

logIO :: TaskName -> IO (Either Error a) -> GithubInteraction a
logIO task a = logTask task >> hoistGH a

ghUserError :: String -> GithubInteraction a
ghUserError = hoistEitherT . left . UserError


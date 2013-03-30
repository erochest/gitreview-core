{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RecordWildCards            #-}

-- | This contains types for pulling a random commit out of Github, based on
-- some simple chronological properties. That's a complicated way of saying
-- that it picks a random, recent commit.

module Github.Review.Types
    ( GithubAccount(..)
    , TaskName
    , TaskList
    , RepoCommit
    , GithubInteraction
    , evalGithubInteraction
    , GithubInteractionT(..)
    , evalGithubInteractionT
    , runGithubInteraction
    , retry
    , accum
    , reacc
    , (>>=*)
    , (>>*)
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
import           Control.Monad.RWS
import           Control.Monad.Trans
import qualified Control.Retry as R
import           Data.DList
import           Data.Functor
import           Data.Functor.Identity
import           Data.Monoid
import qualified Data.Text as T
import           Github.Data (Error(..), Repo, Commit)

data GithubAccount = GithubUserName String
                   | GithubOrgName String
                   deriving (Show, Eq)

type TaskName = T.Text
type TaskList = DList TaskName

type RepoCommit = (Repo, Commit)

data GitIntConfig = GitC
                  { retryAccumErrors :: Int
                  , retrySettings    :: R.RetrySettings
                  }

accumErrors :: Lens GitIntConfig GitIntConfig Int Int
accumErrors = lens retryAccumErrors $ \r e -> r { retryAccumErrors = e }

numRetries :: Lens GitIntConfig GitIntConfig R.RetryLimit R.RetryLimit
numRetries =
        lens (R.numRetries . retrySettings) $ \r n ->
            r { retrySettings = (retrySettings r) { R.numRetries = n } }

backoff :: Lens GitIntConfig GitIntConfig Bool Bool
backoff =
        lens (R.backoff . retrySettings) $ \r b ->
            r { retrySettings = (retrySettings r) { R.backoff = b } }

baseDelay :: Lens GitIntConfig GitIntConfig Int Int
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

type GithubInteraction = GithubInteractionT Error IO

-- | This runs the enclosed EitherT and retries according to RetrySettings
-- whenever it returns a Left value.
newtype GithubInteractionT e m a =
        GHT { runGHT :: EitherT e (RWST GitIntConfig TaskList (AccumErrors e) m) a
            }

evalGithubInteraction :: GithubInteraction a
                      -> GitIntConfig
                      -> IO (Either Error a, [TaskName])
evalGithubInteraction = evalGithubInteractionT

evalGithubInteractionT :: (Monad m, Functor m)
                       => GithubInteractionT e m a
                       -> GitIntConfig
                       -> m (Either e a, [TaskName])
evalGithubInteractionT gh c =
        fmap toList <$> evalRWST (runEitherT (runGHT gh)) c mempty

execGithubInteraction :: Monad m
                      => GithubInteractionT e m a
                      -> GitIntConfig
                      -> AccumErrors e
                      -> m (Either e a, TaskList)
execGithubInteraction = evalRWST . runEitherT . runGHT

bindRetry :: Monad m
          => GithubInteractionT e m a
          -> (a -> GithubInteractionT e m b)
          -> GithubInteractionT e m b
x `bindRetry` f = GHT $ runGHT . f =<< runGHT x

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

retry :: (Monad m, MonadIO m)
      => GithubInteractionT e m b
      -> GithubInteractionT e m b
retry m = GHT $ EitherT $ RWST $ \r s ->
    R.retrying (retrySettings r) (isLeft . fst3)
            $ runRWST (runEitherT (runGHT m)) r s

accum :: Monad m
      => a
      -> GithubInteractionT e m a
      -> GithubInteractionT e m a
accum def m = GHT $ EitherT $ RWST $ \r s -> do
    (a, s', w) <- runRWST (runEitherT (runGHT m)) r s
    let (a', s'') = accumError (view accumErrors r) def a s'
    return (a', s'', w)

reacc :: (Monad m, MonadIO m)
      => a
      -> GithubInteractionT e m a
      -> GithubInteractionT e m a
reacc def = accum def . retry

accumError :: Int -> a -> Either e a -> AccumErrors e
           -> (Either e a, AccumErrors e)
accumError _ _ r@(Right _) s = (r, s)
accumError maxAccumCount def l@(Left e) s@AccumErrors{..}
    | maxAccumCount > getSum accumErrorCount = (Right def, s')
    | otherwise                              = (l, s')
    where s' = appendError s e

(>>=*) :: (Monad m, MonadIO m)
       => GithubInteractionT e m a
       -> (a -> GithubInteractionT e m b)
       -> GithubInteractionT e m b
x >>=* f = do
        x' <- x
        retry $ f x'

(>>*) :: (Monad m, MonadIO m)
      => GithubInteractionT e m a
      -> GithubInteractionT e m b
      -> GithubInteractionT e m b
x >>* y = x >>=* const y

insertGH :: Monad m => a -> GithubInteractionT e m a
insertGH = GHT . return

insertEither :: (Monad m) => Either e a -> GithubInteractionT e m a
insertEither = GHT . EitherT . return
    where f x@Hole = undefined
        -- GHT . lift . hoistEither

instance (Monad m, MonadIO m) => Monad (GithubInteractionT e m) where
        x >>= f = x `bindRetry` f
        return  = insertGH

instance (Monad m, MonadIO m) => MonadIO (GithubInteractionT e m) where
        liftIO = lift . liftIO

instance MonadTrans (GithubInteractionT e) where
        lift = GHT . lift . lift

instance (Functor m, Monad m) => Functor (GithubInteractionT e m) where
        fmap f = GHT . fmap f . runGHT

instance (Monad m, Functor m) => Applicative (GithubInteractionT e m) where
        pure    = GHT . pure
        f <*> x = GHT $ do
            f' <- runGHT f
            x' <- runGHT x
            return $ f' x'

runGithubInteraction :: Int -> Int -> Bool -> Int -> GithubInteractionT e IO a
                     -> IO (Either e a, [TaskName])
runGithubInteraction accumErrors numRetries backoff baseDelay m =
    evalGithubInteractionT m $ GitC accumErrors
                                    $ R.RetrySettings (R.limitedRetries numRetries)
                                                      backoff baseDelay

hoistGH :: IO (Either Error a) -> GithubInteractionT Error IO a
hoistGH action =
        insertEither =<< liftIO action

hoistEitherT :: EitherT Error IO a -> GithubInteractionT Error IO a
hoistEitherT = hoistGH . runEitherT

logTask :: Monad m => TaskName -> GithubInteractionT e m ()
logTask = GHT . tell . singleton

logTasks :: Monad m => [TaskName] -> GithubInteractionT e m ()
logTasks = GHT . tell . fromList

logIO :: TaskName -> IO (Either Error a) -> GithubInteractionT Error IO a
logIO task a = logTask task >> hoistGH a

ghUserError :: String -> GithubInteractionT Error IO a
ghUserError = hoistEitherT . left . UserError

data Hole = Hole

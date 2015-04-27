module Control.Monad.TransT (

    TransT(..)
  , tlift
  , tfilt

  , CommuteM(..)

  ) where

import Control.Applicative
import Control.Monad
import Data.Functor.Identity

class CommuteM f where
  commuteM :: Monad m => f (m a) -> m (f a)

instance CommuteM Maybe where
  commuteM mebe = case mebe of
      Nothing -> return Nothing
      Just x -> liftM Just x

instance CommuteM (Either e) where
  commuteM sum = case sum of
      Left l -> return $ Left l
      Right r -> liftM Right r

instance CommuteM Identity where
  commuteM = (liftM Identity) . runIdentity

instance CommuteM [] where
  commuteM = sequence

newtype TransT m f a = TransT {
    runTransT :: m (f a)
  }

instance (Functor m, Functor f) => Functor (TransT m f) where
  fmap f trans = TransT $ (fmap . fmap) f (runTransT trans)

instance (Applicative m, Applicative f) => Applicative (TransT m f) where
  pure = TransT . pure . pure
  mf <*> mx = TransT $ (<*>) <$> runTransT mf <*> runTransT mx

instance (Functor m, Monad m, Functor f, Monad f, CommuteM f) => Monad (TransT m f) where
  return = TransT . return . return
  mx >>= k = TransT $ do
      x <- runTransT mx
      let step1 = fmap (runTransT . k) x
      let step2 = commuteM step1
      fmap join step2

-- | TBD is it OK to ignore f in this instance? What if it's an alternative
--   as well?
instance (Applicative f, Alternative m) => Alternative (TransT m f) where
  empty = TransT empty
  ta <|> tb = TransT $ (runTransT ta) <|> (runTransT tb)

instance (Functor m, Functor f, Monad f, CommuteM f, Monad m, MonadPlus m) => MonadPlus (TransT m f) where
  mzero = TransT mzero
  ta `mplus` tb = TransT $ (runTransT ta) `mplus` (runTransT tb)

tlift :: (Applicative f, Functor m) => m a -> TransT m f a
tlift term = TransT $ fmap pure term

tfilt :: Applicative m => f a -> TransT m f a
tfilt term = TransT $ pure term

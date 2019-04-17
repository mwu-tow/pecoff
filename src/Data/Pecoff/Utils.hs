module Data.Pecoff.Utils where

import Data.Binary (Get)
import Data.Binary.Get (runGet)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- | like 'runGet' but takes a strict 'ByteString'
runGet' :: Get a -> B.ByteString -> a
runGet' g bs = runGet g $ L.fromStrict bs

-- | Repeats action, until the result matches predicate. The final value is not
--   returned.
repeatUntil :: (Monad m) => m a -> (a -> Bool) -> m [a]
repeatUntil getter stopCondition = do
    value <- getter
    if stopCondition value
        then pure []
        else (value :) <$> repeatUntil getter stopCondition
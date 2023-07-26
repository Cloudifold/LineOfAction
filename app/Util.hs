module Util where
-- functions that should be in base, but aren't
-- also Bool should be Maybe ()

import Control.Monad

infix 0 ~>
(~>) :: Bool -> a -> Maybe a
False ~> _ = Nothing
True ~> x = Just x

(<?>) :: Maybe a -> b -> Either b a
Nothing <?> e = Left e
Just x <?> _ = Right x

getRight :: Either a b -> Maybe b
getRight (Left x) = Nothing
getRight (Right x) = Just x

doWhileM_ :: Monad m => m Bool -> m ()
doWhileM_ m = do
    b <- m
    when b $ doWhileM_ m

doUntilM :: Monad m => m (Maybe a) -> m a
doUntilM m = do
    a <- m
    case a of
        Nothing -> doUntilM m
        Just x -> return x
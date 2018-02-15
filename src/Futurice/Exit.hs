-- | See https://github.com/haskell/hackage-security/blob/master/hackage-security/src/Hackage/Security/Util/Exit.hs
module Futurice.Exit (
    ExitT,
    Exit,
    runExitT,
    runExit,
    exit,
    exitIfNothing,
    exitIfLeft,
    ) where

import Futurice.Prelude
import Prelude ()

type ExitT m a = ExceptT a m a
type Exit a    = Either a a

runExitT :: Functor m => ExitT m a -> m a
runExitT = fmap runExit . runExceptT

runExit :: Exit a -> a
runExit = either id id

exit :: MonadError a m => a -> m b
exit = throwError

exitIfNothing :: MonadError a m => Maybe b -> a -> m b
exitIfNothing x e = maybe (exit e) pure x

exitIfLeft :: MonadError a m => Either c b -> (c -> a) -> m b
exitIfLeft x f = either (exit . f) pure x

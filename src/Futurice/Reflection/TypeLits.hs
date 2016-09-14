{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.Reflection.TypeLits {-# DEPRECATED "Only needed for base <4.8" #-} (
    reifyTypeableSymbol,
    ) where

import Data.Proxy    (Proxy)
import Data.Typeable (Typeable)
import GHC.TypeLits

-- This could be for base-4.8.0, but we use it for testing
--
#if !MIN_VERSION_base(4,9,0)
import Data.Typeable.Internal (TyCon(..), TypeRep, mkTyConApp)
import GHC.Fingerprint
import GHC.Prim
import Unsafe.Coerce

symbolTypeRep :: KnownSymbol s => Proxy s -> TypeRep
symbolTypeRep p = mkTyConApp tc []
    where
    tc = TyCon
#if MIN_VERSION_base(4,8,0)
           { tyConFingerprint  = fingerprintString (mk pack modu nm)
#else
           { tyConHash         = fingerprintString (mk pack modu nm)
#endif
           , tyConPackage      = pack
           , tyConModule       = modu
           , tyConName         = nm
           }
    pack = "base"
    modu = "GHC.TypeLits"
    nm   = show (symbolVal p)
    mk a b c = a ++ " " ++ b ++ " " ++ c

newtype MagicTypeable (s :: Symbol) r = MagicTypeable (Typeable s => r)
newtype ReifiedTypeable s = ReifiedTypeable (Proxy# s -> TypeRep)

reifyTypeableSymbol :: forall s r. KnownSymbol s => Proxy s -> (Typeable s => r) -> r
reifyTypeableSymbol p k = unsafeCoerce (MagicTypeable k :: MagicTypeable s r) rtr
  where
    tr  = symbolTypeRep p
    rtr = ReifiedTypeable (\_ -> tr)

#else
-- | With @base >= 4.9@ the implementation is trivial.
reifyTypeableSymbol :: forall s r. KnownSymbol s => Proxy s -> (Typeable s => r) -> r
reifyTypeableSymbol _ f = f
#endif

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.Reflection.TypeLits (
    reifySymbolTypeable,
    ) where

import Data.Typeable.Internal
import GHC.Fingerprint
import GHC.Prim
import GHC.TypeLits
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

reifySymbolTypeable :: forall s r. KnownSymbol s => Proxy s -> (Typeable s => r) -> r
reifySymbolTypeable p k = unsafeCoerce (MagicTypeable k :: MagicTypeable s r) rtr
  where
    tr  = symbolTypeRep p
    rtr = ReifiedTypeable (\_ -> tr)

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | 'Unit1' has trivial instance! Like 'Data.Proxy.Proxy', but in kind
-- 'Data.Constraint.Constraint'.
module Futurice.Constraint.Unit1 where

import Futurice.Constraint.ForallSymbol

-- | Class with instances for all types.
class Unit1 a
instance Unit1 a
instance ForallFSymbol Unit1 b where
    instFSymbol = Dict

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
import IsInstance
import GHC.TypeLits

class C1 (x :: Symbol) r s | x r -> s

class C (xs :: [Symbol]) (r :: *) (v :: *) | xs r -> v

instance (C1 x r s, C xs s v) => C (x ': xs) r v


{-
-- generated code is rejected because the
-- "x r -> s" FD should be in the instance body
-- if C needs to be tested
instance (And q1 (And q2 True) ~ q,
          TryCxt (C1 x r s) q1,
          TryCxt (C xs s v) q2) =>
         TryCxt (C (x ': xs) r v) q
-}


writeTryCxt ''C

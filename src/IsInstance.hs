{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module IsInstance where

import Language.Haskell.TH
import GHC.Exts (Constraint)
import Control.Applicative
import Data.Proxy




type family ToCxt (a :: k) (b :: *) :: Constraint
type instance ToCxt f x = f x
type instance ToCxt (c ': cs) b = (ToCxt c b, ToCxt cs b)
type instance ToCxt '[] b = ()



data MInst c t where
    Yes :: ToCxt c t => t -> MInst c t
    No :: t -> MInst c t


class IsInstance (c :: k) t where
    {-# MINIMAL ifHasInstance | toMInst #-}
    ifHasInstance :: proxy c
          -> r -- ^ else
          -> (ToCxt c t => t -> r) -- ^ then
          -> t
          -> r
    ifHasInstance _ no yes t = case toMInst t :: MInst c t of
                                 No x -> no
                                 Yes x -> yes x
    toMInst :: t -> MInst c t
    toMInst t = ifHasInstance (Proxy :: Proxy c) (No t) Yes t


-- | else case
instance IsInstance (c :: * -> Constraint) b where
    ifHasInstance _ elseR _ _ = elseR
    toMInst = No

-- | collapse lists of constraints, so we can write:
--
-- > ifHasInstance '[Show,Num] n (\n' -> show (n' + 1)) "NO SHOW/NUM"
instance (IsInstance c t, IsInstance cs t) => IsInstance (c ': cs) t where
    ifHasInstance _ no yes = ifHasInstance pc no (ifHasInstance pcs no yes)
      where pc = Proxy :: Proxy c
            pcs = Proxy :: Proxy cs

instance IsInstance '[] b where
    ifHasInstance _ no yes t = yes t
    toMInst = Yes

writeIsInstance :: Name -> DecsQ
writeIsInstance className = do
    ClassI _ insts <- reify className
    -- ClassI _ instsDone <- reify ''IsInstance
    let doneTys = [] -- [ ty | InstanceD _ (_ `AppT` ty `AppT` _) _ <- instsDone ]
    sequence $ [ InstanceD cxt
                      <$> [t| IsInstance $(conT className) $(return ty) |] 
                      <*> [d| $(varP 'ifHasInstance) = \ _proxy _ f t -> f t
                              $(varP 'toMInst) = Yes |]
                        | InstanceD cxt (AppT _ ty) _ <- insts,
                          ty `notElem` doneTys ]

{- what do we do about instances like

instance (Show a, Show b) => Show (a,b)


instance (IsInstance Show a, IsInstance Show b) => IsInstance Show (a,b)


does not provide the required Show (a,b) constraint



class IsInstance' c a (b :: Bool)

class (IsInstance' Show a b1, IsInstance' Show b b2, And b1 b2 ~ bool) => IsInstance' Show (a,b) bool

but with the bool there, ambiguous types tend to happen since the FD isn't allowed



-}
{-
class IsInstance (c :: k) t (b :: Bool)

instance (f ~ False) => IsInstance (c :: * -> Constraint) t f


type family And (a :: Bool) (b :: Bool) :: Bool
type instance And True True = True
type instance And False b = b

instance (IsInstance c t b1, IsInstance cs t b2, b ~ And b1 b2) => IsInstance (c ': cs) t b
instance (b ~ True) => IsInstance '[] t b


writeIsInstance :: Name -> DecsQ
writeIsInstance className = do
    ClassI _ insts <- reify className
    -- ClassI _ instsDone <- reify ''IsInstance
    let doneTys = [] -- [ ty | InstanceD _ (_ `AppT` ty `AppT` _) _ <- instsDone ]
    true <- newName "true"
    trueT <- [t| True |]
    sequence $ [ InstanceD (EqualP (VarT true) trueT : cxt)
                      <$> [t| IsInstance $(conT className) $(return ty) $(varT true) |] 
                      <*> pure []
                        | InstanceD cxt (AppT _ ty) _ <- insts,
                          ty `notElem` doneTys ]



class IfHasInstance c t where
    ifHasInstance :: Proxy c 
        -> r
        -> (ToCxt c t => t -> r)
        -> t
        -> r 

class IfHasInstance' (b :: Bool) c t where
    ifHasInstance' :: Proxy b
        -> Proxy c
        -> r
        -> (ToCxt c t => t -> r)
        -> t
        -> r

instance (ToCxt c t) => IfHasInstance' True c t where
    ifHasInstance' _b _c _no yes t = yes t

instance IfHasInstance' False c t where
    ifHasInstance' _b _c no _yes _no = no


instance (IsInstance c t b, IfHasInstance' b c t) => IfHasInstance c t where
    ifHasInstance = ifHasInstance' (Proxy :: Proxy b)
    -}

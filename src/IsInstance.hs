{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 711
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
module IsInstance
  (ifHasInstance,
   whenHasInstance,
   writeTryCxt,
   
   -- * for type signatures
   TryCxt,
   GetDict(..),
   Dict(..),
   AndCxt,
  ) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.State
import Language.Haskell.TH
import GHC.Exts (Constraint)
import Control.Applicative
import Data.Proxy
import Control.Monad
import Data.List
import Data.Monoid

import Data.Char
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Generics

-- import Data.Constraint (Dict(Dict))
data Dict c where
    Dict :: c => Dict c

type family And (x :: Bool) (y :: Bool) :: Bool
type instance And True y = y
type instance And False y = False

-- | 
class TryCxt (c :: Constraint) (b :: Bool) | c -> b

-- | overlapping case: there is no instance
instance (False ~ false) => TryCxt c false

-- | @AndCxt :: (* -> Constraint) -> (* -> Constraint) -> (* -> Constraint)@
class (f x, g x) => AndCxt f g x

-- | the only instance
instance (f x, g x) => AndCxt f g x

-- | writeTryCxt can define this, but it would have to be in another
-- module, so just write it by hand
instance (TryCxt (f x) a, TryCxt (g x) b, ab ~ And a b)
    => TryCxt (AndCxt f g x) ab


-- | provides convenient notation for requiring one or many constraints for
-- 'ifHasInstance' / 'whenHasInstance':
--
-- > Proxy :: Proxy [Read,Show,Num]
-- > Proxy :: Proxy Num
--
-- An alternative would be to directly use a class ('AndCxt') that can make
-- a type of kind `* -> Constraint` from many types of that kind.
-- In other words, ToCxt could be avoided if people instead used
--
-- > Proxy :: Proxy (Read `AndCxt` Show `AndCxt` Num)
type family ToCxt (fs :: m) :: * -> Constraint
type instance ToCxt (f ': g ': fs) = AndCxt f (ToCxt (g ': fs))
type instance ToCxt '[f] = f
type instance ToCxt f = f


-- | getDict captures the type class dictionary for `c` when `b ~ True`
class GetDict_ (b :: Bool) (c :: Constraint) where
    getDict_ :: TryCxt c b => Proxy b -> Maybe (Dict c)

instance      GetDict_ False c where getDict_ _ = Nothing
instance c => GetDict_ True  c where getDict_ _ = Just Dict

-- | GetDict_ except it has a functional dependency | c -> b, due to
-- 'TryCxt'
type GetDict b c = (GetDict_ b c, TryCxt c b)

getDict :: forall b c. GetDict b c => Maybe (Dict c)
getDict = getDict_ (Proxy :: Proxy b)

ifHasInstance :: forall pc c t b r proxy. (GetDict b c, c ~ ToCxt pc t)
    => proxy pc -- ^ a type annotation is needed to specify which constraint neeeds
        -- to be satisfied. Valid kinds are
        --
        -- > Proxy [* -> Constraint]
        -- > Proxy (* -> Constraint)
        --
        -- > Proxy :: Proxy Show
        -- > Proxy :: Proxy '[Read, Show]
      -> r -- ^ result if TryCxt instances are missing (possibly because 'writeTryCxt' has not been called for the right classes)
      -> (c => t -> r) -- ^ then
      -> t
      -> r
ifHasInstance _ no yes = case getDict of
                             Nothing -> \_ -> no
                             Just (Dict :: Dict c) -> yes

whenHasInstance :: forall pc c t b r proxy. (GetDict b c, c ~ ToCxt pc t)
    => proxy pc
    -> (c => t -> r)
    -> t
    -> Maybe r
whenHasInstance _ yes = case getDict of
                             Nothing -> \_ -> Nothing
                             Just (Dict :: Dict c) -> Just . yes

data WriteTryCxtSt = WriteTryCxtSt {
  seenInstances :: S.Set InstanceDec,
  triedClasses :: S.Set Name
  } deriving Show

instance Monoid WriteTryCxtSt where
    mappend (WriteTryCxtSt a b) (WriteTryCxtSt a' b') =
      WriteTryCxtSt (a <> a') (b <> b')
    mempty = WriteTryCxtSt mempty mempty

filterTriedClasses x = do
    m <- get
    put (m { triedClasses = triedClasses m `S.union` x })
    return (x S.\\ triedClasses m)


writeTryCxt className = do
  s0 <- runIO (readIORef writeTryCxtStRef)  
  (r, s) <- writeTryCxt2 className `runStateT` s0 { triedClasses = S.insert className (triedClasses s0) }
  runIO (writeIORef writeTryCxtStRef s)
  return r

{-# NOINLINE writeTryCxtStRef #-}
writeTryCxtStRef :: IORef WriteTryCxtSt
writeTryCxtStRef = unsafePerformIO $ newIORef (WriteTryCxtSt mempty mempty) 

writeTryCxt2 :: Name -> StateT WriteTryCxtSt Q [Dec]
writeTryCxt2 className = do
    (dec, next) <- writeTryCxt1 className
    decs <- mapM writeTryCxt2 . S.toList =<< filterTriedClasses next
    return (dec ++ concat decs)


writeTryCxt1 :: Name -> StateT WriteTryCxtSt Q ([Dec], S.Set Name)
writeTryCxt1 className | c : _ <- nameBase className, isLower c = return mempty
writeTryCxt1 className = StateT $ \s -> do
    ClassI _ insts <- reify className
    ClassI _ existingInsts <- reify ''TryCxt
    let existingInstsSet = let b = S.fromList (map rename existingInsts) `S.union` seenInstances s
            in b
        keepInst inst = rename inst `S.notMember` existingInstsSet
        {- instance C X

          leads to
        
           instance TryCxt (C X) True
        -}
        noCxt hd = instanceD (return [])
              [t| TryCxt $(return hd) 'True |]
              []

        {- instance (A a, B a) => C a

          leads to
        
           instance (TryCxt (A a) q1, TryCxt (B a) q2, (q1 `And` q2) ~ r) => TryCxt (C a) r
        -}
        yesCxt cxt hd = do
          qs <- replicateM (length cxt) (newName "q")
          r <- newName "r"
          let andCxts :: [TypeQ] -> TypeQ
              andCxts = foldr (\ x xs -> [t| $x `And` $xs |] ) [t| 'True |]

              tentativeCxt = sequence $
                   [t| $(andCxts (map varT qs)) ~ $(varT r) |] :
                   [ [t| TryCxt $(return c) $(varT q) |]
                                | (q, c) <- zip qs cxt ]
          instanceD tentativeCxt [t| TryCxt $(return hd) $(varT r) |]
              []

    (newDecs, bodyCxts) <- unzip . map snd . filter fst <$> sequence [ do
                r <- case cxt of
                    [] -> noCxt instHd
                    _ -> yesCxt cxt instHd
                return (keepInst r, (r, getInstBodyNames cxt))
          | InstanceD cxt instHd _ <- insts ]

    let s' = s { seenInstances = existingInstsSet `S.union` S.fromList newDecs }
    return ((newDecs, S.unions bodyCxts), s')

-- | (rename x == rename y) <==> x and y are duplicate instances
rename :: Dec -> Dec
rename x = everywhereM (mkM fixName) (sortCxt (everywhere (mkT fixPromotedT) x)) `evalState` M.empty
  where
    fixName :: Name -> State (M.Map Name Name) Name
    fixName n | maybe False isLower (listToMaybe (nameBase n)) = state $ \ m -> 
        let n' = mkName $ "x" ++ show (M.size m)
        in case M.lookup n m of
             Nothing -> (n' , M.insert n n' m)
             Just n -> (n, m)
      | otherwise = return n

    -- ghc-7.10.2 (at least) does not consistently use PromotedT
    -- https://gist.github.com/aavogt/b60fa8b50384f84832a3
    fixPromotedT :: Type -> Type
    fixPromotedT (PromotedT x) = ConT x
    fixPromotedT x = x

    sortCxt (InstanceD ct ty d) = InstanceD (sort ct) ty d

{- given that Cxt is the (A a, B a) part in

  instance (A a, B a A XY) => C a

  make a set [A,B]

These cases won't be handled correctly

instance ((Num :: * -> Constraint) Int) => Foo Int
instance c a => D c a

-}
getInstBodyNames :: Cxt -> S.Set Name
getInstBodyNames xs = S.fromList $ mapMaybe f xs
  where
    f EqualityT = Nothing
    f (AppT x y) = f x
    f (ConT x) = Just x
    f (VarT x) = Just x -- wrong?
    f x = error ("IsInstance.getInstBodyNames: can't handle " ++ show x)



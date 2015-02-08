{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import IsInstance
import Data.Proxy
import Test.Hspec
import Unsafe.Coerce

writeIsInstance ''Show
writeIsInstance ''Num
writeIsInstance ''Fractional

f1 n = ifHasInstance (Proxy :: Proxy '[Show,Fractional]) "NA" (\n -> show $ n/2) n


f2a :: IsInstance Show t => t -> String
f2a n = ifHasInstance (Proxy :: Proxy Show) "NA" show n


#if __GLASGOW_HASKELL__ == 708
-- ghc-7.6 bug cannot infer the following type
f2b :: IsInstance Fractional t => t -> Either t t
#endif
f2b n = ifHasInstance (Proxy :: Proxy Fractional) (Left n) (\m -> Right (m/2)) n


f2 :: (IsInstance Fractional t, IsInstance Show t) => t -> String
f2 x = either ((++) "Left " . f2a) ((++) "Right " . f2a) (f2b x)

-- the inferred type is
-- (Show b, IsInstance Fractional b) => b -> String
-- f2_ x = f2a (f2b x)

-- instance (Show a, Show b) => IsInstance Show (Either a b)
--
-- actually want: (IsInstance Show a, IsInstance Show b) => IsInstance Show (Either a b)?

class C a where c :: a -> String

instance C Int where c _ = "c"
writeIsInstance ''C
{-

instance (IsInstance C a, IsInstance C b) => IsInstance C (Either a b) where
    ifHasInstance p n y (Left x) = ifHasInstance c n (y . Left) x
    -- ifHasInstance p n y (Right x) = ifHasInstance p n (undefined . Right) x
-}

mergeMInst :: ToCxt c (Either a b) => Either (MInst c a) (MInst c b) -> MInst c (Either a b)
mergeMInst (Left (No a)) = No (Left a)
mergeMInst (Right (No a)) = No (Right a)
mergeMInst (Left (Yes a)) = Yes (Left a)
mergeMInst (Right (Yes a)) = Yes (Right a)



main = hspec $ do
  it "f1" $ do
    f1 (1 :: Int) `shouldBe` "NA"
    f1 (1 :: Double) `shouldBe` "0.5"

  it "f2" $ do
    f2 (1 :: Int) `shouldBe` "Left 1"
    f2 (1 :: Double) `shouldBe` "Right 0.5"
    f2 ('1' : ) `shouldBe` "Left NA"

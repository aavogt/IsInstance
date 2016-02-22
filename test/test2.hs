{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import IsInstance
import Data.Proxy

writeTryCxt ''Show

f x = ifHasInstance (Proxy :: Proxy Show) "NO SHOW" show x
main = do
    putStrLn (f (+1))
    putStrLn (f ())

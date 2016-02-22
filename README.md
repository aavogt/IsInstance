# IsInstance
Usually people say that there is no way to check whether a type has an
instance of a class in haskell (see for example results for
<http://stackoverflow.com/search?q=%5bhaskell%5d+instanceof>) 

This code provides an `ifHasInstance` function following solution 1
<https://wiki.haskell.org/GHC/AdvancedOverlap>. GHC features such as
ConstraintKinds and template haskell are used to make that pattern
more convenient.

## Example

```haskell
-- the same as test/test2.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import IsInstance
import Data.Proxy

-- generates a TryCxt instance mirroring every Show instance
writeTryCxt ''Show

f x = ifHasInstance (Proxy :: Proxy Show) "NO SHOW" show x

main = do
    putStrLn (f (+1))
    putStrLn (f ())

    {- prints

    NO SHOW
    ()

    -}
```

This is more "local" than adding an overlapping instance to show:

```haskell
f x = show x
instance Show x where show _ = "<NO SHOW>"
```

# TODO

* an ExpQ splice for `ifHasInstance` that adds the necessary writeTryCxt declarations
* instance bodies may reference other classes in a way that writeTryCxt doesn't
  see, so `TryCxt` may be missing. For example:

```haskell
instance c Int => D c
instance (c Int, c ~ Num) => D Int
```

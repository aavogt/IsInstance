# IsInstance
Usually people say that there is [no way to check whether a type has an
instance of a class in haskell](http://stackoverflow.com/search?q=%5bhaskell%5d+instanceof) 

This code provides an `ifHasInstance` function following solution 1
[SPJ and Oleg's Advanced Overlap on the haskellwiki](https://wiki.haskell.org/GHC/AdvancedOverlap).
GHC features such as ConstraintKinds and template haskell make that
pattern more convenient.

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
    putStrLn (f (+1,"y"))
    putStrLn (f ('x',"y"))

    {- prints

    NO SHOW
    ('x',"y")

    -}
```

This is more "local" than adding an overlapping instance to show:

```haskell
f x = show x
instance Show x where show _ = "<NO SHOW>"
```

# TODO

* an ExpQ template haskell splice for `ifHasInstance` that adds the necessary writeTryCxt declarations
* individual constraints in the instance body can depend on each other but they are
  currently tested independently, while there are instances whose bodies should be checked in a particular order. For example `tests/fail1.hs` has `instance (c Int, c ~ Num) => D Int` which leads to the generated TryCxt instance has an instance body including a `(TryCxt (c Int) b1, TryCxt (c ~ Num) b2)`. Ghc rejects that instance due to the functional dependencies on TryCxt. In this case, `c ~ Num` should be checked first:

```haskell
  (TryCxt (c Int) b1, TryCxt (c ~ Num) b2)
  (TryCxt (c Int) b1, TryCxt (c ~ Num) True)
  (TryCxt (c Int) b1, c ~ Num)
  (TryCxt (Num Int) b1) -- can then find b1 ~ True
```

## Related Work

### ifcxt

[ifcxt](https://github.com/mikeizbicki/ifcxt) is a similar age. It is the same idea as "IsInstance", except IsInstance doesn't suffer from this [bug]https://github.com/mikeizbicki/ifcxt/issues/6

### GHC Impredicative-2015

The feature as a built-in class is intended to improve ghc's handling of
impredicative polymorphism (fs :: [ forall a. a -> a ])
https://ghc.haskell.org/trac/ghc/wiki/ImpredicativePolymorphism/Impredicative-2015

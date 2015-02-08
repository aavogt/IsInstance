# IsInstance
Check whether a type has an instance.

Overlapping instances are used in the package, but writing:

```haskell
f x = ifHasInstance (Proxy :: Proxy Show) "<NO SHOW>" show x
```

is more "local" than adding an overlapping instance to show:

```haskell
f x = show x
instance Show x where show _ = "<NO SHOW>"
```

# TODO

* MPTCs
* `instance (Show a, Show b) => IsInstance Show (a,b)` is currently generated
* an ExpQ splice for `ifHasInstance` that adds the necessary writeIsInstance declarations
* writeInstance should not create duplicate instances, but the most straightforwad solution (looking at reifyInstances ''IsInstance) fails

# Abstractions

## This is not new

There are patterns common accross many different types

## This is not new

There are patterns common accross many different types

```java
class Integer implements Comparable<Integer>
class Float implements Comparable<Float>
class Time implements Comparable<Time>
```

## We are not afraid

> - Java/C# `interface`
> - C++ `virtual`
> - Scala `implicits`

More powerful type system allows for more powerful abstractions

> - Haskell `typeclass`

## Typeclasses

```haskell
class Eq a where
    (==) :: a -> a -> Bool
```

## Typeclasses

```haskell
class Eq a where
    (==) :: a -> a -> Bool
```

## Typeclasses

```haskell
class Eq a where
    (==) :: a -> a -> Bool
```

```haskell
data Integer =  ... | -1 | 0 | 1 | ...
```

```haskell
instance Eq Integer where
    ...
    1 ==  1 = True
    0 ==  0 = True
   -1 == -1 = True
    ...
    _ ==  _ = False
```

## Typeclasses

```haskell
class Eq a where
    (==) :: a -> a -> Bool
```

```haskell
instance Eq a => Eq [a] where
    [] == [] = True
    (x:xs) == (y:ys) = x == y && ys == ys
```

## Typeclasses

```haskell
class Eq a where
    (==) :: a -> a -> Bool
```

```haskell
instance Eq a => Eq [a] where
    [] == [] = True
    (x:xs) == (y:ys) = x == y && ys == ys
```

```haskell
elem :: Eq a => a -> [a] -> Bool
```

## Typeclasses

```haskell
class Monoid m where
    (<>) :: a -> a -> a
    mempty :: a
```

```haskell
class Foldable t where
    foldMap :: Monoid m => (a -> m) -> t a -> m
```

## Typeclasses

```haskell
class Monoid m where
    (<>) :: a -> a -> a
    mempty :: a
```

```haskell
class Foldable t where
    foldMap :: Monoid m => (a -> m) -> t a -> m
```

```haskell
elem :: (Foldable t, Eq a) => a -> t a -> Bool
```

## {.middle}

One especially useful typeclass is called `Monad`

## Functor

```haskell
class Functor m where
    fmap :: (a -> b) -> m a -> m b
```

## Functor

```haskell
class Functor m where
    fmap :: (a -> b) -> m a -> m b
```

```haskell
data Maybe a = Nothing | Just a
```

```haskell
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just (f a)
```

## Functor

```haskell
class Functor m where
    fmap :: (a -> b) -> m a -> m b
```

```haskell
data Maybe a = Nothing | Just a
```

```haskell
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just (f a)
```

```haskell
instance Functor [a] where
    fmap _ [] = []
    fmap f (x:xs) = f x : fmap f xs
```

## Monad

```haskell
class Functor m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a
```

## Monad

```haskell
class Functor m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a
```

```haskell
instance Monad (Maybe a) where
    Nothing >>= _ = Nothing
    Just x >>= f = f x
    return = Just
```

## Monad

```haskell
class Functor m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a
```

```haskell
instance Monad (Maybe a) where
    Nothing >>= _ = Nothing
    Just x >>= f = f x
    return = Just
```

```haskell
instance Monad [a] where
    [] >>= _ = []
    (x:xs) >>= f = f x ++ (xs >>= f)
    return x = [x]
```

## More monads

```haskell
instance Monad IO
instance Monad ((->) r)
instance Monoid a => Monad ((,) a)
```

## More monads

```haskell
instance Monad IO
instance Monad ((->) r)
instance Monoid a => Monad ((,) a)
```

```haskell
f :: Int -> Bool
f = liftA2 (&&) (< 10) (/= 4)
```

## Bad: manually threading explicit state

```haskell
type Random a = Gen -> (a, Gen)

randInt :: Random Int

randLetter :: Random Char
randLetter g = (c, g')
  where
    (i, g') = rand g
    c = chr (mod i 26 + ord 'a')

randPassword :: Int -> Random String
randPassword 0 g = ([]  , g  )
randPassword n g = (c:cs, g'')
  where
    (c , g' ) = randLetter g
    (cs, g'') = randPassword (n - 1) g'
```

## Good: using Monad

```haskell
instance Functor Random where
    (fmap f r) g = let (a, g') = r in (f a, g')

instance Monad Random where
    (r >>= f)  g = let (a, g' ) = r g in f a
    (return a) g = (a, g)

randInt :: Random Int

randLetter :: Random Char
randLetter = fmap toAlph randInt
  where toAlph i = chr (mod i 26 + ord 'a')

randPassword :: Int -> Random String
randPassword n = replicateM n randLetter
```

## Event better: using the State Monad

```haskell
import Control.Monad.Trans.State (State)

-- type State s a = s -> (a, s)

type Random = State Gen

randInt :: Random Int

randLetter :: Random Char
randLetter = fmap toAlph randInt
  where toAlph i = chr (mod i 26 + ord 'a')

randPassword :: Int -> Random String
randPassword n = replicateM n randLetter
```

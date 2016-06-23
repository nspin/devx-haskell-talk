# Abstractions

## Manually explicit threading state

```haskell
type Random a = Gen -> (a, Gen)
```

## Manually explicit threading state

```haskell
type Random a = Gen -> (a, Gen)
```

```haskell
randInt :: Random Int

randChar :: Random Char
randChar g = (c, g')
  where
    (i, g') = rand g
    c = chr (i + ord 'a')

randPassword :: Int -> Random String
randPassword 0 g = ([]  , g  )
randPassword n g = (c:cs, g'')
  where
    (c , g' ) = randChar g
    (cs, g'') = randPassword (n - 1) g'
```

## Using common abstractions

Don't focus on the 'M' word

## Using common abstractions

```haskell
import Control.Monad (fmap, replicateM)
import Control.Monad.State.Lazy (State)

type Random a = State Gen a
```

## Using common abstractions

```haskell
import Control.Monad (fmap, replicateM)
import Control.Monad.State.Lazy (State)

type Random a = State Gen a
```

```haskell
randInt :: Random Int

randChar :: Random Char
randChar = fmap toAlph randInt
  where
    toAlph i = chr (i + ord 'a')

randPassword :: Int -> Random String
randPassword n = replicateM n randChar
```

## They are awesome

<!-- TODO intriguing but tame example (e.g. using forall a. Applicative ((->) a)) -->

## They are not *that* special

You are familiar with this concept

> - Java/C# `interface`
> - C++ `virtual`
> - Scala `implicits`

More powerful type system allows for more powerful abstractions

> - Haskell `typeclass`

## {.middle}

*One* of them is called `Monad`


# Language Overview

## Program structure

Top-level statements

## Program structure

```haskell
data Color = Red | Orange | Yellow | Green
```

## Program structure

```haskell
data Color = Red | Orange | Yellow | Green

data Shape = Circle Int | Rectangle Int Int
```

## Program structure

```haskell
data Color = Red | Orange | Yellow | Green

data Shape = Circle Int | Rectangle Int Int

data Graphic = Graphic Color Shape
```

## Program structure

```haskell
data Color = Red | Orange | Yellow | Green

data Shape = Circle Int | Rectangle Int Int

data Graphic = Graphic Color Shape
```

```haskell
favoriteColor :: Color
favoriteColor = Green

tennisBall :: Graphic
tennisBall = Graphic favoriteColor (Circle 12)
```

## Program structure

```haskell
data Color = Red | Orange | Yellow | Green

data Shape = Circle Int | Rectangle Int Int

data Graphic = Graphic Color Shape
```

```haskell
nameOf :: Color -> String
nameOf Red    = "red"
nameOf Orange = "orange"
nameOf Yellow = "yellow"
nameOf Green  = "green"
```

## Program structure

```haskell
data Color = Red | Orange | Yellow | Green

data Shape = Circle Int | Rectangle Int Int

data Graphic = Graphic Color Shape
```

```haskell
nameOf :: Color -> String
nameOf Red    = "red"
nameOf Orange = "orange"
nameOf Yellow = "yellow"
nameOf Green  = "green"

describe :: Graphic -> String
describe (Graphic color shape) = nameOf color ++ " " ++ shapeDescription
  where
    shapeDescription = case shape of
        Circle radius -> "circle with a radius of " ++ show radius
        Rectangle height width -> show height ++ "x" ++ show width ++ " rectangle"
```

```haskell
(++) :: String -> String -> String
show :: Int -> String
```

## Program structure

```haskell
data Color = Red | Orange | Yellow | Green

data Shape = Circle Int | Rectangle Int Int

data Graphic = Graphic Color Shape
```

```haskell
nameOf :: Color -> String
nameOf Red    = "red"
nameOf Orange = "orange"
nameOf Yellow = "yellow"
nameOf Green  = "green"

describe :: Graphic -> String
describe (Graphic color shape) = nameOf color ++ " " ++ shapeDescription
  where
    shapeDescription = case shape of
        Circle radius -> "circle with a radius of " ++ show radius
        Rectangle height width -> show height ++ "x" ++ show width ++ " rectangle"
```

```bash
>>> describe tennisBall
"green circle with a radius of 12"
>>> describe (Graphic Orange (Rectangle 3 7))
"orange 3x7 rectangle"
```

## Properties

> - Statically typed
> - Purely functional
> - Lazy

<!--
type-level
value-level

optional type signatures
pattern = expression
'where' and similar constructs
-->

## Statically typed

> - Types of all values and expressions determined and checked at compile time
> - Eliminates large class of run-time errors
> - Useful for documentation
> - Type inference
> - Powerful type system

## Statically typed

Lame

```java
boolean all(Predicate<A> pred, List<A> list)
```

## Statically typed

Lame

```java
boolean all(Predicate<A> pred, List<A> list)
```

Higher-order polymorphism

```haskell
all :: Foldable t => (a -> Bool) -> t a -> Bool
```

<!--
type-level variable
-->

## Functional

> - Functions are first-class values
> - Higher-order functions
> - Currying
> - Tacit programming
> - Expressive

## Functional

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter pred (x:xs) = if pred x then x:ys else ys
  where
    ys = filter pred xs
```

<!--
list syntax
-->

## Pure

> - Referential transparency
> - Think table lookup
> - Easy to test, reason about, and even prove the correctness of code
> - Allows for heavy optimization
> - ...
> - Everything is immutable

<!-- if compiles, runs -->

## Pure

No loops

```haskell
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs
```

<!--
must use recursion, but doesn't have to be explicit
shared structure between data
-->

## Lazy

> - Possible because of purity
> - Thunks

## Lazy

- Possible because of purity
- Thunks

```haskell
zeros :: [Int]
zeros = 0 : zeros
```

## Lazy

- Possible because of purity
- Thunks

```haskell
zeros :: [Int]
zeros = 0 : zeros
```

```haskell
mystery :: Shape
mystery = Circle (1 / 0)
```

## Fun with laziness

```haskell
fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)
```

## Fun with laziness

```haskell
fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)
```

```haskell
seive :: [Integer] -> [Integer]
seive (p:xs) = p : seive [ x | x <- xs, x `mod` p /= 0 ]
```

## Fun with laziness

```haskell
fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)
```

```haskell
seive :: [Integer] -> [Integer]
seive (p:xs) = p : seive [ x | x <- xs, x `mod` p /= 0 ]

primes :: [Integer]
primes = seive [2..]
```

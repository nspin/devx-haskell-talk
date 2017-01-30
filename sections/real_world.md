# Into the real world

## {.middle}

What is a program without effects?

## Implicit state

```c
void make_password(int n, char *buf) {
    while (n-- > 0) {
        buf[n] = 'a' + rand() % 26;
    }
}
```

## Explicit state

```haskell
type Random a = Gen -> (a, Gen)
```

## Explicit state

```haskell
type Random a = Gen -> (a, Gen)
```

```haskell
randInt :: Random Int
randInt g = (r .&. 0x4FFFFFFF, r .&. 0x1FFFFFFF)
  where
    r = a * g + c 
    a = 1103515245
    c = 12345

type Gen = Int
```

## Explicit state

```haskell
type Random a = Gen -> (a, Gen)
```

```haskell
randInt :: Random Int
```

```haskell
randLetter :: Random Char
randLetter g = (c, g')
  where
    (i, g') = rand g
    c = chr (mod i 26 + ord 'a')
```

## Explicit state

```haskell
type Random a = Gen -> (a, Gen)
```

```haskell
randInt :: Random Int
randLetter :: Random Char
```

```haskell
randPassword :: Int -> Random String
randPassword 0 g = ([]  , g  )
randPassword n g = (c:cs, g'')
  where
    (c , g' ) = randLetter g
    (cs, g'') = randPassword (n - 1) g'
```

## Explicit state

```haskell
type Random a = Gen -> (a, Gen)
```

```haskell
randInt :: Random Int
randLetter :: Random Char
randPassword :: Int -> Random String
```

## Monad preview: bad

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

## Monad preview: good

```haskell
type Random a = State Gen a

randInt :: Random Int

randLetter :: Random Char
randLetter = fmap toAlph randInt
  where toAlph i = chr (mod i 26 + ord 'a')

randPassword :: Int -> Random String
randPassword n = replicateM n randLetter
```

## The real world

```haskell
type Universe
```

## The real world

```haskell
type Universe

type IO a = Universe -> (a, Universe)
```

## The real world

```haskell
type Universe

type IO a = Universe -> (a, Universe)

getChar :: IO Char
```

## The real world

```haskell
type Universe

type IO a = Universe -> (a, Universe)

getChar :: IO Char
putStrLn :: String -> IO ()
```

## The real world according to GHC

```haskell
-- | @State#@ is the primitive, unlifted type of states. It has
--         one type parameter, thus @State# RealWorld@, or @State# s@,
--         where s is a type variable. The only purpose of the type parameter
--         is to keep different state threads separate. It is represented by
--         nothing at all. 

data State# s

-- | @RealWorld@ is deeply magical. It is /primitive/, but it is not
--         /unlifted/ (hence @ptrArg@). We never manipulate values of type
--         @RealWorld@; it's only used in the type system, to parameterise
--         @State#@. 

data RealWorld
```

## The real world according to GHC

```haskell
type Universe = State# RealWorld

type IO a = Universe -> (a, Universe)

getChar :: IO Char
print   :: String -> IO ()
```

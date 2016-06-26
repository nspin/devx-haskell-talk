{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import Prelude hiding (all, reverse, takeWhile, zip, concat, concatMap)
import Test.HUnit

main :: IO ()
main = do
   _ <- runTestTT $ TestList [ testStyle,
                               testLists ]
   return ()

-------------------------------------------------------------------------------
-- SECTION 1

testStyle :: Test
testStyle = "testStyle" ~:
   TestList [ tabc , treverse, tzap ]

-- The abc function takes three booleans x, y, and z.
-- The function returns the result (x && (y || z)).
abc :: Bool -> Bool -> Bool -> Bool
abc = error "unimplemented"

tabc :: Test
tabc = "abc" ~: TestList [abc True True False  ~?= True,
                          abc True False True  ~?= True,
                          abc True False False ~?= False,
                          abc False True True  ~?= False]

-- The reverse function takes a list as a parameter
-- and returns the reversed list.
reverse :: [a] -> [a]
reverse = error "unimplemented"

treverse :: Test
treverse = "reverse" ~: TestList [reverse [3,2,1] ~?= [1,2,3],
                                  reverse [2,1]   ~?= [1,2],
                                  reverse [1]     ~?= [1],
                                  reverse []      ~?= ([] :: [Int])]


-- The zap function takes two lists, a list of unary functions and a list
-- of arguments. The function returns a list of values where each value
-- is the result of calling the function with the argument at the same index.
zap :: [a -> b] -> [a] -> [b]
zap = error "unimplemented"

tzap :: Test
tzap = "zap" ~:
  TestList [ zap [ (+1), \n -> n - 1, (+1) ]
                 ([3, 4, 5] :: [Int]) ~?= [4,3,6],
             zap [ null, not . null ]
                 [ [], "a" ] ~?= [True, True],
             zap [] "a"    ~?= ([] :: [Int]),
             zap [not] []  ~?= []]

-------------------------------------------------------------------------------
-- SECTION 2

testLists :: Test
testLists = "testLists" ~: TestList [tintersperse, ttakeWhile, tfind,
                                     tall, tmap2, tzip, tconcat]

-- The intersperse function takes an element and a list
-- and intersperses that element between the elements of the list.
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"

intersperse :: a -> [a] -> [a]
intersperse = error "unimplemented"

tintersperse :: Test
tintersperse = "intersperse" ~:
  TestList [ intersperse 0 []      ~?= [],
             intersperse 1 [0]     ~?= [0],
             intersperse 2 [1,1]   ~?= [1,2,1],
             intersperse 2 [1,1,1] ~?= [1,2,1,2,1]]

-- takeWhile, applied to a predicate p and a list xs,
-- returns the longest prefix (possibly empty) of xs of elements
-- that satisfy p:
-- For example,
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = error "unimplemented"

ttakeWhile :: Test
ttakeWhile = "takeWhile" ~:
  TestList [ takeWhile (< 1) [] ~?= [],
             takeWhile (< 3) [1,2,3,4,1,2,3,4] ~?= [1,2],
             takeWhile (< 9) [1,2,3] ~?= [1,2,3],
             takeWhile (< 0) [1,2,3] ~?= [] ]

-- find pred lst returns the first element of the list that
-- satisfies the predicate. Because no element may do so, the
-- answer is returned in a "Maybe".
-- for example:
--     find odd [0,2,3,4] returns Just 3
find :: (a -> Bool) -> [a] -> Maybe a
find = error "unimplemented"

tfind :: Test
tfind = "find" ~:
  TestList [ find odd []    ~?= Nothing,
             find odd [1,3] ~?= Just 1,
             find odd [2]   ~?= Nothing ]


-- all pred lst returns False if any element of lst fails to satisfy
-- pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False
all :: (a -> Bool) -> [a] -> Bool
all = error "unimplemented"

tall :: Test
tall = "all" ~:
  TestList [ all odd [] ~?= True,
             all odd [1,2] ~?= False,
             all odd [1,3] ~?= True ]

-- map2 f xs ys returns the list obtained by applying f to
-- to each pair of corresponding elements of xs and ys. If
-- one list is longer than the other, then the extra elements
-- are ignored.
-- i.e.
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- NOTE: map2 is called zipWith in the standard library.
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 = error "unimplemented"

tmap2 :: Test
tmap2 = "map2" ~:
  TestList [ map2 (+) [] [0]        ~?= [],
             map2 (+) [0] []        ~?= [],
             map2 (+) [1] [1]       ~?= [2],
             map2 (+) [1,2] [1,2,3] ~?= [2,4]]

-- zip takes two lists and returns a list of corresponding pairs. If
-- one input list is shorter, excess elements of the longer list are
-- discarded.
-- for example:
--    zip [1,2] [True] returns [(1,True)]
zip :: [a] -> [b] -> [(a,b)]
zip = error "unimplemented"

tzip :: Test
tzip = "zip" ~:
  TestList [ zip [] [] ~?= ([] :: [(Int,Int)]),
             zip [] [0] ~?= ([] :: [(Int,Int)]),
             zip [1,2] "ab" ~?= [(1,'a'),(2,'b')],
             zip [1,2] [True] ~?= [(1,True)]]

-- concat

-- The concatenation of all of the elements of a list of lists
-- for example:
--    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]
concat :: [[a]] -> [a]
concat = error "unimplemented"

tconcat :: Test
tconcat = "concat" ~:
  TestList [ concat [] ~?= ([] :: [Int]),
             concat [[1,2]] ~?= [1,2],
             concat [[1],[2],[3]] ~?= [1,2,3],
             concat [[1,2,3],[4,5,6],[7,8,9]] ~?= [1,2,3,4,5,6,7,8,9],
             concat [[1,2],[],[3,4]] ~?= [1,2,3,4]]


-------------------------------------------------------------------------------
-- SECTION 3

-- concatMap

-- Map a function over all the elements of a list and concatenate the results.
-- for example:
--    concatMap (\x -> [x,x+1,x+2]) [1,2,3]  returns [1,2,3,2,3,4,3,4,5]
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap = error "unimplemented"

tconcatMap :: Test
tconcatMap = "concatMap" ~:
  TestList [ concatMap (: []) [] ~?= ([] :: [Int]),
             concatMap (\x -> [x,x+1,x+2]) [1,2,3] ~?= [1,2,3,2,3,4,3,4,5],
             concatMap (\x -> [x+1]) [1,1,1] ~?= [2,2,2]]

lcs :: String -> String -> String
lcs = error "unimplemented"

testLcs :: Test
testLcs = "Lcs" ~: TestList [
    lcs "" "" ~?= "",
    lcs "a" "" ~?= "",
    lcs "a" "b" ~?= "",
    lcs "a" "a" ~?= "a",
    lcs "abc" "abc" ~?= "abc",
    lcs "Advanced" "Advantaged" ~?= "Advaned",
    lcs "abcd" "acbd" ~?= "acd" ]

-- Exercise set 5a
--
-- * defining algebraic datatypes
-- * recursive datatypes

module Set5a where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: Define the type Vehicle that has four constructors: Bike,
-- Bus, Tram and Train.
--
-- The constructors don't need any fields.
data Vehicle = Bike | Bus | Tram | Train

------------------------------------------------------------------------------
-- Ex 2: Define the type BusTicket that can represent values like these:
--  - SingleTicket
--  - MonthlyTicket "January"
--  - MonthlyTicket "December"
data BusTicket = SingleTicket | MonthlyTicket String

------------------------------------------------------------------------------
-- Ex 3: Here's the definition for a datatype ShoppingEntry that
-- represents an entry in a shopping basket. It has an item name (a
-- String), an item price (a Double) and a count (an Int). You'll also
-- find two examples of ShoppingEntry values.
--
-- Implement the functions totalPrice and buyOneMore below.

data ShoppingEntry = MkShoppingEntry String Double Int
  deriving Show

threeApples :: ShoppingEntry
threeApples = MkShoppingEntry "Apple" 0.5 3

twoBananas :: ShoppingEntry
twoBananas = MkShoppingEntry "Banana" 1.1 2

-- totalPrice should return the total price for an entry
--
-- Hint: you'll probably need fromIntegral to convert the Int into a
-- Double
--
-- Examples:
--   totalPrice threeApples  ==> 1.5
--   totalPrice twoBananas   ==> 2.2

totalPrice :: ShoppingEntry -> Double
totalPrice (MkShoppingEntry name price count) = price * fromIntegral count

-- buyOneMore should increment the count in an entry by one
--
-- Example:
--   buyOneMore twoBananas    ==> MkShoppingEntry "Banana" 1.1 3

buyOneMore :: ShoppingEntry -> ShoppingEntry
buyOneMore (MkShoppingEntry name price count) = MkShoppingEntry name price (count+1)

------------------------------------------------------------------------------
-- Ex 4: define a datatype Person, which should contain the age (an
-- Int) and the name (a String) of a person.
--
-- Also define a Person value fred, and the functions getAge, getName,
-- setAge and setName (see below).

data Person = MkPerson Int String
  deriving Show

-- fred is a person whose name is Fred and age is 90
fred :: Person
fred = MkPerson 90 "Fred"

-- getName returns the name of the person
getName :: Person -> String
getName (MkPerson age name) = name

-- getAge returns the age of the person
getAge :: Person -> Int
getAge (MkPerson age name) = age

-- setName takes a person and returns a new person with the name changed
setName :: String -> Person -> Person
setName name (MkPerson age old_name) = MkPerson age name

-- setAge does likewise for age
setAge :: Int -> Person -> Person
setAge age (MkPerson old_age name) = MkPerson age name

------------------------------------------------------------------------------
-- Ex 5: define a datatype Position which contains two Int values, x
-- and y. Also define the functions below for operating on a Position.
--
-- Examples:
--   getY (up (up origin))    ==> 2
--   getX (up (right origin)) ==> 1

data Position = MkPosition Int Int

-- origin is a Position value with x and y set to 0
origin :: Position
origin = MkPosition 0 0

-- getX returns the x of a Position
getX :: Position -> Int
getX (MkPosition x y) = x

-- getY returns the y of a position
getY :: Position -> Int
getY (MkPosition x y) = y

-- up increases the y value of a position by one
up :: Position -> Position
up (MkPosition x y) = MkPosition x (y+1)

-- right increases the x value of a position by one
right :: Position -> Position
right (MkPosition x y) = MkPosition (x+1) y

------------------------------------------------------------------------------
-- Ex 6: Here's a datatype that represents a student. A student can
-- either be a freshman, a nth year student, or graduated.

data Student = Freshman | NthYear Int | Graduated
  deriving (Show,Eq)

-- Implement the function study, which changes a Freshman into a 1st
-- year student, a 1st year student into a 2nd year student, and so
-- on. A 7th year student gets changed to a graduated student. A
-- graduated student stays graduated even if he studies.

study :: Student -> Student
study Freshman = NthYear 1
study Graduated = Graduated
study (NthYear 7) = Graduated
study (NthYear n) = NthYear (n+1)

------------------------------------------------------------------------------
-- Ex 7: define a datatype UpDown that represents a counter that can
-- either be in increasing or decreasing mode. Also implement the
-- functions zero, toggle, tick and get below.
--
-- NB! Define _two_ constructors for your datatype (feel free to name the
-- constructors however you want)
--
-- Examples:
--
-- get (tick zero)
--   ==> 1
-- get (tick (tick zero))
--   ==> 2
-- get (tick (tick (toggle (tick zero))))
--   ==> -1

data UpDown = Increasing Int | Decreasing Int

-- zero is an increasing counter with value 0
zero :: UpDown
zero = Increasing 0

-- get returns the counter value
get :: UpDown -> Int
get (Increasing count) = count
get (Decreasing count) = count

-- tick increases an increasing counter by one or decreases a
-- decreasing counter by one
tick :: UpDown -> UpDown
tick (Increasing count) = Increasing (count+1)
tick (Decreasing count) = Decreasing (count-1)

-- toggle changes an increasing counter into a decreasing counter and
-- vice versa
toggle :: UpDown -> UpDown
toggle (Increasing count) = Decreasing count
toggle (Decreasing count) = Increasing count


------------------------------------------------------------------------------
-- Ex 8: you'll find a Color datatype below. It has the three basic
-- colours Red, Green and Blue, and two color transformations, Mix and
-- Invert.
--
-- Mix means the average of the two colors in each rgb channel.
--
-- Invert means subtracting all rgb values from 1.
--
-- Implement the function rgb :: Color -> [Double] that returns a list
-- of length three that represents the rgb value of the given color.
--
-- Examples:
--
-- rgb Red   ==> [1,0,0]
-- rgb Green ==> [0,1,0]
-- rgb Blue  ==> [0,0,1]
--
-- rgb (Mix Red Green)                    ==> [0.5,0.5,0]
-- aka Mix [1 0 0] with [0 1 0]   to get [1/2 1/2 0/2]

-- rgb (Mix Red (Mix Red Green))          ==> [0.75,0.25,0]
-- aka Mix [1 0 0] with [0.5 0.5 0]   to get [1.5 0.5 0] aka [3/4 1/4 0/4]

-- rgb (Invert Red)                       ==> [0,1,1]
-- rgb (Invert (Mix Red (Mix Red Green))) ==> [0.25,0.75,1]
-- rgb (Mix (Invert Red) (Invert Green))  ==> [0.5,0.5,1]
-- aka Mix [0 1 1] with [1 0 1]     to get [1/2 1/2 2/2]

data Color = Red | Green | Blue | Mix Color Color | Invert Color
  deriving Show

rgb :: Color -> [Double]
rgb Red = [1,0,0]
rgb Green = [0,1,0]
rgb Blue = [0,0,1]
rgb (Invert c) = map (1 -) (rgb c)
rgb (Mix c1 c2) = let [r1, g1, b1] = rgb c1 in
  let [r2, g2, b2] = rgb c2 in
    map (/2) [r1+r2, g1+g2, b1+b2]

------------------------------------------------------------------------------
-- Ex 9: define a parameterized datatype OneOrTwo that contains one or
-- two values of the given type. The constructors should be called One and Two.
--
-- Examples:
--   One True         ::  OneOrTwo Bool
--   Two "cat" "dog"  ::  OneOrTwo String
data OneOrTwo a = One a | Two a a

------------------------------------------------------------------------------
-- Ex 10: define a recursive datatype KeyVals for storing a set of
-- key-value pairs. There should be two constructors: Empty and Pair.
--
-- Empty represents an empty collection. It should have no fields.
--
-- Pair should have three fields, one for the key, one for the value,
-- and one for the rest of the collection (of type KeyVals)
--
-- The KeyVals datatype is parameterized by the key type k and
-- the value type v.
--
-- For example:
--
--  Pair "cat" True (Pair "dog" False Empty)  ::  KeyVals String Bool
--
-- Also define the functions toList and fromList that convert between
-- KeyVals and lists of pairs.

data KeyVals k v = Empty | Pair k v (KeyVals k v)
  deriving Show

toList :: KeyVals k v -> [(k,v)]
toList Empty = []
toList (Pair k v rest) = (k,v):toList rest

fromList :: [(k,v)] -> KeyVals k v
fromList [] = Empty
fromList ((k,v):rest) = Pair k v (fromList rest)

------------------------------------------------------------------------------
-- Ex 11: The data type Nat is the so called Peano
-- representation for natural numbers. Define functions fromNat and
-- toNat that convert natural numbers to Ints and vice versa.
--
-- Examples:
--   fromNat (PlusOne (PlusOne (PlusOne Zero)))  ==>  3
--   toNat 3    ==> Just (PlusOne (PlusOne (PlusOne Zero)))
--   toNat (-3) ==> Nothing
--

data Nat = Zero | PlusOne Nat
  deriving (Show,Eq)

fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (PlusOne Zero) = 1
fromNat (PlusOne (PlusOne num)) = 1+fromNat (PlusOne num)

toNat :: Int -> Maybe Nat
toNat 0 = Just Zero
toNat 1 = Just (PlusOne Zero)
toNat n = if n < 0 then Nothing else
  case toNat (n-1) of
    Nothing -> Nothing
    Just num -> Just (PlusOne num)

------------------------------------------------------------------------------
-- Ex 12: While pleasingly simple in its definition, the Nat datatype is not
-- very efficient computationally. Instead of the unary Peano natural numbers,
-- computers use binary numbers.
--
-- Binary numbers are like decimal numbers, except that binary numbers have
-- only two digits (called bits), 0 and 1. The table below gives some
-- examples:
--
--   decimal | binary
--   --------+-------
--         0 |      0
--         1 |      1
--         2 |     10
--         7 |    111
--        44 | 101100
--
-- For allowing arbitrarily long binary numbers, our representation, the
-- datatype Bin, includes a special End constructor for denoting the end of
-- the binary number. In order to make computation with Bin easier, the bits
-- are represented in increasing order by significance (i.e. "backwards").
-- Consider the Bin numbers O (I (I End)), representing 110 in binary or
-- 6 in decimal, and I (I (O End)) that represents 011 in binary or 3 in
-- decimal. The most significant (last) bit, the bit I, of O (I (I End)) is
-- greater than the bit O, which is the most significant bit of I (I (O End)).
-- Therefore, O (I (I End)) is greater than I (I (O End)).
--
-- Your task is to write functions prettyPrint, fromBin, and toBin that
-- convert Bin to human-readable string, Bin to Int, and Int to Bin
-- respectively.
--
-- Examples:
--   prettyPrint End                     ==> ""
--   prettyPrint (O End)                 ==> "0"
--   prettyPrint (I End)                 ==> "1"
--   prettyPrint (O (O (I (O (I End))))) ==> "10100"
--   map fromBin [O End, I End, O (I End), I (I End), O (O (I End)),
--                  I (O (I End))]
--     ==> [0, 1, 2, 3, 4, 5]
--   fromBin (I (I (O (O (I (O (I (O End)))))))) ==> 83
--   fromBin (I (I (O (O (I (O (I End)))))))     ==> 83
--   map toBin [0..5] ==>
--     [O End,I End,O (I End),I (I End),O (O (I End)),I (O (I End))]
--   toBin 57 ==> I (O (O (I (I (I End)))))
--
-- Challenge: Can you implement toBin by directly converting its input into a
-- sequence of bits instead of repeatedly applying inc?
--
data Bin = End | O Bin | I Bin
  deriving (Show, Eq)

-- This function increments a binary number by one.
inc :: Bin -> Bin
inc End   = I End
inc (O b) = I b
inc (I b) = O (inc b)

prettyPrint :: Bin -> String
prettyPrint End = ""
prettyPrint (O rest) = prettyPrint rest ++ "0"
prettyPrint (I rest) = prettyPrint rest ++ "1"

fromBin :: Bin -> Int
fromBin b = fromBin' b (O End) 0
  where fromBin' :: Bin -> Bin -> Int -> Int
        fromBin' b currBin currNum = if currBin == b
          then currNum
          else fromBin' b (inc currBin) (currNum+1)

toBin :: Int -> Bin
toBin num = toBin' num 0 (O End)
  where toBin' :: Int -> Int -> Bin -> Bin
        toBin' num currNum currBin = if num == currNum
          then currBin
          else toBin' num (currNum+1) (inc currBin)
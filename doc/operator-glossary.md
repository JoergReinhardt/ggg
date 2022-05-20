== [[operator-glossary]] ==

+--------+----------------------+-----------------------+-------------------+
| Prec-  |   Left associative   |    Non-associative    | Right associative |
| edence |      operators       |       operators       |    operators      |
+--------+----------------------+-----------------------+-------------------+
| 9      | !!                   |                       | .                 |
| 8      |                      |                       | ^, ^^, **         |
| 7      | *, /, `div`,         |                       |                   |
|        | `mod`, `rem`, `quot` |                       |                   |
| 6      | +, -                 |                       |                   |
| 5      |                      |                       | :, ++             |
| 4      |                      | ==, /=, <, <=, >, >=, |                   |
|        |                      | `elem`, `notElem`     |                   |
| 3      |                      |                       | &&                |
| 2      |                      |                       | ||                |
| 1      | >>, >>=              |                       |                   |
| 0      |                      |                       | $, $!, `seq`      |
+--------+----------------------+-----------------------+-------------------+

Function application $

($) :: (a -> b) -> a -> b

One of the most common operators, and source of initial confusion, is the $
operator. All this does is apply a function. So, f $ x is exactly equivalent to
f x. If so, why would you ever use $? The primary reason is - for those who
prefer the style - to avoid parentheses. For example, you can replace:

foo (bar (baz bin))
with

foo $ bar $ baz bin

A less common but arguably more compelling use case is to capture the act of
applying a function to an argument. To clarify that rather vague statement with
an example:

double :: Int -> Int
double x = x + x

square :: Int -> Int
square x = x * x

main :: IO ()
main = print (map ($ 5) [double, square])

The ($ 5) bit means "apply the function to 5", and then we can use map to use
it with both the double and square functions.

Function composition .
(.) :: (b -> c) -> (a -> b) -> (a -> c)
Not much more to it than that: take two functions and compose them together.

double :: Int -> Int
double x = x + x

square :: Int -> Int
square x = x * x

main :: IO ()

main = (print . double . square) 5

Or you can combine this together with the $ operator to avoid those parentheses
if you're so inclined:

main = print . double . square $ 5

In addition to its usage for function composition, the period is also used for
hierarchical modules, e.g.: import qualified Data.Monoid

main :: IO ()

main = putStrLn $ Data.Monoid.mappend "hello " "world"

Finally, in the Control.Category module, the Category typeclass also uses the .
operator to define categorical composition. This generalizes standard function
composition, but is not as commonly used.

Reverse function application &

(&) :: a -> (a -> b) -> b

& is just like $ only backwards. Take our example for $:

foo $ bar $ baz bin
This is semantically equivalent to:

bin & baz & bar & foo

& is useful because the order in which functions are applied to their arguments
read left to right instead of the reverse (which is the case for $). This is
closer to how English is read so it can improve code clarity.

In our function composition example we composed the functions square, double,
and print and applied the resulting function to the number 5.

Rewriting it using & gives us

import Data.Function

double :: Int -> Int
double x = x + x

square :: Int -> Int
square x = x * x

main :: IO ()
main = 5 & square & double & print

Monoidal append <>

(<>) :: Monoid m => m -> m -> m

The <> operator is just a synonym for the mappend function. This comes from the
Monoid typeclass, which represents types which have an identity and an
associative binary operation. Some examples:

For lists, <> is the same as ++ (append two lists) For vectors, this logic
holds as well For Sets, this is a union operation (all values present in either
Set) For Maps, we have a "left biased union", meaning we combine the key/value
pairs from both inputs, and if both inputs share a key, the value in the left
input is selected For numbers, both addition and multiplication form a Monoid,
where 0 is the additive identity (since 0 + x = x) and 1 is the multiplicative
identity (since 1 * x = x). Therefore, to avoid confusion, Data.Monoid defines
helper newtype wrappers Sum and Product

import Data.Monoid ((<>))

main :: IO ()
main = putStrLn $ "hello " <> "there " <> "world!"
Functor map <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$) :: Functor f => a -> f b -> f a
($>) :: Functor f => f a -> b -> f b

The <$> operator is just a synonym for the fmap function from the Functor
typeclass. This function generalizes the map function for lists to many other
data types, such as Maybe, IO, and Map.

import Data.Monoid ((<>))

main :: IO ()

main = do
    putStrLn "Enter your year of birth"
    year <- read <$> getLine
    let age :: Int
        age = 2020 - year
    putStrLn $ "Age in 2020: " <> show age
    
In addition, there are two additional operators provided which replace a value
inside a Functor instead of applying a function. This can be both more
convenient in some cases, as well as for some Functors be more efficient. In
terms of definition:

value <$ functor = const value <$> functor
functor $> value = const value <$> functor

x <$ y = y $> x
x $> y = y <$ x

Applicative function application <*>

(<*>) :: Applicative f => f (a -> b) -> f a -> f b
(*>) :: Applicative f => f a -> f b -> f b
(<*) :: Applicative f => f a -> f b -> f a

Commonly seen with <$>, <*> is an operator that applies a wrapped function to a
wrapped value. It is part of the Applicative typeclass, and is very often seen

in code like the following:

foo <$> bar <*> baz
For cases when you're dealing with a Monad, this is equivalent to:

do x <- bar
   y <- baz
   return (foo x y)

Other common examples including parsers and serialization libraries. Here's an
example you might see using the aeson package:

data Person = Person { name :: Text, age :: Int } deriving Show

-- We expect a JSON object, so we fail at any non-Object value.

instance FromJSON Person where
    parseJSON (Object v) = Person <$> v .: "name" <*> v .: "age"
    parseJSON _ = empty

To go along with this, we have two helper operators that are less frequently
used:

*> ignores the value from the first argument. It can be defined as:

a1 *> a2 = (id <$ a1) <*> a2

Or in do-notation:

a1 *> a2 = do
    _ <- a1
    a2
For Monads, this is completely equivalent to >>.

<* is the same thing in reverse: perform the first action then the second, but
only take the value from the first action. Again, definitions in terms of <*>
and do-notation:

(<*) = liftA2 const

a1 <* a2 = do
    res <- a1
    _ <- a2
    return res
Various monadic binding/composition operators

(>>)  :: Monad m => m a -> m b -> m b
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)

There are a few different monadic binding operators. The two most basic are >>=
and >>, as they can be trivially expressed in do-notation. And as previously
mentioned, >> is just a synonym for *> from the Applicative class, so it's even
easier. =<< is just >>= with the arguments reversed.

m1 >>= f = do
    x <- m1
    f x

m1 >> m2 = do
    _ <- m1
    m2

f =<< m1 = do
    x <- m1
    f x

In addition to these two operators, there are also composition operators for
when you have two monadic functions. >=> pipes the result from the left side to
the right side, while <=< pipes the result the other way. In other words:

f >=> g = \x -> do
    y <- f x
    g y

g <=< f = \x -> do
    y <- f x
    g y

f >=> g = g <=< f

g >=> f = f <=< g

Alternative <|>

(<|>) :: Alternative f => f a -> f a -> f a

The Alternative typeclass provides a binary operation on applicative functors
(<|>), as well as some identity value (empty). This is used in the ecosystem
for a number of different activities, e.g.:

In parser libraries for defining different alternative parsing options In the
async library to run two different Concurrently actions at once and take the
first result to succeed

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..))

main :: IO ()
main = do
    res <- runConcurrently $
        (Concurrently (threadDelay 1000000 >> return (Left "Hello"))) <|>
        (Concurrently (threadDelay 2000000 >> return (Right 42)))
    print res
  
whenever i feel overwhelmed by haskell syntax, i remind myself how much more
disturbing notation can be:

API

⌶ U+2336 ⌷ U+2337 ⌸ U+2338 ⌹ U+2339 ⌺ U+233A ⌻ U+233B ⌼ U+233C ⌽ U+233D ⌾ U+233E ⌿ U+233F ⍀ U+2340
⍁ U+2341 ⍂ U+2342 ⍃ U+2343 ⍄ U+2344 ⍅ U+2345 ⍆ U+2346 ⍇ U+2347 ⍈ U+2348 ⍉ U+2349 ⍊ U+234A ⍋ U+234B
⍌ U+234C ⍍ U+234D ⍎ U+234E ⍏ U+234F ⍐ U+2350 ⍑ U+2351 ⍒ U+2352 ⍓ U+2353 ⍔ U+2354 ⍕ U+2355 ⍖ U+2356
⍗ U+2357 ⍘ U+2358 ⍙ U+2359 ⍚ U+235A ⍛ U+235B ⍜ U+235C ⍝ U+235D ⍞ U+235E ⍟ U+235F ⍠ U+2360 ⍡ U+2361
⍢ U+2362 ⍣ U+2363 ⍤ U+2364 ⍥ U+2365 ⍦ U+2366 ⍧ U+2367 ⍨ U+2368 ⍩ U+2369 ⍪ U+236A ⍫ U+236B ⍬ U+236C
⍭ U+236D ⍮ U+236E ⍯ U+236F ⍰ U+2370 ⍱ U+2371 ⍲ U+2372 ⍳ U+2373 ⍴ U+2374 ⍵ U+2375 ⍶ U+2376 ⍷ U+2377
⍸ U+2378 ⍹ U+2379 ⍺ U+237A


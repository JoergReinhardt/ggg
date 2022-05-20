package main

type (
	Cnt func(as ...Item) (b Item, m Cnt) // continuation (monad|monoid)

	BindFnc func(ta, tb Monoton) (m Cnt)                          // bind applicative functors do define monad
	DoFnc   func(ta, tb Monoton, as ...Item) (a Item, ma, mb Cnt) // forall|forany a → ma → mb	Do∷(<<)|(>>)∷Then

	Zip func(...Item) (l, r Cnt)
)

func (c Cnt) Ident() Item    { return c }
func (c Cnt) Type() Identity { return Function }
func (c Cnt) Shape() T {
	return T{
		T{T{Optional, Category}, T{Optional, Function}}, // return type
		T{Optional, Category}}                           // argument type
}
func (c Cnt) Symbol() Str { return Str("⊥|*|[*] → ⨍ → *|⊥, ⨍|⊥") }

////////////////////////////////////////////////////////////////////////////////
// CONTINUITY FREE FUNCTIONS
//
func Condense(i Item, c Cnt) Cnt {
	return Cnt(func(args ...Item) (Item, Cnt) {
		if len(args) > 0 {
			return Condense(i, c)(args...)
		}
		return i, c
	})
}

// suspends application of arguments to continuation

func Suspend(c Cnt, spnd ...Item) Cnt {

	return Cnt(func(args ...Item) (Item, Cnt) {

		if len(args) > 0 {
			return Suspend(c, spnd...)(args...)
		}

		return c(spnd...)
	})
}

// class Functor f where
//    fmap :: (a -> b) -> f a -> f b
//
//main = putStrLn $ "hello " <> "there " <> "world!"
//
// FUNCTORMAP
//Functor map <$>
//
//(<$>) :: Functor f => (a -> b) -> f a -> f b
//(<$) :: Functor f => a -> f b -> f a
//($>) :: Functor f => f a -> b -> f b
//
//The <$> operator is just a synonym for the fmap function from the Functor
//	  typeclass. This function generalizes the map function for lists to
//	  many other data types, such as Maybe, IO, and Map.

// APPLICATIVE
// class (Functor f) => Applicative f where
//    pure :: a -> f a
//    (<*>) :: f (a -> b) -> f a -> f b

// MONOID
// class Monoid m where
//    mEmpty :: m
//    mAppend :: m -> m -> m
//    mConcat :: [m] -> m
//    mCompose = foldr mappend mempty
//

//(<>) :: Monoid m => m -> m -> m

//// CONTINUOUS COMPOSITION OPERATOR
// class Monad m where
//    return :: a -> m a
//    (>>=) :: m a -> (a -> m b) -> m b
//    (>>) :: m a -> m b -> m b
//    x >> y = x >>= \_ -> y
//    fail :: String -> m a
//    fail msg = error msg

// type Zipper_List a = ([a],[a])
//
// go_Forward :: Zipper_List a -> Zipper_List a
// go_Forward (x:xs, bs) = (xs, x:bs)
//
// go_Back :: Zipper_List a -> Zipper_List a
// go_Back (xs, b:bs) = (b:xs, bs)

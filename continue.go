package main

type (
	// CONTINUATION
	Cnt func(as ...Item) (b Item, m Cnt) // continuation (monad|monoid)

	// HIGHER ORDER FUNCTION COMPOSITION
	BindFnc func(ta, tb Monoton) (m Cnt)                          // bind applicative functors do define monad
	DoFnc   func(ta, tb Monoton, as ...Item) (a Item, ma, mb Cnt) // forall|forany a → ma → mb	Do∷(<<)|(>>)∷Then

	Zip func(...Item) (l, r Cnt)
)

// identity & instanciation
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
// resume continuation by evaluating its 'weak normal form', aka. application
// without any arguments, to reveal current head and its immediate successor.
func Resume(c Cnt) (Item, Cnt) { return c() }

// condense new head item with existing coninuation
func Condense(i Item, c Cnt) Cnt {
	return Cnt(func(as ...Item) (Item, Cnt) {
		if len(as) > 0 {
			return Concat(c, i)(as...)
		}
		return i, c
	})
}

// suspends application of arguments to continuation
func Suspend(c Cnt, args ...Item) Cnt {
	return Cnt(func(srgs ...Item) (Item, Cnt) {
		if len(srgs) > 0 {
			args = append(args, srgs...)
		}
		if len(args) > 0 {
			return c(args...)
		}
		return c()
	})
}

// consume arguments, apply tail function to head element and return resulting
// value and continuation
func Consume(a Cnt) (i Item, c Cnt) {
	if i, c = a(); c != nil && i != nil {
		return c(i)
	}
	if c != nil {
		return c()
	}
	return i, c
}

// reduce continuation by consuming arguments and condensing the results.
func Reduce(c Cnt, args ...Item) Cnt { return Condense(c(args...)) }

// cocatenate, by condensing passed continuation with first item in argument
// set recursively.
func Concat(c Cnt, args ...Item) Cnt {
	if len(args) > 0 {
		if len(args) > 1 {
			Concat(Condense(args[0], c), args[1:]...)
		}
		return Condense(args[0], c)
	}
	return c
}

func FoldR(acc, f Cnt, args ...Item) (i Item, t Cnt) {
skip:
	if f == nil {
		return acc()
	}
	i, f = f(args...)
	if i != nil {
		i, acc = acc(i)
	}
	if i == nil {
		goto skip
	}
	return i, Condense(FoldR(acc, f))
}

////////////////////////////////////////////////////////////////////////////////
// EXAMPLE FUNCTORMAP
//main :: IO ()
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

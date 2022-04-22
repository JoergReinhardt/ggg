package main

type (
	// CONTINUATION
	Cnt func(as ...Item) (b Item, m Cnt) // continuation (monad|monoid)

	// HIGHER ORDER FUNCTION COMPOSITION
	BindFnc func(ta, tb Monoton) (m Cnt)                          // bind applicative functors do define monad
	DoFnc   func(ta, tb Monoton, as ...Item) (a Item, ma, mb Cnt) // forall|forany a → ma → mb	Do∷(<<)|(>>)∷Then

	Zip func(...Item) (l, r Cnt)
)

func Declare(i Item, elem ...Identity) Cnt {
	return Cnt(func(args ...Item) (i Item, c Cnt) {
		if len(args) > 0 {
			if len(args) > len(elem) {
				// handle contruction of argument set being a multiple of segments.
				if i, c = i.Type().Continue(args[0:len(elem)]...); i != nil {
					if c != nil {
						return i, Condense(c(args[len(elem):]...))
					}
					return i, Seq(args[len(elem):]).Continue
				}
				return nil, Seq(args).Continue
			}
			// application of partial set of arguments to constructor
			if len(args) < len(elem) {
				if i, c = i.Type().Continue(args...); i != nil {
					if c != nil {
						return i, c
					}
					return i, Declare(i, elem[len(args):]...)
				}
				return nil, Seq(args).Continue
			}
			// construction of sequence of arguments matching the
			// number of segments, returns either result, or nil,
			// no need to test which.
			return i.Type().Continue(args[0:len(elem)]...)
		} // no arguments given → return item as instance and declare
		// another continuation. if length of segments is zero either
		// atomic declaration, or previously defined type, no further
		// instantiation needed, so instance is returned as is and
		// continuation is declared to be of type i, instantiated with
		// value of i
		return i, Declare(i, elem...)
	})
}

// Symbolization
func (c Cnt) Symbol() Str {
	return Str("(") + c.Return().Symbol() + ":" + Continuum.Symbol() + Str(")")
}

// identity & instanciation
func (c Cnt) Type() Identity { return Continuum }
func (c Cnt) Ident() Item    { return c }
func (c Cnt) Signature() T {
	i, cc := c()
	return T{i.Type(), cc.Type()}
}

// function instance
func (c Cnt) Return() Identity { return c }
func (c Cnt) Pattern() T       { return T{} }
func (c Cnt) Function() Fnc {
	return Fnc(func(as ...Item) Item {
		return Link(c(as...))
	})
}

// linked instance
func (c Cnt) Link() Lnk { return Link(c()) }

// listed instance
func (c Cnt) Head() Item { h, _ := c(); return h }
func (c Cnt) Tail() Lst  { _, d := c(); return d.List() }
func (c Cnt) List() Lst {
	return Lst(func() (Item, Lst) {
		h, t := c()
		return h, t.List()
	})
}

// yield evaluates the arguments, or empty call and only returns the resulting
// value, discarding the succeeding continuation.
func (c Cnt) Yield(args ...Item) Item {
	if len(args) > 0 {
		v, _ := c(args...)
		return v
	}
	return c.Head()
}

// progress evaluates arguments, or empty call in order to only progress and
// return continuation state, discarding the value if any resultet.
func (c Cnt) Progress(args ...Item) Cnt {
	if len(args) > 0 {
		_, p := c(args...)
		return p
	}
	_, p := c()
	return p
}

// append, compose, concat & continue methods all reference the instance they
// are method of, since it es expected to evaluate argument types and
// implement an instance of some type for any possible combination there of.
func (c Cnt) Append(xs ...Item) (Item, Cnt)   { return c(xs...) }
func (c Cnt) Compose(xs ...Item) (Item, Cnt)  { return c(xs...) }
func (c Cnt) Continue(xs ...Item) (Item, Cnt) { return c(xs...) }

////////////////////////////////////////////////////////////////////////////////
// CONTINUITY FREE FUNCTIONS
//
func Continue(a Cnt) (i Item, c Cnt) { i, c = a(); return i, c }

// resume continuation by evaluating its 'weak normal form', aka. application
// without any arguments, to reveal current head and its immediate successor.
func Resume(c Cnt) (Item, Cnt) { return c() }

// suspend continuation by taking tail & head (partially evaluated to 'head
// weak normal form') to construct the continuation that preceded it (btw.
// just so it has been mentioned: your usual time travel rules do of cause
// apply! so you better find out on how to acquire the plutonium necessary to
// generate those twenty one Gigawatts it needs to get you there in the first
// place, since don't forget that you can't do the lightning prediction thing
// the first time and don't forget to pick a copy of the appropriate periods
// lightning strike report… but than again, since you are probably reading
// this past 1985, i assume plutonium is readily supplied by any supermarket
// in your vicinity, in which case: don't bother)
// condense current & continuation to weak normal form of continuation
func Condense(i Item, c Cnt) Cnt {
	return Cnt(func(as ...Item) (Item, Cnt) {
		if len(as) > 0 {
			return Concat(c, i)(as...)
		}
		return i, c
	})
}

// consume arguments, apply tail function to head element and return resulting
// value and continuation
func Consume(c Cnt, args ...Item) (Item, Cnt) { return c(args...) }

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

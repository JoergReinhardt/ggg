package main

// FUNCTION $ ATTRIBUTE IDENTITY TYPES
//
// executable data types share certain aspects and distinct themselves in the
// ways they implement those attributes.  function attributes are part of
// function type definition. since every expression in functional programming
// is implemented by a function, function Attributes also are an integral part
// of every general expression, since anything expressed, needs some
// application of a function to act up on, in order to be evaluated.
//
// since functions are first class members all by themselves, functions may
// also be applied to functions, by the means of function operator application,
// so all higher order functional operators are also attributes of the
// functions they operate on.
//
type (
	TypeCons func(...Item) Identity
	Enc      Fnc

	Fnc   func(...Item) Item
	Const func() Item
	Una   func(Item) Item
	Bin   func(Item, Item) Item

	Fat Flag // ← flags aspects of function types…
	Fix Flag // ← ‥one of which is its fixity

	App      func(...Item) (Item, App)
	FMapApp  func(Applicable, Fnc) App
	ApplyFnc func(Applicable, ...Item) (Item, App)

	Cnt func(...Item) (Item, Cnt)

	BindFnc func(a, b Constructor) Cnt
	DoFnc   func(a, b Constructor, args ...Item) (Item, Cnt, Cnt) // forall a → ma → mb	Do ∷ (<<)|(>>)∷Then

	Zip func(...Item) (l, r Cnt)
)

func FoldMap(ac, id Cnt, f func(Item, Item) Item, as ...Item) Cnt {
	return Cnt(func(bs ...Item) (Item, Cnt) {
		if len(bs) > 0 {
		}
		return ac(as...)
	})
}
func IdMap(i Item, f Fnc) Cnt {
	return Cnt(func(as ...Item) (Item, Cnt) {
		return i, nil
	})
} // has been mapped

////////////////////////////////////////////////////////////////////////////////
// GENERIC FUNCTIION
//

// FUNCTION TYPE ATTRIBUTES
//
//go:generate stringer -type Fat
const (
	Expression Fat = 1 << iota
	Parametric
	Polymorph
	Constrain
	Argument
	Pattern
	Partial
	Reverse
	Lambda
	Fixity
	Result
	Assign
	Scope
	Apply
	Curry
	Bind
	Call
	Map
	Dot

	Functional = Expression | Parametric | Polymorph | Constrain |
		Pattern | Partial | Reverse | Lambda | Fixity | Result |
		Assign | Apply | Curry | Call | Dot

	Variable = Pattern | Constrain | Expression | Result

	Operators = Assign | Apply | Curry | Bind | Call | Map | Dot | Reverse

	Fixed = Parametric | Polymorph | Constrain | Partial | Operators

	Nop Fat = 0 // ⇐ Category.Function
)

func (t Fat) Ident() Item                         { return t }
func (t Fat) Type() Identity                      { return Function }
func (t Fat) Signature() T                        { return T{Functional} }
func (t Fat) Symbol() Str                         { return Str(t.String()) }
func (t Fat) Continue(xs ...Item) (i Item, c Cnt) { return i, c }

// FIXITY ATTRIBUTE
//
// fixity demarks the side(s) function arguments are expected to be applied
// change the fixity of the resulting function relative to the contextual
// expression, the function operator is been applied in.
//
//go:generate stringer -type Fix
const (
	Suffix Fix = 1 << iota
	Infix
	Postfix

	Nullary Fix = 0 // ← function takes no arguments

	Fixes = Suffix | Infix | Postfix
)

func (t Fix) Ident() Item                         { return t }
func (t Fix) Type() Identity                      { return Function }
func (t Fix) Signature() T                        { return T{Fixes} }
func (t Fix) Symbol() Str                         { return Str(t.String()) }
func (t Fix) Continue(xs ...Item) (i Item, c Cnt) { return i, c }

////////////////////////////////////////////////////////////////////////////////
// FUNCTION
// class Functor f where
//    fmap :: (a -> b) -> f a -> f b

// NOP
// no-op does not perform any operation and returns function with empty identity,
// return type and pattern.
func EmptyFnc() Fnc {
	return Define(func(...Item) Item {
		return EmptyFnc()
	}, Function, T{}, T{})
}

// FUNCTION IDENTITY
// returns all its arguments unaltered and in order.
func FunctionIdentity() Fnc { // when applied to arguments, function identity leaves them unchanged
	return Fnc(func(xs ...Item) Item {
		if len(xs) > 0 {
			if len(xs) > 1 {
				return Seq(xs)
			}
			return xs[0]
		}
		return T{Function, T{Category, T{Category}}}
	})
}

// FUNCTION DEFINITION
// defines a given function to be a function of the return type passed to
// constructor, taking arguments matching the pattern passed to contructor.
func Define(
	f func(...Item) Item,
	i Identity,
	r Identity,
	p ...Identity,
) Fnc {
	return Fnc(func(xs ...Item) Item {
		if len(xs) > 0 {
			return f(xs...)
		}
		// empty call can safely be overloaded to return type
		// signature, since defined functions aren't nullary
		// expressions.
		return T{r, T{i, T(p)}}
	})
}

func (f Fnc) Ident() Item      { return f() }
func (f Fnc) Type() Identity   { return Function }
func (f Fnc) Signature() T     { return f().(T) }
func (f Fnc) Return() Identity { return f.Signature()[0].(T) }
func (f Fnc) Identity() T      { return f.Signature()[1].(T)[0].(T) }
func (f Fnc) Pattern() T       { return f.Signature()[1].(T)[1].(T) }
func (f Fnc) Symbol() Str      { return f().Type().Symbol() }

func (f Una) Ident() Item    { return f }
func (f Una) Type() Identity { return Function }

func (f Bin) Ident() Item    { return f }
func (f Bin) Type() Identity { return Function }

func (f Const) Ident() Item    { return f }
func (f Const) Type() Identity { return Function }

// FUNCTION COMPOSITION AND APPLICATION
//($) :: (a -> b) -> a -> b
// suspended application
func SuspendApply(f Fnc, a Item) Fnc {
	return Define(func(as ...Item) Item { return f(append([]Item{a}, as...)...) },
		T{Infix, T{Apply, T{a.Type(), f}}}, f.Return(), f.Pattern())
}

// REVERSE SUSPENDED FUNCTION APPLICATION
//(&) : a -> (a -> b) -> b
func ReverseApply(f Fnc, a Item) Fnc {
	return Define(
		func(as ...Item) Item { return f(append([]Item{a}, as...)...) },
		T{Infix, T{Reverse | Apply, T{a.Type(), f}}}, f.Return(), f.Pattern())
}

// FUNCTION CONCATENATION (currying)
//(.) :: (b -> c) -> (a -> b) -> (a -> c)
// chained concatenation
func DotConcat(f, g Fnc) Fnc {
	return Define(
		func(as ...Item) Item { return g(f(as...)) },
		T{Infix, T{Apply | Dot, T{f, g}}}, f.Return(), g.Pattern(),
	)
}

// REVERSED FUNCTION CONCATENATION
func AndConcat(f, g Fnc) Fnc {
	return Define(
		func(bs ...Item) Item { return f(g(bs...)) },
		T{Infix, T{Apply | Dot | Reverse, T{f, g}}}, g.Return(), f.Pattern())
}

////////////////////////////////////////////////////////////////////////////////
// APPLICATIVE FUNCTOR
func (a App) Ident() Item                    { return a }
func (a App) Type() Identity                 { return Applicative }
func (a App) Function() Fnc                  { f, _ := a(); return f.(Fnc) }
func (a App) Pattern() T                     { return a.Function().Pattern() }
func (a App) Return() Identity               { return a.Function().Return() }
func (a App) Signature() T                   { return T{a.Return(), T{a.Function(), T{a.Pattern()}}} }
func (a App) Apply(args ...Item) (Item, App) { return a(args...) }
func (a App) FMap(b Fnc) App {
	// definition of apply function, applying its arguments to the function
	// passed to FMap as it's argument, and apply the resulting value to
	// Applicative instance.  resulting compound function takes values
	// matching b's parameter set to return values of a's return type.
	var f = Define(func(args ...Item) (i Item) {
		i, _ = a(b(args...))
		return i
	}, a.Return(), b, b.Pattern())

	return App(func(args ...Item) (Item, App) {
		if len(args) > 0 { // apply arguments to compound function
			if i := f(args...); i != nil { // return anything yielded
				return i, nil // (left value && !Applicative ∷ *SUCCESS*)
			} // return unused arguments & preceeding applicative,
			// if no result was yielded. (args > 0 && left && right ∷ *PARTIAL*)
			return Seq(args), a
		} // applied without args returns applicative & current function
		return f, a
	})
}

////////////////////////////////////////////////////////////////////////////////
// GENERIC (MONAD) CONTINUATION
//
// continuation types 'Type()' method can't be overload to reveal it's type,
// but itself implements the methods expected to be defined for a 'Fnc'
// instance and defines another method to be returned as such.
func ConsT(
	i Identity,
	r Identity,
	c Cnt,
	s ...Identity,
) Constructor {
	return T{i, T{r, T(s)}, c}
}
func (c Cnt) Type() Identity { return Continuum }
func (c Cnt) Ident() Item    { return c }
func (c Cnt) Signature() T {
	i, cc := c()
	return T{i.Type(), cc.Type()}
}

// symbolization
func (c Cnt) Symbol() Str {
	return Str("(") + c.Return().Symbol() + ":" + Continuum.Symbol() + Str(")")
}

// function instance
func (c Cnt) Return() Identity { return c }
func (c Cnt) Pattern() T       { return T{} }
func (c Cnt) Function() Fnc {
	return Fnc(func(as ...Item) Item {
		return Link(c(as...))
	})
}

// linked pair instance
func (c Cnt) Link() Lnk { return Link(c()) }

// list instance
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
func Continue(c Cnt) (Item, Cnt) { return c() }

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
func Suspend(c Cnt, i Item) Cnt { return Condense(i, c) }

// condense head & tail to weak normal form of continuation
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

// right fold continuity takes a left and right continuity argument and
// optionally additional arguments that may get passed, during succeeding
// invocation(s) of the resulting continuation.  every further invocation will
// apply the head, yielded by right continuation, and any arguments possibly
// passed with the invocation, to apply it (them) as argument(s) to the left
// continuation.  foldr returns the weak head normal form constructed of
// remaining continuation by applying lefts left (head) value, _if_ any was
// yielded(!), eventual arguments and the remaining/resulting continuations,
// recursively to foldr again.  for invocations that yield a left result it
// gets returned immediately together with suspended remainder of the fold
// operation.  if no left result was yielded, yet none of the continuations
// depleted, fold continues by skipping yield/return on all frames that return
// right results exclusively.  that way inner function definition decides about
// eager vs. lazy execution, by either lazyly returning intermediate results
// and suspending, or instead keep accumulating, blocking the caller only ever
// returning once all initial arguments and/or left and/or right continuation
// deplete. (equals eager evaluation under cps). invocation of the inner
// function (tail recursion optimization).
func FoldR(l, r Cnt, args ...Item) Cnt {
	var lh, rh Item
skip:
	if r != nil { //UNIT
		rh, r = r(args...) // call right, apply any args
		if rh != nil {
			lh, l = l(rh)  // apply result to left
			if lh != nil { // RECURSIVE LAZY FOLD (yield and suspend)
				return Condense(lh, FoldR(l, r))
			} // SKIPPED EAGER FOLD (skip & continue)
			goto skip
		} // FAILURE (return arguments to caller)
		return Condense(nil, Seq(args).Continue)
	} // FINAL (ACCUMULATED) RESULT
	return Condense(l, nil)
}

// continuation passing style apparently allows for lazy evaluation of left
// fold (right bound), for infinite lists, i guess?  and what could that turn
// out to even mean, when values are applied?? TODO: Tests!
func FoldL(l, r Cnt, args ...Item) Cnt {
	var lh, rh Item
skip:
	if l != nil { //UNIT
		lh, l = l() // call right, apply any args
		if lh != nil {
			rh, r = r(lh)  // apply result to left
			if rh != nil { // RECURSIVE LAZY FOLD (yield and suspend)
				return FoldL(l, Condense(rh, r))
			} // SKIPPED EAGER FOLD (skip & continue)
			goto skip
		} // FAILURE (return arguments to caller)
		return Condense(Seq(args), nil)
	} // FINAL (ACCUMULATED) RESULT
	return Condense(nil, r)
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

package main

/// HIGHER ORDER PRIMITIVES
type (
	Dec T // TYPE DECLARATION
	Def T // FUNCTION DEFINITION

	// CONTINUATION CONSTRUCTORS
	Update func(Continue, ...Item) (Item, Continue)
	Lift   func(Item, ...Item) Continue

	Application func(Item, Item, ...Item) (Item, Item, Fnc)

	// APPLICATIVE
	Traverse  func(Item, Traversal) Continue
	Traversal func(Item, Item, ...Item) (Item, Item, Continue)

	// COMPOSEABLE
	BindM func(
		left Continue,
		right Continue,
		bind BindFnc,
		local ...Item,
	) Continue

	BindFnc func(
		left Item,
		right Item,
		args ...Item,
	) (Continue, Continue, Item)

	ArrowT   func(Continue, Continue, ArrowFnc) Continue
	ArrowFnc func(
		left Continue,
		right Continue,
		args ...Item,
	) (
		Item, Item,
		Continue, Continue,
	)
)

// FUNCTION DEFINITION (SIGNATURE TYPE)

// functions allways take arguments and return function identity on empty call.
// constants are of another type.
func Define(t T, vars ...Def) (d Def) { return d }
func (f Fnc) Signature() T            { return f().(T) }
func (f Fnc) Type() T                 { return T{Function} }
func (f Fnc) Ident() T                { return f.Signature()[0].(T) }
func (f Fnc) Symbol() Str             { return Str("") }

// FUNCTIONAL TYPE SUBTYPES
type (
	FnT Flag // ← flags aspects of function types…
	Fix Flag // ← ‥one of which is its fixity
)

//  - function type aspects are elements of the function type.
//
//  - the identity of the entire function type is defined by all aspects of the
//    function type (OR concatenated).
//
//  - the identity of an isntance is the instance itself.
//
//  - the symbol for function aspects identity is 'Functional'…
//
//  - ‥which is the signature of all instances of function type aspect flags.
func (t FnT) Ident() Item  { return t }
func (t FnT) Type() T      { return T{Function} }
func (t FnT) Signature() T { return T{Functional} }
func (t FnT) Symbol() Str  { return Str(t.String()) }

//go:generate stringer -type FnT
const (
	Expression FnT = 1 << iota // computation on elements of functional
	Constraint                 // parameter condition respected by pattern matching
	Parameter                  // parameter defines arguments to be invalid, or viable for application.
	Argument                   // argument of parameter in expression
	Partial                    // partialy applied function, bound arguments and residual parameter set
	Lambda                     // anonymous function defined within expression, closing over scope
	Fixity                     // pre-, in-, or postfix
	Result                     // return result to caller
	Assign                     // binds result to name
	Scope                      // all names in expression context
	Apply                      // ‥left…
	Curry                      // function composition (infix composition) ∷ <·>
	Call                       // ‥, or right…
	Dot                        // concatenate (low prescedence apply)      ∷ <$>

	Functional = Expression | Constraint | Parameter | Argument | Partial |
		Lambda | Fixity | Result | Assign | Apply | Curry | Call | Dot

	Variable = Parameter | Constraint | Expression | Result

	FncOps = Assign | Apply | Curry | Call | Dot

	Nop FnT = 0 // ⇐ Category.Function
)

//go:generate stringer -type Fix
const (
	Suffix Fix = 1 << iota
	Infix
	Postfix

	Nullary Fix = 0 // ← function takes no arguments

	Fixities = Suffix | Infix | Postfix
)

//  - fixity is an element of the 'Functions' (mind the 's'!) type, aka a
//    functional aspect.
//
//  - the type of fixity (the int) is 'Funtional' (identity of 'Functions',
//    subtype of 'Function')
//
//  - the identity of the entire fixity type is 'Fixities', the instance
//    identity is the instance itself
func (t Fix) Ident() Item  { return t }
func (t Fix) Type() T      { return T{Functional} }
func (t Fix) Signature() T { return T{Fixities} }
func (t Fix) Symbol() Str  { return Str(t.String()) }

// apply   ident   aggregator						argset	 ApplyFnc	  ident aggregated
// ---------------------------------------------------------------------------------------------------------------------
// apply → Item → Option.(Item → Either|Or → Either.Item|Or.List) → Item… → Function∷apply → Item Option.Item|List
//

type FoldFncEager func(
	acc Item,
	cur Item,
	args ...Item,
) (accumulated Item)
type FoldMapFncEager func(
	acc Item,
	cur Item,
) (accumulated Item)
type FoldNFncEager func(
	acc Item,
	iter Iterator,
	args ...Item,
) (
	accumulated Item,
	reduced Iterator,
)
type FoldMapNFncEager func(
	acc Item,
	iter Iterator,
) (
	accumulated Item,
	reduced Iterator,
)
type FoldMapFnc func(
	acc Item,
	cur Item,
) (
	intermediate Item,
	accumulated Item,
)
type FoldNFnc func(
	acc Item,
	iter Iterator,
	args ...Item,
) (
	intermediate Item,
	accumulated Item,
	reduced Iterator,
)
type FoldMapNFnc func(
	acc Item,
	iter Iterator,
) (
	intermediate Item,
	accumulated Item,
	reduced Iterator,
)

/// APPLICATIVE
//
// ApplyFnc(identity, aggregator, local, ...arguments) ∷ local, aggregator, identity
func (f Traversal) Ident() Item { return f }
func (f Traversal) Type() T     { return T{Functor} }

// ApplyF(ApplyFnc, identity, temporary, ...locals) ∷ Continuation
func (f Traverse) Ident() Item { return f }
func (f Traverse) Type() T     { return T{Functor} }

func ApplyF(
	apply Traversal,
	ident,
	tempo Item,
	locals ...Item,
) Continue {

	var local Item

	if len(locals) > 0 {
		local = Seq(locals)
	}

	return Continue(func(args ...Item) (Item, Continue) {

	skip:
		if ident == nil {
			return tempo, ApplyF(apply, ident, tempo, local)
		}

		if len(args) > 0 {
			if local != nil {
				args = append(local.(Seq), args...)
			}
			if len(args) > 1 {
				local, tempo, ident = apply(ident, tempo, args...)
				if local == nil {
					goto skip
				}
				return tempo, ApplyF(apply, ident, tempo, args...)
			}
			local, tempo, ident = apply(ident, tempo, args[0])
			if local == nil {
				goto skip
			}
			return tempo, ApplyF(apply, ident, tempo, args...)
		}
		local, tempo, ident = apply(ident, tempo)
		if local == nil {
			goto skip
		}
		return tempo, ApplyF(apply, ident, tempo, locals...)
	})
}

//// CONTINUOUS COMPOSITION OPERATOR
func ComposeM(
	bind BindFnc,
	left, right Continue,
	locals ...Item,
) Continue {

	var (
		uleft     = left
		uright    = right
		immediate Item
	)

	if len(locals) > 0 {
		immediate = Seq(locals)
	}

	return Continue(func(args ...Item) (ident Item, tail Continue) {

	skip:
		if len(args) > 0 {
			args = append(immediate.(Seq), args...)
		}

		left, right, immediate = bind(uleft, uright, args...)
		if immediate != nil {
			goto skip
		}
		return immediate, ComposeM(bind, left, right, immediate)
	})
}

// CONTINUATION DEFINITION
func (c Continue) Ident() Item { return c }
func (c Continue) Type() T     { i, _ := c(); return T{Monad, i.Type()} }
func (c Continue) Signature() T {
	i, _ := c()
	return T{Function, T{Option, i.Type()}, T{i.Type(), T{Monad, i.Type()}}}
}

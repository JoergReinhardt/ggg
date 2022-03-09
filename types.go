package main

import (
	"fmt"
	"math/bits"
	"strconv"
	"strings"
	"unicode"
)

// TYPE CLASSES
//  haskel like type classes are implemented by golang interface types.
type (
	// ITEM
	//  all objects implement the item interface, which is defined by a
	//  method to return underlying object identity (its value) and another
	//  method to reveal the objects internal library type.
	Item interface {
		Ident() Item
		Type() T
	}

	// TYPE
	//
	//  Identity() ParentType|None (root level category → Ident == Identity)
	//
	//  Signature() → slice of member element type(s), comma separated for
	//  tuple types, nested for parametric constructors, both for optional,
	//  alternative, or otherwise polymorph types.
	//
	//  Cons(...Item) (Item, Continue) → data/type constructor
	Identity interface {
		Item          // Ident() → particular T & Type() parent T|Empty
		Signature() T // subtypes of this type
		Symbol() Str
	}

	// SEQUENCE OF TYPE SYMBOLS
	T []Identity
	// FUNCTOR INTERFACES]
	//
	//  the functor interface:
	//
	// ordinary variables are lifted into encapsulated functor exexution
	// context, by composing them together with some function, that
	// generates the resulting value from any value assigned to the
	// variable, to yield a mapped derivate for any possible element of the
	// variables type, while keeping the referential integrity to the
	// original instance, by not changing its identity, yet updating the
	// type of the value to reflect the resulting type.
	//
	//    Identity
	//      fmap id == id
	//
	//    Composition
	//      fmap (f·g) == fmap f · fmap g
	//
	//    (<&>) infix operator for fmap
	//
	// FUNCTION
	Fnc        func(...Item) Item
	ComposeFnc func(f, g Fnc) Fnc

	// COMPOSEABLE
	ComposeItems func(a, b Item) Composeable
	Composeable  interface {
		Compose(Item) Composeable
	}

	// MAPABLE
	FMapIter func(Iterator, Fnc) Mapable
	FMapItem func(Item, Fnc) Mapable
	FMapFnc  func(f, g Fnc) Mapable
	Mapable  interface {
		FMap(Fnc) Mapable
	}
	FMap   func(Mapable, Fnc) Mapable
	ForAll func(Mapable, Fnc) Item

	// TYPE PROJECTED INTO THE FUNCTOR DOMAIN
	//
	// every type implementing the
	// necessary methods can be mapped to the interface type by means of
	// method expression.
	FoldF    func(Composeable, ...Item) (Item, Composeable)
	FoldMap  func(Composeable, FoldF) Foldable
	Foldable interface {
		Composeable
		FoldMap(Fnc) Foldable
		Fold(...Item) (Item, Foldable)
	}
	FoldFnc func(Foldable, ...Item) (Item, Foldable)
	ForEach func(Foldable, ...Item) Foldable
	ForAny  func(Foldable, ...Item) Item

	// APPLICATIVE
	//
	//   data MyState = MyState {arg1 :: Foo, arg2 :: Bar, arg3 :: Baz}
	//
	//   produceFoo :: Applicative f => f Foo
	//   produceBar :: Applicative f => f Bar
	//   produceBaz :: Applicative f => f Baz
	//
	//   mkState :: Applicative f => f MyState
	//   mkState = MyState <$> produceFoo <*> produceBar <*> produceBaz
	FApply     func(Foldable, Fnc) Applicable
	Applicable interface {
		Foldable
		ApplyF(Fnc) Applicable
		Apply(...Item) (Item, Applicable)
	}
	ApplyFnc func(Applicable, ...Item) (Item, Applicable)
	ExecFnc  func(Applicable, ...Item) Applicable // a → fa → fb	    ∷ (<*)
	EvalFnc  func(Applicable, ...Item) Item       // a → fa → fb → b    ∷ (*>)

	// SIGNATURE TYPE
	//
	//  signature returns a slice of types expressing the shape of the
	//  type, with tuples being comma separated and subtypes being nested.
	//
	//    [ElemType<,SecondElemType,…|[SubType<,…>]>]

	// QUALIFIED
	//  ‥qualified to be equality relation…
	EqualFnc  func(a, b Item) Bool
	Qualified interface { // Qualified a
		Item
		Equal(Item) Bool // a → a → Bool (True|False)
	}

	// ORDERED
	//  ‥ordered by comparison of rank relation…
	Ordered interface { //
		Qualified
		Lesser(Item) Bool
		Greater(Item) Bool
	}

	// SEQUENCE OF ITEMS
	Seq []Item // cons∷f(*…)→v|pure∷f(v)→*…|unit∷f(v *ₜ)→v
	// ENUMERATED
	//  lists, sequences, type signatures, and all generators in general
	//  implement iterator.
	EmptyS     func() []Item
	Serialize  func(Sequential, ...Item) Sequential
	Sequential interface {
		Len() Int
		Pick(Int) Item
		Range(s, e Int) Sequential
	}
	// LINKED PAIR
	Lnk func() (Item, Item) // (a,b)

	// LINKED LIST
	Lst func() (Item, Lst) // (x,xs)

	Iterate  func(Iterator) (Item, Iterator)
	Concat   func(Iterator, ...Item) Iterator
	Iterator interface {
		Next() (Item, Iterator)
		Empty() Bool
	}

	// return and take handle tuple & enum construction. signature length
	// is known by declaration. arguments are applied one after another to
	// return fnc, and if validated, are applied to return partially
	// applied mutuals until all parameters are satisfied, and/or arguments
	// fail to validate (depends on if the type is additive|exclusive), at
	// which point a mutual instance, ot nothing is returned as a resulting
	// value.
	//
	// take implementation may also concatenated mutual instance
	// constructors, suspending evaluation, allowing for type level
	// operations like redistribution of indeterminism's from 'slice of
	// undetermined instances' to 'undetermined instance of slice' aka
	// applicative functor.
	Return func(Return, T) Mutual          // ← returns current instance ⇒ Mutual|Partial∙Mutual
	Take   func(Return, T, ...Item) Return // ← concat arguments with instance ⇒ suspended Return construction according to signature
	Mutual interface {
		Take(...Item) Return
		Return() Mutual
	}

	// guarded by some praedicate regarding instance values.
	ConsGuard func(
		test ReturnBool, // test scrutinizes argument
		comp BoolOp, // bool operation composes guards
		prae Item, // praedicate is compared against argument
	) (Item, Bool, Guard)
	Guard interface {
		Guard(...Item) (Item, Bool, Guard)
	}
	ComposeGuards func(...Guard) Guard

	// CONTINUATION
	//  implemented by all higher order types
	ToBeCont     func() Continuation
	ConcCont     func(i, j Iterator) Continuation
	Continuation interface {
		Continue(...Item) (Item, Continue)
	}
	Next      func(Continuation, ...Item) Continue // compute continuation, discard value
	Current   func(Continuation, ...Item) Item     // compute value, discard continuation
	Suspend   func(Continuation, ...Item) Continuation
	Construct func(Continuation, ...Item) (Item, Continuation) // continuation constructor
	Continue  func(...Item) (Item, Continue)                   // continuation closure

	// MONOID
	//  instances of monoidal type can be concatenated, or accumulated with
	//  other instances of the same type… i.e.: Lists can be concatenated,
	//  to return another lists, numbers be added and/or multiplied, to
	//  return another number of the same type.
	//
	//  monoidal types adhere to monoid laws:
	//
	//    (x <> y) <> z = x <> (y <> z) -- associative
	//    mempty <> x = x               -- left identity
	//    x <> mempty = x               -- right identity
	//
	Modal interface {
		Ordered
		Min() Int
		Max() Int
		Unit() Item      // [a] ↔ a
		EmptyMod() Modal // _ → [ ]
	}

	GuardTake func(Guard, Take) Take

	Traversable interface { // Monoid a
		Modal
		Foldable
		Traverse(Fnc) Traversable
		Serialize(Fnc) Sequential
	}

	// MONADIC
	//
	Pure    func(Monadic, ...Item) Monadic
	Bind    func(Fnc, Applicable, Applicable) Monadic // compose monadic operations
	Monadic interface {
		Applicable       // PureA == ReturnM
		Then(Item) Item  // forall a b·ma → mb → mb → b ∷ (>>)
		Do(Item) Monadic // forall a → ma → mb		 ∷ (<<)
	}

	StateFnc func(Stateful, ...Item) (Item, Stateful)
	Stateful interface {
		Monadic
		Run(...Item) (Item, StateFnc)
	}

	// CATEGORY LABLE
	Index Int
	Flag  Unt
	Name  Str
)

func IndexFromFlag(f Flag) Index  { return Index(f.Len()) }
func (i Index) Ident() Item       { return i }
func (i Index) Type() T           { return T{} }
func (i Index) Signature() T      { return make(T, 0, int(i)) }
func (i Index) Symbol() Str       { return Str("[" + fmt.Sprintf("%d", int(i)) + "]") }
func (i Index) FtoI(f Flag) Index { return Index(f.Len()) }
func (i Index) Flag() Flag        { return Flag(1 << i) }
func (i Index) Cons(args ...Item) (Item, Continue) {
	if len(args) > 0 {
		// multiple arguments…
		if len(args) > 1 {
			// ‥equal in magnitude to cardinality of index…
			if len(args) == int(i)+1 {
				// ‥assume this to be an order to construct
				// slice of indices that equals cardinality of
				// index and number of parameters in length,
				// from arguments passed. (i.e. make shur the
				// exact number expected gets taken)
				return Seq(args), nil
			}
			// ‥requested magnitude is over satisfied…
			if len(args) > int(i)+1 {
				// ‥take requested number of arguments as
				// result and continue on remaining arguments.
				return Seq(args[:int(i)+1]), Seq(args[int(i)+1:]).Cons
			}
			// ‥requested magnitude has not been satisfied (yet)…
			return nil, Seq(args).Cons
		}
		// single argument expected to be of type index…
		if idx, ok := args[0].(Index); ok {
			// which turns out to be of type index, gets returned,
			// no continuation.
			return idx, nil
		}
		// or type flag…
		if flg, ok := args[0].(Flag); ok {
			if flg.Composed() {
				if idx := Index(magnitude(flg)); idx == i {
					return idx, nil
				}
			}
			if idx := Index(cardinality(flg)); idx == i {
				return idx, nil
			}
		}
		return nil, Seq(args).Cons
	}
	return i, (i + 1).Cons
}

// Contain(...Type) Type ∷ (T₁|T₂|…|Tₙ)|(T₁ T₂ … Tₙ)
//
//  construct type container for sum, or product types, defined by type
//  constructors passed as arguments to be contained.
func Contain(fs ...Identity) Identity { return T(fs) }

//////////////////////////////////////////////////////
//// SIGNATURE
///
//    signature type is a sequence of type instances, possibly recursive to
//    encode the types data structure.
func (s T) Ident() Item { return s }

// Signature·Type → Type
//
//  type returns signatures first element, if there is one, or nil, if this is
//  the empty signature.
func (s T) Type() T {
	if len(s) > 0 {
		return T{s[0]}
	}
	return T{None}
}
func (s T) Len() Int { return Int(len(s)) }

// Signature·Signature → Type
//
// signature returns either the remainder of elements following the first
// element, wrapped in a signature in case a single element remains and an
// empty signature in case there isn't a remainder.
func (s T) Signature() T {
	if len(s) > 0 {
		if len(s) > 1 {
			return T(s[1:])
		}
		return T{s[0]}
	}
	return T{}
}

// Signature·Empty → Bool
//
// empty praedicate returns true, if signature doesn't contain any elements.
func (s T) Empty() Bool { return len(s) == 0 }

// Signature·Single → Bool
//
// empty praedicate returns true, if signature consists of a single element.
func (s T) Single() Bool { return len(s) == 1 }

// Signature·Double → Bool
//
// empty praedicate returns true, if signature consists of two single elements.
func (s T) Paired() Bool { return len(s) == 2 }

// Signature·Enum → Bool
//
// empty praedicate returns true, if signature contains more then two elements.
func (s T) Many() Bool { return len(s) > 2 }

// Signature·Iterate → Identity Type
//
//  iterate yields first element of signature, or nil, for the first field of
//  the return value pair and a list of remaining elements for the second
//  field.
func (s T) Next() (Item, Iterator) {
	if len(s) > 0 {
		if len(s) > 0 {
			return s[0], s[1:]
		}
		return s[0], T{}
	}
	return T{}, T{}
}

// Signature·First → None|Identity
//
//  first element is either empty, in which case nil is returned, or the first
//  element in signature, conscidered to be the type expressed by this
//  signature.
func (s T) First() Item {
	if len(s) > 0 {
		return s[0]
	}
	return T{}
}
func (s T) Remaining() T {
	if len(s) > 1 {
		return T{s[1:]}
	}
	return T{}
}
func (s T) Last() T {
	if s.Len() > 0 {
		return s[s.Len()-1].(T)
	}
	return T{}
}
func (s T) Preceding() T {
	if len(s) > 1 {
		return T{s[:len(s)-1]}
	}
	return T{}
}

// Signature·Second → None|Identity|Identity.Type
//
//  second element is either empty, in which case nil is returned, a single
//  type element, which gets returned as such, or multiple elements, wich will
//  be returned as a new signature expressing the subtype of the type yielded
//  by the First() method.
func (s T) Second() Item {
	if len(s) > 1 {
		return s[1]
	}
	return T{}
}

// Signature·Sequence → Sequence
//
//  sequence transforms the type fields of signatue, to instances of ident and
//  wraps them in a flat sequence.
func (s T) Sequence() Seq {
	c := make(Seq, 0, len(s))
	for _, e := range s {
		c = append(c, e)
	}
	return c
}

// Signature·Flip → Sequence
//
//  flip returns signatures fields converted to instances of ident and in
//  reversed order.
func (s T) Flip() T {
	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		s[i], s[j] = s[j], s[i]
	}
	return s
}

// Signature·Labels → [String]
//
//  labels returns signature fields printable symbols.
func (s T) Labels() []Str {
	var names = make([]Str, 0, len(s))
	for _, t := range s.Signature() {
		names = append(names, t.(Identity).Symbol())
	}
	return names
}

// Signature·Labels → [string]
//
//  labels returns signature fields printable symbols.
func (s T) strings() []string {
	var names = make([]string, 0, len(s))
	for _, t := range s.Signature() {
		names = append(names, string(t.(Identity).Symbol()))
	}
	return names
}

// Signature·Symbol → Text
//
//  symbol yields printable representation of this signature.
func (s T) Symbol() Str {
	if o, _ := Option.Cons(s.Type()); o != nil {
		if len(s.Signature()) > 0 {
			return o.Type().Signature()[0].Symbol() + Str("|⊥")
		}
		return Category.Symbol() + Str("|⊥")
	}
	if len(s.Signature()) > 0 {
		if len(s.Signature()) > 1 {
			if p := s.Type().Type(); p != nil {
				return p.Symbol() + "·[" + Str(
					strings.Join(p.Signature().strings(), "|")) + "]"
			}
		}
		return s.Type().Symbol() + "·" + s.Signature()[0].Symbol()
	}
	return s.Type().Symbol()
}

// Signature·Cons → Identity Composed
//
//  safely return first two elements as single element and composit of the
//  remaining elements in signature.
func (s T) Cons(args ...Item) (i Item, c Continue) { return i, c }

func (f Take) Type() T     { return }
func (f Take) Ident() Item { return }

func (f Return) Type() T     { return }
func (f Return) Ident() Item { return }

// NAME
//
//  name may be the name of a composed type.  composed names are delimited by
//  an delimiter and|or whitespace.  splitName splits composed names
//  accordingly and strips delimiters and whitespaces in the process, to return
//  a slice of string utilized by name methods.
func splitName(name Name) (names []string) {
	var s = string(name)
	switch {
	case strings.Contains(s, "·"):
		names = make([]string, 0, 1+strings.Count(s, "·"))
		for _, e := range strings.Split(s, "·") {

			names = append(names, strings.TrimSpace(e))
		}
	case strings.Contains(s, "|"):
		names = make([]string, 0, 1+strings.Count(s, "|"))
		for _, e := range strings.Split(s, "|") {
			names = append(names, strings.TrimSpace(e))
		}
	case strings.Contains(s, ","):
		names = make([]string, 0, 1+strings.Count(s, ","))
		for _, e := range strings.Split(s, ",") {
			names = append(names, strings.TrimSpace(e))
		}
	case strings.Contains(s, "."):
		names = make([]string, 0, 1+strings.Count(s, "."))
		for _, e := range strings.Split(s, ".") {
			names = append(names, strings.TrimSpace(e))
		}
	case strings.Contains(s, ";"):
		names = make([]string, 0, 1+strings.Count(s, ";"))
		for _, e := range strings.Split(s, ";") {
			names = append(names, strings.TrimSpace(e))
		}
	case strings.ContainsAny(s, " "):
		names = make([]string, 0, 1+strings.Count(s, " "))
		for _, e := range strings.Split(s, " ") {
			names = append(names, e)
		}

	default: // camelCase
		var camel func([]string, string) []string
		camel = func(ss []string, s string) []string {
			w := []rune(s)
			for i := len(w) - 1; i > 1; i-- {
				if unicode.IsUpper(w[i]) {
					ss = append(ss, string(w[:i]))
					if i < len(w)-1 {
						ss = camel(ss, string(w[:i]))
					}
				}
				ss = append(ss, string(w))
			}
			return ss
		}
		names = camel([]string{}, s)
	}
	return names
}

func (n Name) Ident() Item  { return n }
func (n Name) Type() T      { return T{n} }
func (n Name) Signature() T { return T{Category, n} }
func (n Name) Symbol() Str  { return Str(n) }
func (n Name) Atom() Bool   { return !n.Composed() }
func (n Name) Composed() Bool {
	return Bool(strings.ContainsAny(string(n), "·|,.; "))
}

func (n Name) Names() []Name {
	if n.Atom() {
		return []Name{n}
	}
	var (
		ns    = splitName(n)
		names = make([]Name, 0, len(ns))
	)
	for _, name := range ns {
		names = append(names, Name(name))
	}
	return names
}

func (n Name) Split() T {
	if n.Atom() {
		return T{n}
	}
	var (
		ns = splitName(n)
		ss = make(T, 0, len(ns))
	)
	for _, s := range ns {
		ss = append(ss, Name(s))
	}
	return T{Name("")}
}

// implement ordered lexicaly by name behaviour
func (n Name) Contains(s Str) Bool { return Bool(strings.Contains(string(n.Symbol()), string(s))) }
func (n Name) Compare(a Name) Int  { return Int(strings.Compare(string(n), string(a))) }
func (n Name) Lesser(a Name) Bool  { return Bool(strings.Compare(string(n), string(a)) < 0) }
func (n Name) Eq(a Name) Bool      { return Bool(strings.Compare(string(n), string(a)) == 0) }
func (n Name) Len() int {
	if n.Composed() {
		return len(splitName(n))
	}
	if n == Name("") {
		return 0
	}
	return 1
}

// Name·Cons(...Ident) → Identity Continue
//
//  default behaviour of name cons called empty, is to return the instance and
//  it's cons method.  when called with arguments, cons will test those to be
//  of kind category, or class and validate category, or class symbol to either
//  be contained within n, or not, to return the first argument validated to
//  instanciate type & either nil if single argument was given, or succeeding
//  arguments enclosed in a continuation constructor, in case multiple
//  arguments where passed.
func (n Name) Cons(args ...Item) (Item, Continue) {
	if len(args) > 0 {
		// validate first argument to be either category, or class
		// is not contained in n, or no label at all → return nil and
		// all arguments.
		return nil, Seq(args).Cons
	}
	// empty call → name & continuation
	return n, n.Cons
}

////////////////////////////////////////////////////////////////////////
//// FLAG SET
///
// FLAG SET FREE FUNCTIONS
//
// bitwise set operations to be applied as free functtions and reused as
// methods on flag sets.
func cardinality(f Flag) int    { return bits.Len(uint(f)) }
func magnitude(f Flag) int      { return bits.OnesCount(uint(f)) }
func atomic(f Flag) Bool        { return bits.OnesCount(uint(f)) == 1 }
func composed(f Flag) Bool      { return bits.OnesCount(uint(f)) > 1 }
func contains(s, f Flag) Bool   { return s&f != 0 }
func intersects(s, f Flag) Flag { return s & f }
func unify(s, f Flag) Flag      { return s | f }
func toggle(s, f Flag) Flag     { return s ^ f }
func clear(s, f Flag) Flag      { return s &^ f }
func iterate(fs []Flag) (Flag, []Flag) {
	if len(fs) > 0 {
		if len(fs) > 1 {
			return fs[0], fs[1:]
		}
		return fs[0], nil
	}
	return Flag(0), nil
}
func split(t Flag) []Flag {
	var (
		u = uint(t)
		l = int(cardinality(Flag(u)))
		s = make([]Flag, 0, l)
	)
	for i := 0; i < l; i++ {

		// rotate argument and test if last bit is set…
		if u = bits.RotateLeft(u, -1); bits.TrailingZeros(u) != 0 {
			// ‥allocate and append appropriate flag.
			s = append(s, Flag(bits.RotateLeft(1, i)))
		}
	}
	// return slice of contained flags
	return s
}

func (f Flag) Ident() Item { return f }
func (f Flag) Type() T     { return T{f} }
func (f Flag) Signature() T {
	if f.Atom() {
		return T{f}
	}
	return f.Split()
}
func (f Flag) Eq(a Flag) Bool       { return Bool(f == a) }
func (f Flag) Lesser(a Flag) Bool   { return Bool(f < a) }
func (f Flag) Contains(a Flag) Bool { return contains(f, a) }
func (f Flag) Rank() Int            { return Int(cardinality(f)) }
func (f Flag) Len() Int             { return Int(magnitude(f)) }
func (f Flag) Atom() Bool           { return Bool(atomic(f)) }
func (f Flag) Composed() Bool       { return Bool(!atomic(f)) }
func (f Flag) Flag() Flag           { return f }
func (f Flag) Symbol() Str          { return Str("Flag∷" + strconv.FormatInt(int64(f), 2)) }
func (f Flag) Split() T {
	if cardinality(f) > 1 {
		var (
			s = split(f)
			v = make(T, 0, len(s))
		)
		for _, e := range s {
			v = append(v, e)
		}
		return v
	}
	return T{f}
}
func (f Flag) Cons(args ...Item) (i Item, c Continue) {
	if len(args) > 0 {
		// validate first argument to be a flag…
		if flag, ok := args[0].(Flag); ok {
			// handle multiple arguments…
			if len(args) > 1 {
				// ‥construct an instance of the type expressed
				// in first argument & use its constructor to
				// parameterize remaining arguments.
				if i, c = f.Cons(flag); i != nil {
					return i, Continue(func(next ...Item) (Item, Continue) {
						if len(next) > 0 {
							return i.(Flag).Cons(
								append(args, next...)...)
						}
						return i.(Flag).Cons(args...)
					})
				}
				// ‥argument is a flag not contained in f → no
				// instance & sequence of arguments
				return nil, c
			}
			// validate flag to be contained in f…
			if f.Contains(flag) {
				// ‥return argument validated to be some flag
				// as identity.  don't return continuation,
				// more arguments to return.
				return args[0], nil
			}
		}
		// first argument did not validate as flag and|or instance
		// construction failed → don't return identity, continue with
		// arguments.
		return nil, Seq(args).Cons

	}
	// empty call on atomic flag → as such and it's continuation
	if f.Atom() {
		return f, f.Cons
	}
	// empty call on composed flag → split and continue on signature
	return f, f.Cons
}

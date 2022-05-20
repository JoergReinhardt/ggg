package main

import (
	"math/bits"
	"strconv"
	"strings"
	"unicode"
)

// TYPE CLASSES
//  lets keep things as simple as possible:
type (
	// ITEM
	Item interface {
		Ident() Item
		Type() Identity
	}
	// IDENTITY
	Symbolic interface {
		Symbol() Str // subtypes of this type
	}
	Identity interface {
		Item
		Symbolic
		Shape() T // subtypes of this type
	}
	// FUNCTOR (Applicable|Function|Monoid|Monad|…)
	Applicable interface {
		Apply(...Item) (Item, Cnt)
	}
	// COLLECTIONS
	Enumeratet interface { // List Next() (Item, Lst)
		Next() (Item, Lst)
	}
	Linked interface {
		Left() Item
		Right() Item
		Swap() Lnk
	}
	Sequential interface {
		Len() Int
		Pick(Int) Item
		Range(s, e Int) Seq
	}
	// COMPOSALS
	Tupled interface {
		Take(...Item) Tupled
		Return(...Item) (Item, Tupled)
	}
	Recorded interface {
		Lookup(Name) Item
		Put(Name, Item) Recorded
	}
	// ARBITRARY TYPE|DATA CONSTRUCTOR
	Constructor interface {
		Item
		Cons(...Item) (Item, Cnt)
	}

	// TYPE CLASSES
	Nullable interface {
		Zero() Item
	}
	Printable interface {
		Print() Str
	}
	Qualified interface { // Qualified a
		Equal(Item) Bool // a → a → Bool (True|False)
	}
	Unique interface {
		Set(Item) (Item, Bool)
		Contains(Item) Bool
		Contained(Item) Bool
	}
	Ordered interface { //
		Qualified
		Lesser(Item) Bool
		Greater(Item) Bool
	}
	Numeric interface {
		Number() Numeric
	}
	Appendable interface {
		Append(Item) Appendable
	}
	Composed interface {
		Compose(Item) (Item, Composed)
		First() Item
		Second() Item
	}
	LeftBinding interface {
		Head() Item
		Tail() Item
	}
	RightBinding interface {
		Init() Item
		Last() Item
	}
	Zipped interface {
		LeftBinding
		RightBinding
		Progress() Cnt
		Regress() Cnt
	}
	UpperBound interface {
		Max() Int
	}
	LowerBound interface {
		Min() Int
	}
	Limitet interface {
		UpperBound
		LowerBound
	}
	Modal interface { // Category∷Monoid
		Concat(Item) Item // [a] ↔ a
	}
	Traversable interface { // Monoid a
		Traverse(Fnc) Cnt
		Serialize(Fnc) Seq
	}
	Monoton interface {
		Bind(Monoton) Cnt
	}

	StateFnc func(Stateful, ...Item) (Item, Cnt)
	Stateful interface {
		Monoton
		Run(...Item) (Item, Cnt)
	}

	// IDENTITY LABLE TYPES
	Flag  Unt
	Name  Str
	Names map[Name]int

	T []Identity
)

func isRoot(i Item) Bool {
	return i.Type() == nil
}
func isPattern(i Item) Bool {
	return isRoot(i) && i.Type().Shape() != nil && len(i.Type().Shape()) == 0
}
func isIdentity(i Item) Bool {
	return isRoot(i) && i.Type().Shape() != nil && len(i.Type().Shape()) == 1
}
func isFlag(i Item) Bool {
	if isIdentity(i) && isCat(i, Data) {
		return i.(Dat) == Unsigned
	}
	return false
}
func isName(i Item) Bool {
	if isIdentity(i) && isCat(i, Data) {
		return i.(Dat) == String
	}
	return false
}
func isClass(i Item) Bool {
	return isRoot(i) && i.Type().Shape() != nil && len(i.Type().Shape()) > 1
}
func isCat(i Item, c Cat) Bool {
	if !isRoot(i) { //‥item itself not a category…
		//‥item type is category… ‥contained in 'c'?
		return isRoot(i.Type()) && c.Contains(i.Type().(Cat))
	}
	return false
}

//// SIGNATURE //////////////////////////////////////
///
//    signature type is a sequence of type instances, possibly recursive to
//    encode the types data structure.
func (s T) Ident() Item { return s }

// Shape·Type → Type
//
//  type returns signatures first element, if there is one, or nil, if this is
//  the empty signature.
func (s T) Type() Identity { return nil }

// Shape·Shape → Type
func (s T) Shape() T { return T{} }
func (s T) Len() Int { return Int(len(s)) }

func (t T) Cons(args ...Item) (i Item, c Cnt) {

	//‥when called without parameters, iterates over elements…
	if len(t) > 0 {
		if len(t) > 1 {
			return t[0], t[1:].Cons
		}
		return t[0], T{}.Cons
	}
	return T{}, nil
}

// Shape·Sequence → Sequence
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
func (s T) List() Lst {
	return Lst(func() (Item, Lst) {
		return s.First(), s.Remainder().List()
	})
}

// Shape·Empty → Bool
//
// empty praedicate returns true, if signature doesn't contain any elements.
func (s T) Empty() Bool { return len(s) == 0 }

// Shape·Single → Bool
//
// empty praedicate returns true, if signature consists of a single element.
func (s T) Single() Bool { return len(s) == 1 }

// Shape·Double → Bool
//
// empty praedicate returns true, if signature consists of two single elements.
func (s T) Paired() Bool { return len(s) == 2 }

// Shape·Enum → Bool
//
// empty praedicate returns true, if signature contains more then two elements.
func (s T) Many() Bool { return len(s) > 2 }

// Shape·Generate → Identity Type
//
//  iterate yields first element of signature, or nil, for the first field of
//  the return value pair and a list of remaining elements for the second
//  field.

// Shape·First → None|Identity
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

// Shape·Second → None|Identity|Identity.Type
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
func (s T) Remainder() T {
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

// Shape·Flip → Sequence
//
//  flip returns signatures fields converted to instances of ident and in
//  reversed order.
func (s T) Flip() T {
	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		s[i], s[j] = s[j], s[i]
	}
	return s
}

// Shape·Labels → [String]
//
//  labels returns signature fields printable symbols.
func (s T) Symbols() []Str {
	var names = make([]Str, 0, len(s))
	for _, t := range s.Shape() {
		names = append(names, t.(Identity).Symbol())
	}
	return names
}

// Shape·Labels → [string]
//
//  labels returns signature fields printable symbols.
func (s T) strings() []string {
	var names = make([]string, 0, len(s))
	for _, t := range s.Shape() {
		names = append(names, string(t.(Identity).Symbol()))
	}
	return names
}

// Shape·Symbol → Text
//
//  symbol yields printable representation of this signature.
func (s T) Symbol() Str {
	if o, _ := Optional.Cons(s.Type()); o != nil {
		if len(s.Shape()) > 0 {
			return o.Type().Shape()[0].Symbol() + Str("|⊥")
		}
		return Category.Symbol() + Str("|⊥")
	}
	if len(s.Shape()) > 0 {
		if len(s.Shape()) > 1 {
			if p := s.Type().Type(); p != nil {
				return p.Symbol() + "·[" + Str(
					strings.Join(p.Shape().strings(), "|")) + "]"
			}
		}
		return s.Type().Symbol() + "·" + s.Shape()[0].Symbol()
	}
	return s.Type().Symbol()
}

// fold	    ∷ a → b → b
// f a b    = b
// f a _    = b
// f _ b|_  = _

// concat   ∷ a → a → a
// c (a:as) = a
// c a a    = a
// c _ as   = a
// c a _    = (a → a)
// c _ a    = (a → a)

// FoldMap  ∷ b & fold $ concat $ a * a|(a:as)

// NAME ///////////////////////////////////////////////////////////////////////
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

func (n Name) Ident() Item    { return n }
func (n Name) Type() Identity { return nil }
func (n Name) Shape() T       { return T{String} }
func (n Name) Symbol() Str    { return Str(n) }
func (n Name) Atom() Bool     { return !n.Composed() }
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

// Names
func (n Names) Ident() Item    { return n }
func (n Names) Type() Identity { return n }
func (n Names) Shape() T       { return T{T{Sequence, Category}} }
func (n Names) Names() []Name {
	var (
		i  = 0
		ns = make([]Name, 0, len(n))
	)
	for n := range n {
		ns[i] = n
		i++
	}
	return ns
}
func (n Names) Pairs() []Lnk {
	var (
		i  = 0
		ls = make([]Lnk, 0, len(n))
	)
	for k, v := range n {
		ls[i] = Lnk(func() (l, r Item) { return k, Int(v) })
		i++
	}
	return ls
}
func (n Names) Symbol() Str {
	var (
		i    = 0
		strs = make([]string, 0, len(n))
	)
	for n := range n {
		strs[i] = string(n)
		i++
	}
	return Str(strings.Join(strs, "|"))
}

// Name·Cons(...Ident) → Identity Cons
//
//  default behaviour of name cons called empty, is to return the instance and
//  it's cons method.  when called with arguments, cons will test those to be
//  of kind category, or class and validate category, or class symbol to either
//  be contained within n, or not, to return the first argument validated to
//  instanciate type & either nil if single argument was given, or succeeding
//  arguments enclosed in a continuation constructor, in case multiple
//  arguments where passed.

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

func (f Flag) Ident() Item    { return f }
func (f Flag) Type() Identity { return nil }
func (f Flag) Shape() T {
	return T{Unsigned}
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

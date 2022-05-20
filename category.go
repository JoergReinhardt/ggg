package main

import "strings"

/// BASE CATEGORYS OF STRUCTURE AND BEHAVIOUR
//
type (
	Cat Flag // ⊥ (None) → Category
)

//// Category… → Cons·Kind → None|Category|Pair|Vector
//func (Kind) String() String { return "" }

//go:generate stringer -type Cat
const (
	////// NULLABLE DATA TYPE
	Function Cat = 1 << iota // Kind
	Class                    // ∗… → ⨍ → ∗
	Data
	///// NULLABLE COMPOSIT
	List     // T []|[T]
	Sequence // []|[*] (indexable golang slice, or array)
	///// TRAVERSABLE CONTINUATION (no arguments required → iterator continuation)
	Pair   // T₁ T₂
	Tuple  // T₁ T₂ … Tₙ
	Record // Name∷T₁ Name∷T₂ … Name∷Tₙ (function type abstracts implementation as map, and/or slice index)
	//////// PRAEDICATE & COMPARATOR
	Conditional // Function resulting in Truth ∷ False|True
	Comparative // Function resulting in Order ∷ Lesser|Equal|Greater
	//////// PARAMETRIC FUNCTIONS (return type depends on arguments)
	Choice      // T₁|T₂|…|Tₙ
	Optional    // ⊥|T
	Variadic    // ⊥|T|[T]
	Alternative // Either|Or
	//////// PARAMETRIC/POLYMORPH CONTINUATION (return type depends on enclosed state)
	Applicative
	Continoid
	Monoid
	Monad
	Arrow
	Optic
	Error

	// 'set contains all categories' ⇒ category identity
	Category = Function | Class | Data | List | Sequence | Pair | Tuple |
		Record | Conditional | Comparative | Choice | Alternative | Applicative |
		Continoid | Monoid | Monad | Arrow | Optic | Error

	ProductType = List | Sequence | Pair | Tuple | Record

	SumType = Conditional | Comparative | Choice | Optional | Variadic | Alternative

	Functor = SumType | ProductType // data structure with default operation (mapF)

	// empty set of categories ⇒ class of categories ⇒  category & class unit
	Nothing Cat = 0
)

// IDENTITY
//
// type of categories, or sets there of is 'Category', which is the OR
// concatenated identity of the set of constants of type 'Cat'.  'Category' is
// no member of that set, hence has no type and returns 'nil' to indicate root
// of the type system instead.
func (k Cat) Type() Identity { return nil }

// a categoriy flags identify as tehemselves
func (k Cat) Ident() Item { return k }

// signature is the split set of OR concatenated set members for composed
// types, or single element slice containing the atomic category dentoed by
// flag.
func (k Cat) Shape() (t T) {
	if k.Composed() {
		return k.Split()
	}
	return nil
}

// Category Constructor
//
// Category ∷ Item… → _,_|Item,_|_,Category|Item, Category
//    a∷Cat = Ta, _
//   a!∷Cat = _,_
//    a∷Cat…= Ta,Cons
//        _ = Category, Cons
func (k Cat) Cons(args ...Item) (i Item, c Cnt) {

	if len(args) > 0 {
		if args[0].Type() == nil {
			if t, ok := args[0].(Cat); ok {
				if k.Contains(t) {
					i = args[0]
				}
			}
		}
		if len(args) > 1 {
			c = Suspend(k.Cons, args[1:]...)
		}
		return i, c
	}

	// iterate over categories
	if k == Category { //‥categories iedentity & continue on 'Nothing'
		return k, Cat(0).Cons
	} //‥instance and continue on next…

	if cardinality(Flag(k)) < cardinality(Flag(Category)) {
		return k, Cat(k + 1<<1).Cons
	} //‥Nothing & continue on first category…

	return Cat(0), Cat(1 << 1).Cons
}

// EQUALITY
func (k Cat) Eq(a Cat) Bool { return k == a }

// ORDER
func (k Cat) Lt(a Cat) Bool { return k < a }
func (k Cat) Gt(a Cat) Bool { return k > a }

// CARDINALITY
func (k Cat) Composed() Bool { return composed(Flag(k)) }
func (k Cat) Atomic() Bool   { return !composed(Flag(k)) }

func (c Cat) Num() Int { return Int(cardinality(c.Flag())) }
func (c Cat) Len() Int { return Int(magnitude(c.Flag())) }

// LIMITATION
func (k Cat) Min() Int  { return Int(0) }
func (k Cat) Max() Int  { return Category.Num() }
func (k Cat) Unit() Cat { return 1 }

// SET OPERATION
func (k Cat) Contains(a Cat) Bool  { return contains(Flag(k), Flag(a)) }
func (k Cat) Contained(a Cat) Bool { return contains(Flag(a), Flag(k)) }

// ITERATION
func (k Cat) Flag() Flag { return Flag(k) }
func (k Cat) Next() Cat {
	if k.Num() < k.Max() {
		return Cat(1<<int(k.Num()) + 1)
	}
	return Nothing
}
func (k Cat) Prev() Cat {
	if k.Num() > k.Min() {
		return Cat(1<<int(k.Num()) - 1)
	}
	return Nothing
}
func (k Cat) Split() T {
	if l := magnitude(Flag(k)); l > 1 {
		t := make(T, 0, l)
		for _, f := range split(Flag(k)) {
			t = append(t, Cat(f))
		}
		return t
	}
	return T{k}
}
func (c Cat) Symbol() (s Str) {
	if magnitude(Flag(c)) > 1 {
		if c == Category {
			return "∗"
		}
		s := make([]string, 0, magnitude(Flag(c)))
		for _, f := range split(Flag(c)) {
			f := f
			s = append(s, string(Cat(f).Symbol()))
		}
		return Str(strings.Join(s, "|"))
	}
	if c == Nothing {
		return Str("⊥")
	}
	return Str(c.Symbol())
}
func (k Cat) Sequence() Seq {
	t := make(Seq, 0, magnitude(Flag(k)))
	for _, f := range k.Split() {
		t = append(t, f.(Cat))
	}
	return t
}

// matchCategory tests, if second argument either is a flag of type 'Cat', or
// has a type, which is a flag of type 'Cat'.  if one of those conditions is
// met, the flag is then tested to either be contained, or not in first
// argument, when interptretet as set, possibly of magnitude one.  if thats the
// case, the argument is been returned, in case argument itself was the flag,
// it will be split to return an item of type 'T'.
func matchCategory(c Cat, arg Item) (i Item) {
	// CATEGORY FLAG
	if arg.Type() == nil { // argument type is category identity
		if c == Category { // category identity is to be matched
			if cat, ok := arg.(Cat); ok {
				return cat.Split()
			}
		}
	}
	// ARGUMENT OF TYPE CATEGORY
	if arg.Type().Type() == nil { // argument type is category, or set there of
		if cat, ok := arg.Type().(Cat); ok { // type is indeed a category
			if c.Contains(cat) { // type is contained in category to be matched
				return arg
			}
		}
	}
	return nil
}

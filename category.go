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
	Optional    // T|⊥
	Alternative // Either|Or
	//////// PARAMETRIC/POLYMORPH CONTINUATION (return type depends on enclosed state)
	Monoid
	Monad
	Arrow
	Optic
	Error

	// 'set contains all categories' ⇒ category identity
	Category = Function | Class | Data | List | Sequence | Pair | Tuple | Record |
		Conditional | Comparative | Choice | Alternative | Monoid | Monad | Arrow | Optic | Error

	Product = List | Sequence | Pair | Tuple | Record

	Sum = Conditional | Comparative | Choice | Optional | Alternative

	Functor = Sum | Product

	Applicative = Functor | Monoid

	Continuum = Applicative | Monad | Arrow | Optic

	// empty set of categories ⇒ class of categories ⇒  category & class unit
	Nothing Cat = 0
)

// IDENTITY

// Das Set aller Kategorien ist ein Teil vom Teil der anfangs alles war, ein
// Teil der Finsternis die sich das Licht gebar.
//
// returns category identity (all Flag Constants of type Cat '|' concatinated)
// unless the instance is Category identity, in which case nil is retuned, to
// indicate the root of the hierachical system of categories.
func (k Cat) Type() Identity {
	if k == Category {
		return nil
	}
	return Category
}

func (k Cat) Ident() Item {
	return k
}
func (k Cat) Signature() (t T) {
	if k.Composed() {
		return T{k, k.Split()}
	}
	return T{k}
}

// Category Constructor
//
// Category ∷ Item… → _,_|Item,_|_,Category|Item, Category
//    a∷Cat = Ta, _
//   a!∷Cat = _,_
//    a∷Cat…= Ta,Continue
//        _ = Category, Continue
func (k Cat) Continue(args ...Item) (i Item, c Cnt) {
	if len(args) > 0 {
		var (
			t   Identity
			arg = args[0]
		)
		if arg.Type() == nil {
			if k.Contains(t.(Cat)) {
				i = T{arg.Type(), arg.Type().Signature()}
			}
		}
		if t = arg.Type(); t != nil {
			if t = t.Type(); t == nil {
				if k.Contains(t.(Cat)) {
					i = T{arg.Type(), arg.Type().Signature()}
				}
			}
		}
		if len(args) > 1 {
			args = args[1:]
			return i, Condense(k.Continue(args...))
		}
		return i, nil
	}
	if k == Category {
		return k, Cat(1).Continue
	}
	return k, Cat(k + 1<<1).Continue
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
	return Str(c.String())
}

func (k Cat) Flag() Flag { return Flag(k) }
func (k Cat) Sequence() Seq {
	t := make(Seq, 0, magnitude(Flag(k)))
	for _, f := range k.Split() {
		t = append(t, f.(Cat))
	}
	return t
}

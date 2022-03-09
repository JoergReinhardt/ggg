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
	Class Cat = 1 << iota // Kind
	Data
	Function // ∗… → ⨍ → ∗
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
	Option      // T|⊥
	Choice      // T₁|T₂|…|Tₙ
	Alternative // Either|Or
	//////// PARAMETRIC/POLYMORPH CONTINUATION (return type depends on enclosed state)
	Applicative
	Functor
	Monad
	Error

	// 'set contains all categories' ⇒ category identity
	Category = Class | Data | Function | List | Sequence | Pair | Tuple | Record |
		Functor | Conditional | Comparative | Option | Applicative |
		Choice | Alternative | Monad | Error

	Additive = List | Sequence | Pair | Tuple | Record

	Exclusive = Conditional | Comparative | Option | Choice | Alternative

	Collection = Additive | Exclusive

	// empty set of categories ⇒ class of categories ⇒  category & class unit
	Nothing Cat = 0
)

// IDENTITY
func (k Cat) Ident() Item { return Category }

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

// Das Set aller Kategorien ist ein Teil vom Teil der anfangs alles war, ein
// Teil der Finsternis die sich das Licht gebar.
func (k Cat) Type() T { return T{} }
func (k Cat) Signature() T {
	if k.Composed() {
		return k.Split()
	}
	return T{k}
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
			s = append(s, string(Cat(f).Symbol()))
		}
		return Str(strings.Join(s, "|"))
	}
	if c == Nothing {
		return Str("⊥")
	}
	return Str(c.String())
}

func (k Cat) Flag() Flag    { return Flag(k) }
func (k Cat) Flags() []Flag { return split(Flag(k)) }
func (k Cat) Kinds() []Cat {
	var ks = make([]Cat, 0, cardinality(Flag(k)))
	if cardinality(Flag(k)) > 1 {
		for _, f := range split(Flag(k)) {
			ks = append(ks, Cat(f))
		}
	}
	return ks
}
func (k Cat) Types() []Identity {
	t := T{}
	for _, f := range k.Split() {
		t = append(t, f.(Cat))
	}
	return t
}
func (k Cat) Sequence() Seq {
	t := make(Seq, 0, magnitude(Flag(k)))
	for _, f := range k.Split() {
		t = append(t, f.(Cat))
	}
	return t
}
func promoteCat(t T) Cat {
	if len(t) > 0 {
		// if passed type is no category…
		if c, ok := t[0].(Cat); ok {
			return c
		}
		return promoteCat(t[0].Type())
	}
	return Nothing
}
func (k Cat) Cons(args ...Item) (i Item, c Continue) {
	// HAS ARGUMENT(S) PASSED
	if len(args) > 0 {
		// MANY ARGUMENTS
		if len(args) > 1 {
			// FIRST ARGUMENT DID VALIDATE
			if arg, _ := k.Cons(args[0]); arg != nil {
				// return validated arg, pass remainder to
				// caller as possible call arguments for next
				// continuation application.
				return arg, Seq(args[1:]).Cons
			}
			// INVALID FIRST ARGUMENT
			// if type ain't category at all, abort graceful & pass on
			// all arguments!
			return nil, Seq(args).Cons
		}
		// TEST IF FIRST ARGUMENT IS CATEGORY
		if t := args[0].Type(); len(t) > 0 {
			// IS CATEGORY
			if cat, ok := t[0].(Cat); ok {
				// return _argument_!… ‥if its type is
				// contained in class k
				if k.Composed() {
					if k.Contains(cat) {
						return args[0], nil
					}
				}
				// ‥if its type equals category k
				if cat == k {
					return args[0], nil
				}
			}
			// OR INDEX
			if idx, ok := t[0].(Index); ok {
				// construct category from index
				return Cat(1 << idx), nil
			}
			// OR NEITHER
		}
		// TODO:
		//  deal with type signatures not starting with category:
		//    Item → promote type until category
		//    Other → Error|Cast
	}
	// NO ARGUMENTS PASSED
	//    COMPOSED IDENTITY
	if k.Composed() {
		// CATEGORY IDENTITY
		// jump from category identity to nothing, since category of
		// all categories does not contain itself! (nothing to shave,
		// barber is a woman, move on!)
		if k == Category {
			return k, Nothing.Cons
		}
		// CLASS IDENTITY
		// return classes signature for further evaluation
		return k, k.Split().Cons
	}
	//	NO IDENTITY, NOTHING TO CONTINUE
	if k == Nothing {
		// nothing returns the empty category and empty category
		// constructor (construct me some sweet nothingness),
		return T{}, Nothing.Cons
	}
	//	CURRENT (K) IDENTITY & NEXT K TO CONTINUE ON
	return k, (k << 1).Cons
}

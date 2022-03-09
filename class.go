package main

type (
	cls struct {
		Cat
		T // type signature
	}
)

func (a cls) Ident() Item      { return a.Cat }
func (a cls) Signature() T     { return a.T }
func (a cls) Type() T          { return T{a.Cat} }
func (a cls) Empty() Item      { return cls{0, T{}} }
func (a cls) IsEmpty() Bool    { return len(a.T) == 0 }
func (a cls) IsType() Bool     { return len(a.T) == 1 }
func (a cls) IsClass() Bool    { return a.Cat.Len() > 1 }
func (a cls) IsComposed() Bool { return len(a.T) > 1 && !a.IsClass() }
func (a cls) Symbol() Str      { return Str(a.Cat.String()+"∙") + a.T.Symbol() }

// DERIVATE (DERIVED) INSTANCE
//
// fmap & compose are defined as free functions with two parameters each:
type mapf func(Item, Fnc) Item

// fmap instances of the derived type, to derive
// values of possibly the same, or yet another type → FMap of derivate types
// needs to be augmentet to pass entire derived identity, _not_ just the
// resulting values, or original items identity, in order to ensure the first
// functor law holds.
//
//    Identity
//      fmap id == id
//
//    Composition
//      fmap (f·g) == fmap f · fmap g

// pass instances functor identity & lift argument to the functor domain, by
// allocating an argument functor referencing the argument, fmap and compose.

// QUALIFIED INSTANCE
//
// implements equality by defining the equal method for whatever the embedded
// item is.
type equal func(a, b Item) Bool
type equality struct {
	Item
	equal
}

func (a equality) Equal(b Item) Bool { return a.equal(a.Item, b) }

// ORDER INSTANCE
//
// implements order by defining lesser and greater methods for the empedded
// instance of qualifyable item (which may be a type, class, operation…).
type lesser func(a, b Item) Bool
type greater func(a, b Item) Bool
type order struct {
	Ordered
	lesser
	greater
}

func (a order) Lesser(b Item) Bool  { return a.lesser(a.Ident(), b) }
func (a order) Greater(b Item) Bool { return a.greater(a.Ident(), b) }

// SEQUENTIAL INSTANCE
//
// implements sequence behaviour by defining length, pick and range methods.
// since all methods of sequences close over their sequences, sequence
// instance doesn't need to embed an item to hold reference to the sequence.
type sequence struct {
	Item
	leng func(Item) Int
	pick func(Item, Int) Item
	rang func(Item, Int, Int) Sequential
}

func (a sequence) Len() Int                  { return a.leng(a.Item) }
func (a sequence) Pick(i Int) Item           { return a.pick(a.Item, i) }
func (a sequence) Range(s, e Int) Sequential { return a.rang(a.Item, s, e) }

// ENUMERATED INSTANCE
//
// implements behaviour of all types that can be ennumerated, non serializable
// types included, by implementing an iterator interface defined by the methods
// next, extend and empty.
//
// does not need to embed an instance, since its methods close over their
// instance respectively.
type enum struct {
	Item
	empty  func(Item) Bool
	next   func(Item) (Item, Iterator)
	extend func(Item, ...Item) Iterator
}

func (a enum) Empty() Bool                  { return a.empty(a.Item) }
func (a enum) Next() (Item, Iterator)       { return a.next(a.Item) }
func (a enum) Extend(args ...Item) Iterator { return a.extend(a.Item, args...) }

// CONSTRUCTABLE (TUPLE,PRODUCT) TYPE INSTANCE
// tuple type gets returned in the 'Type()' method of user defined composit
// types, which includes parameter set definitions for user defined functions
// and type constructors.
//
// in order to bind the least neccessary set of interface methods for a tuple
// type, to any user defined type, the 'tuple' struct binds 'Make' & 'Take'
// methods to the tuple type identity and has to be returned by the 'Type'
// method of every instance of a user defined tuple type.
//
// takeT needs to define a function that can either make instances of the user
// defined type and bind its type identity to it and/or return instances of the
// tuple type.
//
// the embedded T describes the tuple signatures fields

type unit func(Modal) (Item, Modal)
type zero func() Modal
type modal struct {
	Item
	zero   func(Item) Modal
	unit   func(Item, Item) Modal
	concat func(Item, ...Item) Modal
}

func (a modal) Zero() Modal               { return a.zero(a.Item) }
func (a modal) Unit(arg Item) Modal       { return a.unit(a.Item, arg) }
func (a modal) Concat(args ...Item) Modal { return a.concat(a.Item, args...) }

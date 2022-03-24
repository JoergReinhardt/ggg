package main

type (
	Truth int8
	Test  func(a, b Qualified) Truth

	Order   int8
	Compare func(a, b Ordered) Order

	Logic  uint8
	Decide func(a, b Bool) Bool

	Relation  uint16
	Contains  func(Unique) Bool
	Contained func(Unique) Bool

	Guard func(Item) (Item, Bool)
)

//go:generate stringer -type Truth
const (
	False     Truth = -1
	Undefined Truth = 0
	True      Truth = 1

	btrue     = '⊤'
	undefined = '¬'
	bfalse    = '⊥'
)

func (a Truth) Ident() (i Item) { return a }
func (a Truth) Type() Identity  { return T{Boolean} }
func (a Truth) Symbol() (s Str) {
	switch a {
	case False:
		return Str(bfalse)
	case True:
		return Str(btrue)
	}
	return Str(undefined)
}
func (a Truth) Signature() (t T) { return T{a} }
func (a Truth) Bool() Bool {
	if a == True {
		return true
	}
	return false
}

// TODO: your usual function might expect application of boolean as default,
// but why did i specify it as 'truth' in the first place, if not utilize the
// fact it can be undecided?… see how it turns out and change eventually.
func (a Truth) Empty() Bool { return a == Undefined }

// TODO: or is maybe concatenation the default operation for all values, no
// matter what until it comes to actual expression of operations?
func (a Truth) Concat(xs ...Item) Item {
	if len(xs) > 0 {
		return append(append(make(Seq, 0, len(xs)+1), a), xs...)
	}
	return a
}
func (a Truth) FromBool(b Bool) Truth {
	if b {
		return True
	}
	return False
}
func (a Truth) Undefined() Bool { return a == Undefined }
func (a Truth) MapF(f Fnc) Item { return f(a) }

// MOTHER OF ALL CONTINUATIONS
//func (a Truth) Continue(xs ...Item) (i Item, c Continue) {
//	if len(xs) > 0 {
//		if len(xs) > 1 {
// a (x:xs) = x → a → (y → b)
//		}
// a x = x → a → (y → b)
//		return Continue(func(ys ...Item) (ii Item, cc Continue) {
//			if len(ys) > 0 {
//				if len(ys) > 1 {
//				}
//			}
//		})
//	}
// a _ = a
//}
//go:generate stringer -type Logic
const (
	Xor Logic = 1 << iota
	Or
	Pip

	And Logic = 0

	Negation Logic = 1<<8 - 1

	LogicOps = Negation

	pip    = '|' // exclusive
	and    = '⋀'
	xor    = '⊻'
	or     = '⋁'
	negate = '¬'
)

func (a Logic) Ident() (i Item)  { return a }
func (a Logic) Signature() (t T) { return T{a} }
func (a Logic) Type() Identity   { return T{Negation} }
func (a Logic) Symbol() (s Str) {
	switch a {
	case Or:
		return Str(or)
	case Pip:
		return Str(pip)
	case Xor:
		return Str(xor)
	case And:
		return Str(pip)
	case Negation:
		return Str(negate)
	}
	return Str("'") + Str(pip) + Str("'") + Str(pip) + Str(and) +
		Str(pip) + Str(xor) + Str(pip) + Str(negate)
}

//go:generate stringer -type Order
const (
	MLesser Order = -2 + iota
	Lesser
	Equal
	Greater
	MGreater

	OrderOp = MLesser | Lesser | Equal | Greater | MGreater

	mlesser  = '≪'
	lesser   = '<'
	equal    = '='
	greater  = '>'
	mgreater = '≫'
)

func (o Order) Ident() Item                         { return o }
func (o Order) Type() Identity                      { return T{Comparative, o} }
func (o Order) Signature() T                        { return T{Lesser, Equal, Greater} }
func (o Order) Continue(xs ...Item) (i Item, c Cnt) { return i, c }
func (o Order) Symbol() Str {
	switch o {
	case Lesser:
		return Str(lesser)
	case Equal:
		return Str(equal)
	case Greater:
		return Str(greater)
	}
	return Str(lesser + pip + greater + pip + greater)
}

func (o Compare) Ident() Item                         { return o }
func (o Compare) Type() Identity                      { return OrderOp }
func (o Compare) Signature() T                        { return T{Function, T{Category, Category}} }
func (o Compare) Symbol() Str                         { return Str("Order ∷ ∗ → ∗ → Lesser|Equal|Greater") }
func (o Compare) Continue(xs ...Item) (i Item, c Cnt) { return i, c }

//go:generate stringer -type Relation
const (
	Only Relation = 1 << iota
	Any
	All
	Superset
	NotSuperset
	Subset
	NotSubset
	Succeeds
	Preceeds

	Unrelated Relation = 0

	all    = 'A'
	ani    = '∀'
	xst    = '∃' // exists
	xsn    = '∄'
	excl   = Str('!') + Str(xst) // exists exactly once
	sup    = '⊃'
	notsup = '⊅'
	sub    = '⊂'
	nosub  = '⊄'
	suc    = '⊱'
	prec   = '⊰'
	unrel  = '¬'
)
const ()

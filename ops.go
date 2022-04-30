package main

type (
	// indexed sets
	Trp int8
	Ord int8
	Lgc int8
	Rln int16
	Arm int8
)

//go:generate stringer -type Trh
const (
	False     Trp = -1
	Undefined Trp = 0
	True      Trp = 1

	truthTrue  = '⊤'
	undefined  = '¬'
	truthFalse = '⊥'
)

var (
	truthNames = map[Trp]Rune{
		True:      '⊤',
		Undefined: '¬',
		False:     '⊥',
	}
	truthByName = map[Rune]Trp{
		'⊤': False,
		'¬': Undefined,
		'⊥': True,
	}
)

func (a Trp) Ident() (i Item) { return a }
func (a Trp) Type() Identity  { return Function }
func (a Trp) Symbol() (s Str) {
	switch a {
	case False:
		return Str(truthFalse)
	case True:
		return Str(truthTrue)
	}
	return Str(undefined)
}
func (a Trp) Signature() (t T) { return T{a} }
func (f Trp) Continue(as ...Item) (a Item, c Cnt) {
	if len(as) > 0 {
		if len(as) > 1 {
			if len(as) > 2 {
			}
		}
	}
	return f, T{False, Undefined, True}.Continue
}
func (a Trp) Bool() Bool {
	if a == True {
		return true
	}
	return false
}
func (a Trp) Concat(xs ...Item) Item {
	if len(xs) > 0 {
		return append(append(make(Seq, 0, len(xs)+1), a), xs...)
	}
	return a
}
func (a Trp) FromBool(b Bool) Trp {
	if b {
		return True
	}
	return False
}
func (a Trp) Undefined() Bool { return a == Undefined }
func (a Trp) MapF(f Fnc) Item { return f(a) }

//go:generate stringer -type Lgc
const (
	And Lgc = 0 + iota
	Or
	Xor
	Nor

	Neg Lgc = -1

	nor = '|' // n-ary or
	and = '⋀'
	xor = '⊻'
	or  = '⋁'
	neg = '¬'
)

func (a Lgc) Ident() (i Item)  { return a }
func (a Lgc) Signature() (t T) { return T{And, Or, Xor, Nor, Neg} }
func (a Lgc) Type() Identity   { return Operator }
func (a Lgc) Symbol() (s Str) {
	switch a {
	case Or:
		return Str(or)
	case Nor:
		return Str(nor)
	case Xor:
		return Str(xor)
	case And:
		return Str(nor)
	case Neg:
		return Str(neg)
	}
	return Str("'") + Str(nor) + Str("'") + Str(nor) + Str(and) +
		Str(nor) + Str(xor) + Str(nor) + Str(neg)
}

// operator constructor returns specific instances depending on argument type
// and number of arguments passed. operator may possibly be partiolly applied.
func (a Lgc) Continue(ts ...Item) (i Item, c Cnt) { // args have to be Identity
	if len(ts) > 0 {
	}
	return i, c
}

//go:generate stringer -type Ord
const (
	MuchLesser Ord = -2 + iota
	Lesser
	Equal
	Greater
	MuchGreater

	OrderOps = MuchLesser | Lesser | Equal | Greater | MuchGreater

	muchLesser  = '≪'
	lesser      = '<'
	equal       = '='
	greater     = '>'
	muchGreater = '≫'
)

var orderNames = map[Ord]Rune{
	MuchLesser:  '≪',
	Lesser:      '<',
	Equal:       '=',
	Greater:     '>',
	MuchGreater: '≫',
}

func (o Ord) Ident() Item                         { return o }
func (o Ord) Type() Identity                      { return Function }
func (o Ord) Signature() T                        { return T{Operator} }
func (o Ord) Continue(xs ...Item) (i Item, c Cnt) { return i, c }
func (o Ord) Symbol() Str {
	var r Rune
	switch o {
	case MuchLesser:
		r = muchLesser
	case Lesser:
		r = lesser
	case Equal:
		r = equal
	case Greater:
		r = greater
	case MuchGreater:
		r = muchGreater
	}
	return Str(r)
}

//go:generate stringer -type Rln
const (
	All Rln = 1 + iota
	Any
	Once
	Exists
	ExistsNot
	Superset
	NoSuperset
	Subset
	NoSubset
	Union
	Intersection
	Succeessor
	Predecessor

	Unrelated Rln = 0

	all          = 'A'
	ani          = '∀'
	once         = '!' + exists // exists exactly once
	exists       = '∃'          // exists
	existsNot    = '∄'
	superSet     = '⊃'
	noSuperSet   = '⊅'
	subSet       = '⊂'
	noSubSet     = '⊄'
	union        = '⋂'
	intersection = '⋃'
	successor    = '⊱'
	predecessor  = '⊰'
	unrelatet    = '¬'
)

var (
	relationNames = map[Rln]Rune{
		unrelatet:    '¬',
		all:          'A',
		ani:          '∀',
		once:         '!',
		exists:       '∃',
		existsNot:    '∄',
		superSet:     '⊃',
		noSuperSet:   '⊅',
		subSet:       '⊂',
		noSubSet:     '⊄',
		union:        '⋂',
		intersection: '⋃',
		successor:    '⊱',
		predecessor:  '⊰',
	}
)

func (r Rln) Ident() Item    { return r }
func (r Rln) Type() Identity { return Function }
func (r Rln) Signature() T {
	return T{r}
}
func (r Rln) Symbol() Str {
	var s Rune
	switch r {
	case All:
		s = all
	case Any:
		s = ani
	case Once:
		s = once
	case Exists:
		s = exists
	case ExistsNot:
		s = existsNot
	case Superset:
		s = superSet
	case noSuperSet:
		s = noSuperSet
	case Subset:
		s = subSet
	case NoSubset:
		s = noSubSet
	case Union:
		s = union
	case Intersection:
		s = intersection
	case Succeessor:
		s = successor
	case Predecessor:
		s = predecessor
	}
	return Str(s)
}
func (r Rln) Continue(args ...Item) (i Item, c Cnt) { return i, c }

//go:generate stringer -type Ath
const (
	Substract Arm = -1 + iota
	Add
	Multiply
	Divide
	Power
	Sum
	Product
	Coproduct
	Root
	NaryAdd
	NaryDotMultiply
	NaryXMultiply

	substract       = '-'
	add             = '+'
	multiply        = '×'
	divide          = '÷'
	root            = '√'
	power           = 'ⁿ'
	sum             = '⅀'
	product         = 'Π'
	coproduct       = '∐'
	naryAdd         = '⨁'
	naryDotMultiply = '⨀'
	naryXMultiply   = '⨂'
)

func (a Arm) Ident() Item    { return a }
func (a Arm) Type() Identity { return Operator }
func (a Arm) Signature() T {
	return T{Substract, Add, Multiply, Divide, Power, Sum, Product,
		Coproduct, Root, NaryAdd, NaryDotMultiply, NaryXMultiply}
}
func (a Arm) Symbol() Str                           { return "" }
func (a Arm) Continue(args ...Item) (i Item, c Cnt) { return i, c }

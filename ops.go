package main

import "strings"

type (
	// indexed sets
	Trp int8
	Ord int8
	Lgc int8
	Rln int8
	Arm int8
)

//go:generate stringer -type Trh
const (
	False     Trp = -1
	Undefined Trp = 0
	True      Trp = 1

	ttrue     = '⊤'
	undefined = '¬'
	tfalse    = '⊥'
)

func (a Trp) Ident() (i Item) { return a }
func (a Trp) Type() Identity  { return T{Boolean} }
func (a Trp) Symbol() (s Str) {
	switch a {
	case False:
		return Str(tfalse)
	case True:
		return Str(ttrue)
	}
	return Str(undefined)
}
func (f Trp) Continue(as ...Item) (a Item, c Cnt) {
	if len(as) > 0 {
		if len(as) > 1 {
			if len(as) > 2 {
			}
		}
	}
	return f, T{False, Undefined, True}.Continue
}
func (a Trp) Signature() (t T) { return T{a} }
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
	MLesser Ord = -2 + iota
	Lesser
	Equal
	Greater
	MGreater

	OrderOps = MLesser | Lesser | Equal | Greater | MGreater

	mlesser  = '≪'
	lesser   = '<'
	equal    = '='
	greater  = '>'
	mgreater = '≫'
)

func (o Ord) Ident() Item                         { return o }
func (o Ord) Type() Identity                      { return Operator }
func (o Ord) Signature() T                        { return T{Lesser, Equal, Greater} }
func (o Ord) Continue(xs ...Item) (i Item, c Cnt) { return i, c }
func (o Ord) Symbol() Str {
	switch o {
	case Lesser:
		return Str(lesser)
	case Equal:
		return Str(equal)
	case Greater:
		return Str(greater)
	}
	return Str(lesser + nor + greater + nor + greater)
}

//go:generate stringer -type Rln
const (
	Only Rln = 1 + iota
	Any
	All
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

	all           = 'A'
	ani           = '∀'
	once          = '!' + xst // exists exactly once
	xst           = '∃'       // exists
	xsn           = '∄'
	superSet      = '⊃'
	noSuperSet    = '⊅'
	subSet        = '⊂'
	noSubSet      = '⊄'
	union         = '⋂'
	intrersection = '⋃'
	successor     = '⊱'
	predecessor   = '⊰'
	unrelatet     = '¬'
)

func (r Rln) Ident() Item    { return r }
func (r Rln) Type() Identity { return Operator }
func (r Rln) Signature() T {
	return T{Unrelated, Only, Any, All, Superset, NoSuperset, Subset,
		NoSubset, Union, Intersection, Succeessor, Predecessor}
}
func (r Rln) Symbol() Str {
	return Str(strings.Join([]string{string(all), string(ani), string(xst),
		string(xsn), string(once), string(superSet), string(noSuperSet), string(subSet),
		string(noSubSet), string(union), string(Intersection), string(successor), string(predecessor), string(unrelatet)}, "|"))
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

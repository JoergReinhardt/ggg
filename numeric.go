package main

import "strings"

//go:generate stringer -type TruthId
const (
	False TruthId = 0 + iota
	True
	Uniq
	Any
	All
	Not
	And
	Xor
	Or
	LT
	GT

	Truth = True | False | Uniq | Any | All | Not | And | Xor | Or | LT | GT

	SymTrue  TruthSym = "⊤"
	SymFalse          = "⊥"
	SymUniq           = "∃!"
	SymAny            = "∃"
	SymAll            = "∀"
	SymNot            = "¬"
	SymAnd            = "⋀"
	SymXor            = "⊻"
	SymOr             = "⋁"
	SymLT             = ">"
	SymGT             = "<"
)

type (
	// BOOLEAN LOGIC OPERATORS
	TruthId  Int
	TruthSym Str
)

func (i TruthId) Ident() Item  { return Truth }
func (i TruthId) Type() T      { return T{Boolean} }
func (i TruthId) Signature() T { return T{i} }
func (i TruthId) Symbol() Str {
	if Flag(i).Len() > 1 {
		ss := make([]string, 0, Flag(i).Len())
		for f := range Flag(i).Split() {
			ss = append(ss, string(TruthId(f).Symbol()))
		}
		return Str(strings.Join(ss, "|"))
	}
	return Str(i.String())
}

//go:generate stringer -type OrdId
const (
	Lesser OrdId = -1 + iota
	Equal
	Greater

	Order = Lesser | Equal | Greater
)

type (

	////////// ORDER AND COMPARISION
	OrdId   Int // Lesser|Equal|Greater
	Compare func(l, r Item) OrdId
)

func (o OrdId) Ident() Item  { return o }
func (o OrdId) Type() T      { return T{Comparative} }
func (o OrdId) Symbol() Str  { return Str(o.String()) }
func (o OrdId) Signature() T { return T{Lesser, Equal, Greater} }

func (o Compare) Ident() Item  { return o }
func (o Compare) Type() T      { return T{Order} }
func (o Compare) Signature() T { return T{Function, T{Category, Category}} }
func (o Compare) Symbol() Str  { return Str("Order ∷ ∗ → ∗ → Lesser|Equal|Greater") }

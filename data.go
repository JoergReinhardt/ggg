package main

import (
	"fmt"
	"math/big"
	"strconv"
	"strings"
)

type (
	Dat Flag // ⊥ (Empty) → DataType
	GoV interface{}
)

//func (KindData) String() string { return "" }

//go:generate stringer -type Dat
const (
	// Data (see category.go) ← DataType → Boolean∷…∷Literal
	Boolean Dat = 1 << iota
	Unsigned
	Integer
	Rational
	Irrational
	Imaginary
	Binary
	Double
	Character
	String
	Native
	Literal

	Bitwise = Boolean | Binary | Double | Character | Unsigned

	Numeric = Boolean | Unsigned | Integer | Rational |
		Irrational | Imaginary | Binary | Double

	Symbolic = Character | String | Literal

	Transform = Native | Literal

	Instances = Bitwise | Numeric | Symbolic | Transform

	None Dat = 0
)

//func (DataFlag) String() string { return "" }

func consData(...Item) (o Item, c Continue) {
	return o, c
}
func (d Dat) Ident() Item { return Instances }
func (d Dat) Type() T     { return T{Data} }
func (d Dat) Signature() T {
	if d == None {
		return T{Data}
	}
	return T{d}
}
func (d Dat) Len() Int      { return Int(magnitude(Flag(d))) }
func (d Dat) Number() Index { return Index(cardinality(d.Flag()) - 1) }
func (c Dat) Symbol() Str {
	if magnitude(Flag(c)) > 1 {
		s := make([]string, 0, magnitude(Flag(c)))
		for _, f := range split(Flag(c)) {
			s = append(s, string(Dat(f).Symbol()))
		}
		return Str(strings.Join(s, "|"))
	}
	if c == None {
		return Str("⊥")
	}
	return Str(c.String())
}

func (Dat) Null() Item { return Class }
func (Dat) Unit(args ...Item) Identity {
	if len(args) > 0 {
		if len(args) > 1 {
			args = args[1:]
		}
	}
	return nil
}
func (d Dat) Flag() Flag     { return Flag(d) }
func (c Dat) Flags() []Flag  { return split(Flag(c)) }
func (d Dat) Composed() Bool { return composed(Flag(d)) }

func (d Dat) Split() T {
	sig := make([]Identity, 0, cardinality(Flag(d)))
	for _, f := range split(Flag(d)) {
		sig = append(sig, Identity(Dat(f)))
	}
	return sig
}

func (d Dat) Cons(args ...Item) (i Item, c Continue) {
	return i, c
}

func (d Dat) G(g GoV) Item {
	var n Item
	switch g.(type) {
	case bool:
		n = Bool(g.(bool))
	case uint, uint16, uint32, uint64:
		n = Unt(g.(uint))
	case int, int8, int16, int64:
		n = Int(g.(int))
	case float64, float32:
		n = Flt(g.(float64))
	case complex128, complex64:
		n = Cplx(g.(complex128))
	case byte:
		n = Byte(g.(byte))
	case rune:
		n = Rune(g.(rune))
	case string, []rune:
		n = Str(g.(string))
	default:
		n = None
	}
	return n
}

type (
	// BASE TYPE DATA CONSTRUCTORS
	Bool bool
	Unt  uint
	Int  int
	Rat  big.Rat
	Flt  float64
	Cplx complex128
	Byte byte   // ← uint8
	Doub uint16 // ← uint16
	Rune rune   // ← uint32
	Str  string

	BoolConst func() Bool
	UntConst  func() Unt
	IntConst  func() Int
	RatConst  func() *Rat
	FltConst  func() Flt
	CplxConst func() Cplx
	ByteConst func() Byte
	DoubConst func() Doub
	RuneConst func() Rune
	StrConst  func() Str

	CastBool func(Item) Bool
	CastUnt  func(Item) Unt
	CastInt  func(Item) Int
	CastRat  func(Item) *Rat
	CastFlt  func(Item) Flt
	CastCplx func(Item) Cplx
	CastByte func(Item) Byte
	CastDoub func(Item) Doub
	CastRune func(Item) Rune
	CastStr  func(Item) Str

	BoolAssert func(Item) Bool
	UntAssert  func(Item) Unt
	IntAssert  func(Item) Int
	RatAssert  func(Item) *Rat
	FltAssert  func(Item) Flt
	CplxAssert func(Item) Cplx
	ByteAssert func(Item) Byte
	DoubAssert func(Item) Doub
	RuneAssert func(Item) Rune
	StrAssert  func(Item) Str

	InitBool func(bool) Bool
	InitUnt  func(uint) Unt
	InitInt  func(int) Int
	InitRat  func(*big.Rat) *Rat
	InitFlt  func(float64) Flt
	InitCplx func(complex128) Cplx
	InitByte func(byte) Byte
	InitDoub func(uint16) Doub
	InitRune func(rune) Rune
	InitStr  func(string) Str

	BoolNat func(Bool) bool
	UntNat  func(Unt) uint
	IntNat  func(Int) int
	RatNat  func(*Rat) *big.Rat
	FltNat  func(Flt) float64
	CplxNat func(Cplx) complex128
	ByteNat func(Byte) byte   // ← uint8
	DoubNat func(Doub) uint16 // ← uint16
	RuneNat func(Rune) rune   // ← uint32
	StrNat  func(Str) string

	// ENUMERATED BASE TYPE DATA CONSTRUCTORS
	BoolSeq  []Bool
	UintSeq  []Unt
	IntSeq   []Int
	RatSeq   []*Rat
	FltSeq   []Flt
	CmplxSeq []Cplx
	ByteSeq  []Byte
	DoubSeq  []Doub
	RuneSeq  []Rune
	StrSeq   []Str

	// variadic parameterized functions that take, and/or return instances
	// of base types.
	ReturnBool func(...Item) Bool
	ReturnUint func(...Item) Unt
	ReturnInt  func(...Item) Int
	ReturnRat  func(...Item) *Rat
	ReturnFlt  func(...Item) Flt
	ReturnCplx func(...Item) Cplx
	ReturnByte func(...Item) Byte
	ReturnDoub func(...Item) Doub
	ReturnRune func(...Item) Rune
	ReturnStr  func(...Item) Str

	TakeBool func(...Bool) Item
	TakeUint func(...Unt) Item
	TakeInt  func(...Int) Item
	TakeRat  func(...*Rat) Item
	TakeFlt  func(...Flt) Item
	TakeCplx func(...Cplx) Item
	TakeByte func(...Byte) Item
	TakeDoub func(...Doub) Item
	TakeRune func(...Rune) Item
	TakeStr  func(...Str) Item

	BoolOp  func(...Bool) Bool
	UintOp  func(...Unt) Unt
	IntOp   func(...Int) Int
	RatOp   func(...*Rat) *Rat
	FloatOp func(...Flt) Flt
	FltOp   func(...Cplx) Cplx
	ByteOp  func(...Byte) Byte
	DoubOp  func(...Doub) Doub
	RuneOp  func(...Rune) Rune
	StrOp   func(...Str) Str

	IterateBool  func(BoolSeq, ...Bool) (Bool, BoolSeq)
	IterateUint  func(UintSeq, ...Unt) (Unt, UintSeq)
	IterateInt   func(IntSeq, ...Int) (Int, IntSeq)
	IterateRat   func(RatSeq, ...*Rat) (*Rat, RatSeq)
	IterateFlt   func(FltSeq, ...Flt) (Flt, FltSeq)
	IterateCmplx func(CmplxSeq, ...Cplx) (Cplx, CmplxSeq)
	IterateByte  func(ByteSeq, ...Byte) (Byte, ByteSeq)
	IterateDoub  func(DoubSeq, ...Doub) (Doub, DoubSeq)
	IterateRune  func(RuneSeq, ...Rune) (Rune, RuneSeq)
	IterateStr   func(StrSeq, ...Str) (Str, StrSeq)

	ContinueBool func(args ...Item) (o Item, c Continue)
	ContinueUnt  func(args ...Item) (o Item, c Continue)
	ContinueInt  func(args ...Item) (o Item, c Continue)
	ContinueRat  func(args ...Item) (o Item, c Continue)
	ContinueFlt  func(args ...Item) (o Item, c Continue)
	ContinueCplx func(args ...Item) (o Item, c Continue)
	ContinueByte func(args ...Item) (o Item, c Continue)
	ContinueDoub func(args ...Item) (o Item, c Continue)
	ContinueRune func(args ...Item) (o Item, c Continue)
	ContinueStr  func(args ...Item) (o Item, c Continue)
)

////  DATA TYPE CONSTRUCTORS
///
///   TYPE CONTRUCTOR
//
//    data type cons method returns an expectation monad to expect, validate
//    and instanciate instances of the methods receiver type.

func (e Bool) Ident() Item { return e }
func (e Unt) Ident() Item  { return e }
func (e Int) Ident() Item  { return e }
func (e Rat) Ident() Item  { return e }
func (e Flt) Ident() Item  { return e }
func (e Cplx) Ident() Item { return e }
func (e Byte) Ident() Item { return e }
func (e Doub) Ident() Item { return e }
func (e Rune) Ident() Item { return e }
func (e Str) Ident() Item  { return e }

func (e Bool) Null() Item { return Bool(false) }
func (e Unt) Null() Item  { return Unt(uint(0)) }
func (e Int) Null() Item  { return Int(int(0)) }
func (e Rat) Null() Item  { return Rat(*big.NewRat(0, 1)) }
func (e Flt) Null() Item  { return Flt(float64(0)) }
func (e Cplx) Null() Item { return Cplx(complex(0, 0)) }
func (e Byte) Null() Item { return Byte(byte(0)) }
func (e Doub) Null() Item { return Doub(byte(0)) }
func (e Rune) Null() Item { return Rune(rune(' ')) }
func (e Str) Null() Item  { return Str("") }

func (e Bool) Unit() Item { return Bool(true) }
func (e Unt) Unit() Item  { return Unt(uint(1)) }
func (e Int) Unit() Item  { return Int(int(1)) }
func (e Rat) Unit() Item  { return Rat(*big.NewRat(1, 1)) }
func (e Flt) Unit() Item  { return Flt(float64(1)) }
func (e Cplx) Unit() Item { return Cplx(complex(1, 1)) }
func (e Byte) Unit() Item { return Byte(byte(1)) }
func (e Doub) Unit() Item { return Doub(byte(1)) }
func (e Rune) Unit() Item { return Rune(rune(' ')) }
func (e Str) Unit() Item  { return Str(" ") }

func (e Bool) Type() T { return T{Data, Boolean} }
func (e Unt) Type() T  { return T{Data, Unsigned} }
func (e Int) Type() T  { return T{Data, Integer} }
func (e Rat) Type() T  { return T{Data, Rational} }
func (e Flt) Type() T  { return T{Data, Irrational} }
func (e Cplx) Type() T { return T{Data, Imaginary} }
func (e Byte) Type() T { return T{Data, Binary} }
func (e Doub) Type() T { return T{Data, Double} }
func (e Rune) Type() T { return T{Data, Character} }
func (e Str) Type() T  { return T{Data, String} }

func (e Bool) Signature() T { return T{Truth} }
func (e Unt) Signature() T  { return T{Unsigned, e} }
func (e Int) Signature() T  { return T{Integer, e} }
func (e Rat) Signature() T  { return T{Rational, e} }
func (e Flt) Signature() T  { return T{Irrational, e} }
func (e Cplx) Signature() T { return T{Imaginary, e} }
func (e Byte) Signature() T { return T{Binary, e} }
func (e Doub) Signature() T { return T{Double, e} }
func (e Rune) Signature() T { return T{Character, e} }
func (e Str) Signature() T  { return T{String, e} }

func (e Bool) Symbol() Str { return Str(Boolean.String()) }
func (e Unt) Symbol() Str  { return Str(Unsigned.String()) }
func (e Int) Symbol() Str  { return Str(Integer.String()) }
func (e Rat) Symbol() Str  { return Str(Rational.String()) }
func (e Flt) Symbol() Str  { return Str(Irrational.String()) }
func (e Cplx) Symbol() Str { return Str(Imaginary.String()) }
func (e Byte) Symbol() Str { return Str(Binary.String()) }
func (e Doub) Symbol() Str { return Str(Binary.String()) }
func (e Rune) Symbol() Str { return Str(Character.String()) }
func (e Str) Symbol() Str  { return Str(String.String()) }

func (b Bool) Write() Str {
	if b {
		return "True"
	}
	return "False"
}

func (u Unt) Write() Str { return Str(strconv.Itoa(int(u))) }
func (i Int) Write() Str { return Str(strconv.Itoa(int(i))) }
func (r Rat) Write() Str { return Str((*big.Rat)(&r).String()) }
func (f Flt) Write() Str { return Str(strconv.FormatFloat(float64(f), 'G', -1, 64)) }
func (c Cplx) Write() Str {
	r, i := real(complex128(c)), imag(complex128(c))
	return Str(fmt.Sprintf("%f +%fi", r, i))
}
func (b Byte) Write() Str { return Str(b) }
func (b Doub) Write() Str { return Str(b) }
func (r Rune) Write() Str { return Str(r) }
func (s Str) Write() Str  { return Str(s) }

// SEQUENTIAL DATA
func (e BoolSeq) Ident() Item  { return e }
func (e UintSeq) Ident() Item  { return e }
func (e IntSeq) Ident() Item   { return e }
func (e RatSeq) Ident() Item   { return e }
func (e FltSeq) Ident() Item   { return e }
func (e CmplxSeq) Ident() Item { return e }
func (e ByteSeq) Ident() Item  { return e }
func (e DoubSeq) Ident() Item  { return e }
func (e RuneSeq) Ident() Item  { return e }
func (e StrSeq) Ident() Item   { return e }

func (e BoolSeq) Type() T  { return T{Boolean} }
func (e UintSeq) Type() T  { return T{Unsigned} }
func (e IntSeq) Type() T   { return T{Integer} }
func (e RatSeq) Type() T   { return T{Rational} }
func (e FltSeq) Type() T   { return T{Irrational} }
func (e CmplxSeq) Type() T { return T{Imaginary} }
func (e ByteSeq) Type() T  { return T{Binary} }
func (e DoubSeq) Type() T  { return T{Binary} }
func (e RuneSeq) Type() T  { return T{Character} }
func (e StrSeq) Type() T   { return T{String} }

func (e BoolSeq) Null() Item  { return BoolSeq{} }
func (e UintSeq) Null() Item  { return UintSeq{} }
func (e IntSeq) Null() Item   { return IntSeq{} }
func (e RatSeq) Null() Item   { return RatSeq{} }
func (e FltSeq) Null() Item   { return FltSeq{} }
func (e CmplxSeq) Null() Item { return CmplxSeq{} }
func (e ByteSeq) Null() Item  { return ByteSeq{} }
func (e DoubSeq) Null() Item  { return DoubSeq{} }
func (e RuneSeq) Null() Item  { return RuneSeq{} }
func (e StrSeq) Null() Item   { return StrSeq{} }

func (e BoolSeq) Signature() T  { return T{Sequence, T{Data, T{Boolean}}} }
func (e UintSeq) Signature() T  { return T{Sequence, T{Data, T{Unsigned}}} }
func (e IntSeq) Signature() T   { return T{Sequence, T{Data, T{Integer}}} }
func (e RatSeq) Signature() T   { return T{Sequence, T{Data, T{Rational}}} }
func (e FltSeq) Signature() T   { return T{Sequence, T{Data, T{Irrational}}} }
func (e CmplxSeq) Signature() T { return T{Sequence, T{Data, T{Imaginary}}} }
func (e ByteSeq) Signature() T  { return T{Sequence, T{Data, T{Binary}}} }
func (e DoubSeq) Signature() T  { return T{Sequence, T{Data, T{Binary}}} }
func (e RuneSeq) Signature() T  { return T{Sequence, T{Data, T{Character}}} }
func (e StrSeq) Signature() T   { return T{Sequence, T{Data, T{String}}} }

func (e BoolSeq) Symbol() Str  { return Str("[") + Boolean.Symbol() + Str("]") }
func (e UintSeq) Symbol() Str  { return Str("[") + Unsigned.Symbol() + Str("]") }
func (e IntSeq) Symbol() Str   { return Str("[") + Integer.Symbol() + Str("]") }
func (e RatSeq) Symbol() Str   { return Str("[") + Rational.Symbol() + Str("]") }
func (e FltSeq) Symbol() Str   { return Str("[") + Irrational.Symbol() + Str("]") }
func (e CmplxSeq) Symbol() Str { return Str("[") + Imaginary.Symbol() + Str("]") }
func (e ByteSeq) Symbol() Str  { return Str("[") + Binary.Symbol() + Str("]") }
func (e DoubSeq) Symbol() Str  { return Str("[") + Binary.Symbol() + Str("]") }
func (e RuneSeq) Symbol() Str  { return Str("[") + Character.Symbol() + Str("]") }
func (e StrSeq) Symbol() Str   { return Str("[") + String.Symbol() + Str("]") }

func ReturnBoolSeq(args ...Bool) BoolSeq     { return BoolSeq(args) }
func ReturnUintSeq(args ...Unt) UintSeq      { return UintSeq(args) }
func ReturnIntSeq(args ...Int) IntSeq        { return IntSeq(args) }
func ReturnRatioSeq(args ...*Rat) RatSeq     { return RatSeq(args) }
func ReturnFloatSeq(args ...Flt) FltSeq      { return FltSeq(args) }
func ReturnComplexSeq(args ...Cplx) CmplxSeq { return CmplxSeq(args) }
func ReturnByteSeq(args ...Byte) ByteSeq     { return ByteSeq(args) }
func ReturnDoubSeq(args ...Doub) DoubSeq     { return DoubSeq(args) }
func ReturnRuneSeq(args ...Rune) RuneSeq     { return RuneSeq(args) }
func ReturnStringSeq(args ...Str) StrSeq     { return StrSeq(args) }

package main

import (
	"fmt"
	"math/big"
	"strconv"
	"strings"
)

//func (KindData) String() string { return "" }
type (
	Dat uint32
	Go  interface{}
)

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
	Character
	String
	Slice
	Struct
	KeyVal
	Native
	Literal

	Bitwise = Boolean | Binary | Character | Unsigned

	Number = Unsigned | Integer | Rational |
		Irrational | Imaginary

	Text = Character | String | Literal

	Structure = Slice | Struct | KeyVal

	Transform = Native | Literal

	Value = Structure | Transform

	DataIdentity = Bitwise | Number | Text | Structure | Transform

	None Dat = 0
)

//func (DataFlag) String() string { return "" }
func (d Dat) Ident() Item    { return d }
func (d Dat) Type() Identity { return Data }
func (d Dat) Shape() T {
	if d.Composed() {
		return d.Split()
	}
	return T{}
}
func (d Dat) Cons(args ...Item) (i Item, c Cnt) {

	if len(args) > 0 {
		if t, _ := Data.Cons(args[0].Type().Type()); t != nil {
			if t, ok := t.(Dat); ok {
				if d.Contains(t) {
					i = args[0]
				}
			}
		}
		if len(args) > 1 {
			c = Suspend(d.Cons, args[1:]...)
		}
		return i, c
	}

	if cardinality(Flag(d)) < cardinality(Flag(DataIdentity)) {
		return d, (d + 1<<1).Cons
	} // ‥or wraps around to zero element of monoidal ring
	return Dat(0), Dat(1).Cons
}
func (d Dat) Len() Int    { return Int(magnitude(Flag(d))) }
func (d Dat) Number() Int { return Int(cardinality(d.Flag()) - 1) }
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
	return Str(c.Symbol())
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
func (d Dat) Flag() Flag            { return Flag(d) }
func (c Dat) Flags() []Flag         { return split(Flag(c)) }
func (d Dat) Composed() Bool        { return composed(Flag(d)) }
func (d Dat) Contains(arg Dat) Bool { return contains(Flag(d), Flag(arg)) }

func (d Dat) Split() T {
	sig := make([]Identity, 0, cardinality(Flag(d)))
	for _, f := range split(Flag(d)) {
		sig = append(sig, Identity(Dat(f)))
	}
	return sig
}
func matchData(d Dat, arg Item) (i Item) {
	// DATA FLAG
	if t := matchCategory(Data, arg.Type()); t != nil {
		if dat, ok := t.(Dat); ok {
			if d.Contains(dat) {
				return dat.Split()
			}
		}
	}
	// ARGUMENT OF TYPE DATA
	if t := matchCategory(Data, arg.Type().Type()); t != nil {
		if dat, ok := t.(Dat); ok {
			if d.Contains(dat) {
				return arg
			}
		}
	}
	return nil
}

func (d Dat) G(g Go) Item {
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
		n = Img(g.(complex128))
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
	Img  complex128
	Byte byte // ← uint8
	Rune rune // ← uint32
	Str  string

	GoToBool func(bool) Bool
	GoToUnt  func(uint) Unt
	GoToInt  func(int) Int
	GoToRat  func(*big.Rat) *Rat
	GoToFlt  func(float64) Flt
	GoToImg  func(complex128) Img
	GoToByte func(byte) Byte
	GoToRune func(rune) Rune
	GoToStr  func(string) Str

	GoToData func(Go) Item

	BoolToGo func(Bool) bool
	UntToGo  func(Unt) uint
	IntToGo  func(Int) int
	RatToGo  func(*Rat) *big.Rat
	FltToGo  func(Flt) float64
	ImgToGo  func(Img) complex128
	ByteToGo func(Byte) byte // ← uint8
	RuneToGo func(Rune) rune // ← uint32
	StrToGo  func(Str) string

	DataToGo func(Item) Go

	BoolConst func() Bool
	UntConst  func() Unt
	IntConst  func() Int
	RatConst  func() *Rat
	FltConst  func() Flt
	ImgConst  func() Img
	ByteConst func() Byte
	RuneConst func() Rune
	StrConst  func() Str

	DataConstant func() Item

	BoolMap func(Item) Bool
	UntMap  func(Item) Unt
	IntMap  func(Item) Int
	RatMap  func(Item) *Rat
	FltMap  func(Item) Flt
	ImgMap  func(Item) Img
	ByteMap func(Item) Byte
	RuneMap func(Item) Rune
	StrMap  func(Item) Str

	DataMap func(Item) Item

	BoolUnaryOp func(Bool) Bool
	UintUnaryOp func(Unt) Unt
	IntUnaryOp  func(Int) Int
	RatUnaryOp  func(*Rat) *Rat
	FltUnaryOp  func(Flt) Flt
	ImgUnaryOp  func(Img) Img
	ByteUnaryOp func(Byte) Byte
	RuneUnaryOp func(Rune) Rune
	StrUnaryOp  func(Str) Str

	DataUnary func(Item) Item

	BoolBinaryOp func(a, b Bool) Bool
	UntBinaryOp  func(a, b Unt) Unt
	IntBinaryOp  func(a, b Int) Int
	RatBinaryOp  func(a, b *Rat) *Rat
	FltBinaryOp  func(a, b Flt) Flt
	ImgBinaryOp  func(a, b Img) Img
	ByteBinaryOp func(a, b Byte) Byte
	RuneBinaryOp func(a, b Rune) Rune
	StrBinaryOp  func(a, b Str) Str

	DataBinary func(a, b Item) Item

	BoolNaryOp func(...Bool) Bool
	UintNaryOp func(...Unt) Unt
	IntNaryOp  func(...Int) Int
	RatNaryOp  func(...*Rat) *Rat
	FltNaryOp  func(...Flt) Flt
	ImgNaryOp  func(...Img) Img
	ByteNaryOp func(...Byte) Byte
	RuneNaryOp func(...Rune) Rune
	StrNaryOp  func(...Str) Str

	DataNary Fnc

	CastBool func(Item) Bool
	CastUnt  func(Item) Unt
	CastInt  func(Item) Int
	CastRat  func(Item) *Rat
	CastFlt  func(Item) Flt
	CastImg  func(Item) Img
	CastByte func(Item) Byte
	CastRune func(Item) Rune
	CastStr  func(Item) Str

	MapData func(Item) Item

	// variadic parameterized functions that take, and/or return instances
	// of base types.
	ReturnBool func(...Item) Bool
	ReturnUint func(...Item) Unt
	ReturnInt  func(...Item) Int
	ReturnRat  func(...Item) *Rat
	ReturnFlt  func(...Item) Flt
	ReturnImg  func(...Item) Img
	ReturnByte func(...Item) Byte
	ReturnRune func(...Item) Rune
	ReturnStr  func(...Item) Str

	ReturnData Fnc

	TakeBool func(...Bool) Item
	TakeUint func(...Unt) Item
	TakeInt  func(...Int) Item
	TakeRat  func(...*Rat) Item
	TakeFlt  func(...Flt) Item
	TakeImg  func(...Img) Item
	TakeByte func(...Byte) Item
	TakeRune func(...Rune) Item
	TakeStr  func(...Str) Item

	TakeData Fnc

	ContinueBool func(a Bool, args ...Item) (i Item, c Cnt)
	ContinueUnt  func(a Unt, args ...Item) (i Item, c Cnt)
	ContinueInt  func(a Int, args ...Item) (i Item, c Cnt)
	ContinueRat  func(a Rat, args ...Item) (i Item, c Cnt)
	ContinueFlt  func(a Flt, args ...Item) (i Item, c Cnt)
	ContinueImg  func(a Img, args ...Item) (i Item, c Cnt)
	ContinueByte func(a Byte, args ...Item) (i Item, c Cnt)
	ContinueRune func(a Rune, args ...Item) (i Item, c Cnt)
	ContinueStr  func(a Str, args ...Item) (i Item, c Cnt)

	DataFoldMap func(Item, ...Item) (Item, Cnt)

	// ENUMERATED BASE TYPE DATA CONSTRUCTORS
	BoolSeq []Bool
	UntSeq  []Unt
	IntSeq  []Int
	RatSeq  []*Rat
	FltSeq  []Flt
	ImgSeq  []Img
	ByteSeq []Byte
	RuneSeq []Rune
	StrSeq  []Str

	IterateBool func(a BoolSeq, args ...Bool) (fst Bool, snd BoolSeq)
	IterateUint func(a UntSeq, args ...Unt) (fst Unt, snd UntSeq)
	IterateInt  func(a IntSeq, args ...Int) (fst Int, snd IntSeq)
	IterateRat  func(a RatSeq, args ...*Rat) (fst *Rat, snd RatSeq)
	IterateFlt  func(a FltSeq, args ...Flt) (fst Flt, snd FltSeq)
	IterateImg  func(a ImgSeq, args ...Img) (fst Img, snd ImgSeq)
	IterateByte func(a ByteSeq, args ...Byte) (fst Byte, snd ByteSeq)
	IterateRune func(a RuneSeq, args ...Rune) (fst Rune, snd RuneSeq)
	IterateStr  func(a StrSeq, args ...Str) (fst Str, snd StrSeq)

	DataIterate func(Item, ...Item) (Item, Sequential)
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
func (e Img) Ident() Item  { return e }
func (e Byte) Ident() Item { return e }
func (e Rune) Ident() Item { return e }
func (e Str) Ident() Item  { return e }

func (e Bool) Type() Identity { return Boolean }
func (e Unt) Type() Identity  { return Unsigned }
func (e Int) Type() Identity  { return Integer }
func (e Rat) Type() Identity  { return Rational }
func (e Flt) Type() Identity  { return Irrational }
func (e Img) Type() Identity  { return Imaginary }
func (e Byte) Type() Identity { return Binary }
func (e Rune) Type() Identity { return Character }
func (e Str) Type() Identity  { return String }

func (e Bool) Shape() T { return T{Bool(false), Bool(true)} }
func (e Unt) Shape() T  { return T{Unt(uint(0)), Unt(uint(1))} }
func (e Int) Shape() T  { return T{Int(int(0)), Int(int(1))} }
func (e Rat) Shape() T  { return T{Rat(*big.NewRat(0, 1)), Rat(*big.NewRat(1, 1))} }
func (e Flt) Shape() T  { return T{Flt(float64(0)), Flt(float64(1))} }
func (e Img) Shape() T  { return T{Img(complex(0, 0)), Img(complex(1, 1))} }
func (e Byte) Shape() T { return T{Byte(byte(0)), Byte(byte(1))} }
func (e Rune) Shape() T { return T{Rune(rune(' ')), Rune(rune(' '))} }
func (e Str) Shape() T  { return T{Str(""), Str(" ")} }

func (e Bool) Symbol() Str { return Str(Boolean.Symbol()) }
func (e Unt) Symbol() Str  { return Str(Unsigned.Symbol()) }
func (e Int) Symbol() Str  { return Str(Integer.Symbol()) }
func (e Rat) Symbol() Str  { return Str(Rational.Symbol()) }
func (e Flt) Symbol() Str  { return Str(Irrational.Symbol()) }
func (e Img) Symbol() Str  { return Str(Imaginary.Symbol()) }
func (e Byte) Symbol() Str { return Str(Binary.Symbol()) }
func (e Rune) Symbol() Str { return Str(Character.Symbol()) }
func (e Str) Symbol() Str  { return Str(String.Symbol()) }

// atomic instances are unique and dont continue
func (e Bool) Cons(as ...Item) (i Item, c Cnt) {
	if len(as) > 0 {
	}
	return e, nil
}
func (e Unt) Cons(as ...Item) (Item, Cnt) {
	return e, nil
}
func (e Int) Cons(as ...Item) (Item, Cnt) {
	return e, nil
}
func (e Rat) Cons(as ...Item) (Item, Cnt) {
	return e, nil
}
func (e Flt) Cons(as ...Item) (Item, Cnt) {
	return e, nil
}
func (e Img) Cons(as ...Item) (Item, Cnt) {
	return e, nil
}
func (e Byte) Cons(as ...Item) (Item, Cnt) {
	return e, nil
}
func (e Rune) Cons(as ...Item) (Item, Cnt) {
	return e, nil
}
func (e Str) Cons(as ...Item) (Item, Cnt) {
	return e, nil
}

// sequences of atomic instances mimic generic sequence
func (e BoolSeq) Continue(es ...Item) (i Item, c Cnt) {
	if len(e) > 0 {
		if len(e) > 1 {
			return e[0], e[1:].Continue
		}
		return e[0], nil
	}
	return e, nil
}
func (e IntSeq) Continue(es ...Item) (Item, Cnt) {
	if len(e) > 0 {
		if len(e) > 1 {
			return e[0], e[1:].Continue
		}
		return e[0], nil
	}
	return e, nil
}
func (e UntSeq) Continue(es ...Item) (Item, Cnt) {
	if len(e) > 0 {
		if len(e) > 1 {
			return e[0], e[1:].Continue
		}
		return e[0], nil
	}
	return e, nil
}
func (e RatSeq) Continue(es ...Item) (Item, Cnt) {
	if len(e) > 0 {
		if len(e) > 1 {
			return e[0], e[1:].Continue
		}
		return e[0], nil
	}
	return e, nil
}
func (e FltSeq) Continue(es ...Item) (Item, Cnt) {
	if len(e) > 0 {
		if len(e) > 1 {
			return e[0], e[1:].Continue
		}
		return e[0], nil
	}
	return e, nil
}

func (e ImgSeq) Continue(es ...Item) (Item, Cnt) {
	if len(e) > 0 {
		if len(e) > 1 {
			return e[0], e[1:].Continue
		}
		return e[0], nil
	}
	return e, nil
}

func (e ByteSeq) Continue(es ...Item) (Item, Cnt) {
	if len(e) > 0 {
		if len(e) > 1 {
			return e[0], e[1:].Continue
		}
		return e[0], nil
	}
	return e, nil
}
func (e RuneSeq) Continue(es ...Item) (Item, Cnt) {
	if len(e) > 0 {
		if len(e) > 1 {
			return e[0], e[1:].Continue
		}
		return e[0], nil
	}
	return e, nil
}
func (e StrSeq) Continue(es ...Item) (Item, Cnt) {
	if len(e) > 0 {
		if len(e) > 1 {
			return e[0], e[1:].Continue
		}
		return e[0], nil
	}
	return e, nil
}

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
func (c Img) Write() Str {
	r, i := real(complex128(c)), imag(complex128(c))
	return Str(fmt.Sprintf("%f +%fi", r, i))
}
func (b Byte) Write() Str { return Str(b) }
func (r Rune) Write() Str { return Str(r) }
func (s Str) Write() Str  { return Str(s) }

// SEQUENTIAL DATA
func (e BoolSeq) Ident() Item { return e }
func (e UntSeq) Ident() Item  { return e }
func (e IntSeq) Ident() Item  { return e }
func (e RatSeq) Ident() Item  { return e }
func (e FltSeq) Ident() Item  { return e }
func (e ImgSeq) Ident() Item  { return e }
func (e ByteSeq) Ident() Item { return e }
func (e RuneSeq) Ident() Item { return e }
func (e StrSeq) Ident() Item  { return e }

func (e BoolSeq) Type() Identity { return Data | Sequence }
func (e UntSeq) Type() Identity  { return Data | Sequence }
func (e IntSeq) Type() Identity  { return Data | Sequence }
func (e RatSeq) Type() Identity  { return Data | Sequence }
func (e FltSeq) Type() Identity  { return Data | Sequence }
func (e ImgSeq) Type() Identity  { return Data | Sequence }
func (e ByteSeq) Type() Identity { return Data | Sequence }
func (e RuneSeq) Type() Identity { return Data | Sequence }
func (e StrSeq) Type() Identity  { return Data | Sequence }

func (e BoolSeq) Signature() T { return T{Boolean} }
func (e UntSeq) Signature() T  { return T{Unsigned} }
func (e IntSeq) Signature() T  { return T{Integer} }
func (e RatSeq) Signature() T  { return T{Rational} }
func (e FltSeq) Signature() T  { return T{Irrational} }
func (e ImgSeq) Signature() T  { return T{Imaginary} }
func (e ByteSeq) Signature() T { return T{Binary} }
func (e RuneSeq) Signature() T { return T{Character} }
func (e StrSeq) Signature() T  { return T{String} }

func (e BoolSeq) Null() Item { return BoolSeq{} }
func (e UntSeq) Null() Item  { return UntSeq{} }
func (e IntSeq) Null() Item  { return IntSeq{} }
func (e RatSeq) Null() Item  { return RatSeq{} }
func (e FltSeq) Null() Item  { return FltSeq{} }
func (e ImgSeq) Null() Item  { return ImgSeq{} }
func (e ByteSeq) Null() Item { return ByteSeq{} }
func (e RuneSeq) Null() Item { return RuneSeq{} }
func (e StrSeq) Null() Item  { return StrSeq{} }

func ReturnBoolSeq(args ...Bool) BoolSeq { return BoolSeq(args) }
func ReturnUntSeq(args ...Unt) UntSeq    { return UntSeq(args) }
func ReturnIntSeq(args ...Int) IntSeq    { return IntSeq(args) }
func ReturnRatioSeq(args ...*Rat) RatSeq { return RatSeq(args) }
func ReturnFltSeq(args ...Flt) FltSeq    { return FltSeq(args) }
func ReturnImgSeq(args ...Img) ImgSeq    { return ImgSeq(args) }
func ReturnByteSeq(args ...Byte) ByteSeq { return ByteSeq(args) }
func ReturnRuneSeq(args ...Rune) RuneSeq { return RuneSeq(args) }
func ReturnStringSeq(args ...Str) StrSeq { return StrSeq(args) }

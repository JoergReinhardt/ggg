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
	GoV interface{}
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
	TwoByte
	Character
	String
	Slice
	Struct
	KeyVal
	Native
	Literal

	Bitwise = Boolean | Binary | TwoByte | Character | Unsigned

	Numeric = Boolean | Unsigned | Integer | Rational |
		Irrational | Imaginary | Binary | TwoByte

	Text = Character | String | Literal

	Structure = Slice | Struct | KeyVal

	Transform = Native | Literal

	Value = Structure | Transform

	DataIdentity = Bitwise | Numeric | Text | Structure | Transform

	None Dat = 0
)

//func (DataFlag) String() string { return "" }
func (d Dat) Ident() Item    { return d }
func (d Dat) Type() Identity { return Data }
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

func (d Dat) Continue(args ...Item) (i Item, c Cnt) {
	if len(args) > 0 {
		arg := args[0]
		if len(args) > 1 {
			args = args[1:]
			return arg, Condense(d.Continue(args...))
		}
		if t, _ := Data.Continue(arg.Type().Type()); t != nil {
			return arg, nil
		}
		return nil, Seq(args).Continue
	}
	if d == DataIdentity {
		return d, Dat(1).Continue
	}
	return d, (d + 1<<1).Continue
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
	Bool  bool
	Unt   uint
	Int   int
	Rat   big.Rat
	Flt   float64
	Img   complex128
	Byte  byte   // ← uint8
	DByte uint16 // ← uint16
	Rune  rune   // ← uint32
	Str   string

	GoToBool func(bool) Bool
	GoToUnt  func(uint) Unt
	GoToInt  func(int) Int
	GoToRat  func(*big.Rat) *Rat
	GoToFlt  func(float64) Flt
	GoToImg  func(complex128) Img
	GoToByte func(byte) Byte
	GoToDyte func(uint16) DByte
	GoToRune func(rune) Rune
	GoToStr  func(string) Str

	GoToData func(GoV) Item

	BoolToGo func(Bool) bool
	UntToGo  func(Unt) uint
	IntToGo  func(Int) int
	RatToGo  func(*Rat) *big.Rat
	FltToGo  func(Flt) float64
	ImgToGo  func(Img) complex128
	ByteToGo func(Byte) byte    // ← uint8
	DyteToGo func(DByte) uint16 // ← uint16
	RuneToGo func(Rune) rune    // ← uint32
	StrToGo  func(Str) string

	DataToGo func(Item) GoV

	BoolConst func() Bool
	UntConst  func() Unt
	IntConst  func() Int
	RatConst  func() *Rat
	FltConst  func() Flt
	ImgConst  func() Img
	ByteConst func() Byte
	DyteConst func() DByte
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
	DyteMap func(Item) DByte
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
	DyteUnaryOp func(DByte) DByte
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
	DyteBinaryOp func(a, b DByte) DByte
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
	DyteNaryOp func(...DByte) DByte
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
	CastDyte func(Item) DByte
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
	ReturnDyte func(...Item) DByte
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
	TakeDyte func(...DByte) Item
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
	ContinueDyte func(a DByte, args ...Item) (i Item, c Cnt)
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
	DyteSeq []DByte
	RuneSeq []Rune
	StrSeq  []Str

	IterateBool func(a BoolSeq, args ...Bool) (fst Bool, snd BoolSeq)
	IterateUint func(a UntSeq, args ...Unt) (fst Unt, snd UntSeq)
	IterateInt  func(a IntSeq, args ...Int) (fst Int, snd IntSeq)
	IterateRat  func(a RatSeq, args ...*Rat) (fst *Rat, snd RatSeq)
	IterateFlt  func(a FltSeq, args ...Flt) (fst Flt, snd FltSeq)
	IterateImg  func(a ImgSeq, args ...Img) (fst Img, snd ImgSeq)
	IterateByte func(a ByteSeq, args ...Byte) (fst Byte, snd ByteSeq)
	IterateDyte func(a DByte, args ...DByte) (fst DByte, snd DByte)
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

func (e Bool) Ident() Item  { return e }
func (e Unt) Ident() Item   { return e }
func (e Int) Ident() Item   { return e }
func (e Rat) Ident() Item   { return e }
func (e Flt) Ident() Item   { return e }
func (e Img) Ident() Item   { return e }
func (e Byte) Ident() Item  { return e }
func (e DByte) Ident() Item { return e }
func (e Rune) Ident() Item  { return e }
func (e Str) Ident() Item   { return e }

func (e Bool) Null() Item  { return Bool(false) }
func (e Unt) Null() Item   { return Unt(uint(0)) }
func (e Int) Null() Item   { return Int(int(0)) }
func (e Rat) Null() Item   { return Rat(*big.NewRat(0, 1)) }
func (e Flt) Null() Item   { return Flt(float64(0)) }
func (e Img) Null() Item   { return Img(complex(0, 0)) }
func (e Byte) Null() Item  { return Byte(byte(0)) }
func (e DByte) Null() Item { return DByte(byte(0)) }
func (e Rune) Null() Item  { return Rune(rune(' ')) }
func (e Str) Null() Item   { return Str("") }

func (e Bool) Unit() Item  { return Bool(true) }
func (e Unt) Unit() Item   { return Unt(uint(1)) }
func (e Int) Unit() Item   { return Int(int(1)) }
func (e Rat) Unit() Item   { return Rat(*big.NewRat(1, 1)) }
func (e Flt) Unit() Item   { return Flt(float64(1)) }
func (e Img) Unit() Item   { return Img(complex(1, 1)) }
func (e Byte) Unit() Item  { return Byte(byte(1)) }
func (e DByte) Unit() Item { return DByte(byte(1)) }
func (e Rune) Unit() Item  { return Rune(rune(' ')) }
func (e Str) Unit() Item   { return Str(" ") }

func (e Bool) Type() Identity  { return Boolean }
func (e Unt) Type() Identity   { return Unsigned }
func (e Int) Type() Identity   { return Integer }
func (e Rat) Type() Identity   { return Rational }
func (e Flt) Type() Identity   { return Irrational }
func (e Img) Type() Identity   { return Imaginary }
func (e Byte) Type() Identity  { return Binary }
func (e DByte) Type() Identity { return TwoByte }
func (e Rune) Type() Identity  { return Character }
func (e Str) Type() Identity   { return String }

func (e Bool) Signature() T  { return T{} }
func (e Unt) Signature() T   { return T{} }
func (e Int) Signature() T   { return T{} }
func (e Rat) Signature() T   { return T{} }
func (e Flt) Signature() T   { return T{} }
func (e Img) Signature() T   { return T{} }
func (e Byte) Signature() T  { return T{} }
func (e DByte) Signature() T { return T{} }
func (e Rune) Signature() T  { return T{} }
func (e Str) Signature() T   { return T{} }

func (e Bool) Symbol() Str  { return Str(Boolean.String()) }
func (e Unt) Symbol() Str   { return Str(Unsigned.String()) }
func (e Int) Symbol() Str   { return Str(Integer.String()) }
func (e Rat) Symbol() Str   { return Str(Rational.String()) }
func (e Flt) Symbol() Str   { return Str(Irrational.String()) }
func (e Img) Symbol() Str   { return Str(Imaginary.String()) }
func (e Byte) Symbol() Str  { return Str(Binary.String()) }
func (e DByte) Symbol() Str { return Str(Binary.String()) }
func (e Rune) Symbol() Str  { return Str(Character.String()) }
func (e Str) Symbol() Str   { return Str(String.String()) }

func (e Bool) Continue(es ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(bs) > 0 { // CONCATENATE|APPLY
			// concatenate instance with arguments → Sequence
			if s, _ := (BoolSeq{e}).Continue(bs...); s != nil {
				return s, nil
			}
			return nil, Seq(bs).Continue
		} // no arguments → return instance
		return e, nil
	})(es...)
}
func (e BoolSeq) Continue(es ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(es) > 0 { // concatenate constructors arguments, if any
			if i, _ := Boolean.Continue(es[0]); i != nil {
				if len(es) > 1 {
					if s, _ := e.Continue(es[1:]...); s != nil {
						return append(e, s.(BoolSeq)...), nil
					}
				}
				e = append(e, i.(Bool))
			}
			return nil, Seq(es).Continue
		}
		return e, nil
	})(es...)
}

func (e Unt) Continue(as ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(bs) > 0 { // CONCATENATE|APPLY
			// concatenate instance with arguments → Sequence
			if s, _ := (UntSeq{e}).Continue(bs...); s != nil {
				return s, nil
			}
			return nil, Seq(bs).Continue
		} // no arguments → return instance
		return e, nil
	})(as...)
}
func (e UntSeq) Continue(es ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(es) > 0 { // concatenate constructors arguments, if any
			if i, _ := Unsigned.Continue(es[0]); i != nil {
				if len(es) > 1 {
					if s, _ := e.Continue(es[1:]...); s != nil {
						return append(e, s.(UntSeq)...), nil
					}
				}
				e = append(e, i.(Unt))
			}
			return nil, Seq(es).Continue
		}
		return e, nil
	})(es...)
}

func (e Int) Continue(as ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(bs) > 0 { // CONCATENATE|APPLY
			// concatenate instance with arguments → Sequence
			if s, _ := (IntSeq{e}).Continue(bs...); s != nil {
				return s, nil
			}
			return nil, Seq(bs).Continue
		} // no arguments → return instance
		return e, nil
	})(as...)
}
func (e IntSeq) Continue(es ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(es) > 0 { // concatenate constructors arguments, if any
			if i, _ := Integer.Continue(es[0]); i != nil {
				if len(es) > 1 {
					if s, _ := e.Continue(es[1:]...); s != nil {
						return append(e, s.(IntSeq)...), nil
					}
				}
				e = append(e, i.(Int))
			}
			return nil, Seq(es).Continue
		}
		return e, nil
	})(es...)
}

func (e *Rat) Continue(as ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(bs) > 0 { // CONCATENATE|APPLY
			// concatenate instance with arguments → Sequence
			if s, _ := (RatSeq{e}).Continue(bs...); s != nil {
				return s, nil
			}
			return nil, Seq(bs).Continue
		} // no arguments → return instance
		return e, nil
	})(as...)
}
func (e RatSeq) Continue(es ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(es) > 0 { // concatenate constructors arguments, if any
			if i, _ := Rational.Continue(es[0]); i != nil {
				if len(es) > 1 {
					if s, _ := e.Continue(es[1:]...); s != nil {
						return append(e, s.(RatSeq)...), nil
					}
				}
				e = append(e, i.(*Rat))
			}
			return nil, Seq(es).Continue
		}
		return e, nil
	})(es...)
}
func (e Flt) Continue(as ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(bs) > 0 { // CONCATENATE|APPLY
			// concatenate instance with arguments → Sequence
			if s, _ := (FltSeq{e}).Continue(bs...); s != nil {
				return s, nil
			}
			return nil, Seq(bs).Continue
		} // no arguments → return instance
		return e, nil
	})(as...)
}
func (e FltSeq) Continue(es ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(es) > 0 { // concatenate constructors arguments, if any
			if i, _ := Irrational.Continue(es[0]); i != nil {
				if len(es) > 1 {
					if s, _ := e.Continue(es[1:]...); s != nil {
						return append(e, s.(FltSeq)...), nil
					}
				}
				e = append(e, i.(Flt))
			}
			return nil, Seq(es).Continue
		}
		return e, nil
	})(es...)
}

func (e Img) Continue(as ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(bs) > 0 { // CONCATENATE|APPLY
			// concatenate instance with arguments → Sequence
			if s, _ := (ImgSeq{e}).Continue(bs...); s != nil {
				return s, nil
			}
			return nil, Seq(bs).Continue
		} // no arguments → return instance
		return e, nil
	})(as...)
}
func (e ImgSeq) Continue(es ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(es) > 0 { // concatenate constructors arguments, if any
			if i, _ := Imaginary.Continue(es[0]); i != nil {
				if len(es) > 1 {
					if s, _ := e.Continue(es[1:]...); s != nil {
						return append(e, s.(ImgSeq)...), nil
					}
				}
				e = append(e, i.(Img))
			}
			return nil, Seq(es).Continue
		}
		return e, nil
	})(es...)
}

func (e Byte) Continue(as ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(bs) > 0 { // CONCATENATE|APPLY
			// concatenate instance with arguments → Sequence
			if s, _ := (ByteSeq{e}).Continue(bs...); s != nil {
				return s, nil
			}
			return nil, Seq(bs).Continue
		} // no arguments → return instance
		return e, nil
	})(as...)
}
func (e ByteSeq) Continue(es ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(es) > 0 { // concatenate constructors arguments, if any
			if i, _ := Binary.Continue(es[0]); i != nil {
				if len(es) > 1 {
					if s, _ := e.Continue(es[1:]...); s != nil {
						return append(e, s.(ByteSeq)...), nil
					}
				}
				e = append(e, i.(Byte))
			}
			return nil, Seq(es).Continue
		}
		return e, nil
	})(es...)
}
func (e DByte) Continue(as ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(bs) > 0 { // CONCATENATE|APPLY
			// concatenate instance with arguments → Sequence
			if s, _ := (DyteSeq{e}).Continue(bs...); s != nil {
				return s, nil
			}
			return nil, Seq(bs).Continue
		} // no arguments → return instance
		return e, nil
	})(as...)
}
func (e DyteSeq) Continue(es ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(es) > 0 { // concatenate constructors arguments, if any
			if i, _ := TwoByte.Continue(es[0]); i != nil {
				if len(es) > 1 {
					if s, _ := e.Continue(es[1:]...); s != nil {
						return append(e, s.(DyteSeq)...), nil
					}
				}
				e = append(e, i.(DByte))
			}
			return nil, Seq(es).Continue
		}
		return e, nil
	})(es...)
}
func (e Rune) Continue(as ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(bs) > 0 { // CONCATENATE|APPLY
			// concatenate instance with arguments → Sequence
			if s, _ := (RuneSeq{e}).Continue(bs...); s != nil {
				return s, nil
			}
			return nil, Seq(bs).Continue
		} // no arguments → return instance
		return e, nil
	})(as...)
}
func (e RuneSeq) Continue(es ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(es) > 0 { // concatenate constructors arguments, if any
			if i, _ := Character.Continue(es[0]); i != nil {
				if len(es) > 1 {
					if s, _ := e.Continue(es[1:]...); s != nil {
						return append(e, s.(RuneSeq)...), nil
					}
				}
				e = append(e, i.(Rune))
			}
			return nil, Seq(es).Continue
		}
		return e, nil
	})(es...)
}
func (e Str) Continue(as ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(bs) > 0 { // CONCATENATE|APPLY
			// concatenate instance with arguments → Sequence
			if s, _ := (StrSeq{e}).Continue(bs...); s != nil {
				return s, nil
			}
			return nil, Seq(bs).Continue
		} // no arguments → return instance
		return e, nil
	})(as...)
}
func (e StrSeq) Continue(es ...Item) (Item, Cnt) {
	return Cnt(func(bs ...Item) (i Item, c Cnt) {
		if len(es) > 0 { // concatenate constructors arguments, if any
			if i, _ := Text.Continue(es[0]); i != nil {
				if len(es) > 1 {
					if s, _ := e.Continue(es[1:]...); s != nil {
						return append(e, s.(StrSeq)...), nil
					}
				}
				e = append(e, i.(Str))
			}
			return nil, Seq(es).Continue
		}
		return e, nil
	})(es...)
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
func (b Byte) Write() Str  { return Str(b) }
func (b DByte) Write() Str { return Str(fmt.Sprintf("%d", b)) }
func (r Rune) Write() Str  { return Str(r) }
func (s Str) Write() Str   { return Str(s) }

// SEQUENTIAL DATA
func (e BoolSeq) Ident() Item { return e }
func (e UntSeq) Ident() Item  { return e }
func (e IntSeq) Ident() Item  { return e }
func (e RatSeq) Ident() Item  { return e }
func (e FltSeq) Ident() Item  { return e }
func (e ImgSeq) Ident() Item  { return e }
func (e ByteSeq) Ident() Item { return e }
func (e DyteSeq) Ident() Item { return e }
func (e RuneSeq) Ident() Item { return e }
func (e StrSeq) Ident() Item  { return e }

func (e BoolSeq) Type() Identity { return T{Boolean} }
func (e UntSeq) Type() Identity  { return T{Unsigned} }
func (e IntSeq) Type() Identity  { return T{Integer} }
func (e RatSeq) Type() Identity  { return T{Rational} }
func (e FltSeq) Type() Identity  { return T{Irrational} }
func (e ImgSeq) Type() Identity  { return T{Imaginary} }
func (e ByteSeq) Type() Identity { return T{Binary} }
func (e DyteSeq) Type() Identity { return T{Binary} }
func (e RuneSeq) Type() Identity { return T{Character} }
func (e StrSeq) Type() Identity  { return T{String} }

func (e BoolSeq) Null() Item { return BoolSeq{} }
func (e UntSeq) Null() Item  { return UntSeq{} }
func (e IntSeq) Null() Item  { return IntSeq{} }
func (e RatSeq) Null() Item  { return RatSeq{} }
func (e FltSeq) Null() Item  { return FltSeq{} }
func (e ImgSeq) Null() Item  { return ImgSeq{} }
func (e ByteSeq) Null() Item { return ByteSeq{} }
func (e DyteSeq) Null() Item { return DyteSeq{} }
func (e RuneSeq) Null() Item { return RuneSeq{} }
func (e StrSeq) Null() Item  { return StrSeq{} }

func (e BoolSeq) Signature() T { return T{Sequence, T{Data, T{Boolean}}} }
func (e UntSeq) Signature() T  { return T{Sequence, T{Data, T{Unsigned}}} }
func (e IntSeq) Signature() T  { return T{Sequence, T{Data, T{Integer}}} }
func (e RatSeq) Signature() T  { return T{Sequence, T{Data, T{Rational}}} }
func (e FltSeq) Signature() T  { return T{Sequence, T{Data, T{Irrational}}} }
func (e ImgSeq) Signature() T  { return T{Sequence, T{Data, T{Imaginary}}} }
func (e ByteSeq) Signature() T { return T{Sequence, T{Data, T{Binary}}} }
func (e DyteSeq) Signature() T { return T{Sequence, T{Data, T{Binary}}} }
func (e RuneSeq) Signature() T { return T{Sequence, T{Data, T{Character}}} }
func (e StrSeq) Signature() T  { return T{Sequence, T{Data, T{String}}} }

func ReturnBoolSeq(args ...Bool) BoolSeq  { return BoolSeq(args) }
func ReturnUntSeq(args ...Unt) UntSeq     { return UntSeq(args) }
func ReturnIntSeq(args ...Int) IntSeq     { return IntSeq(args) }
func ReturnRatioSeq(args ...*Rat) RatSeq  { return RatSeq(args) }
func ReturnFltSeq(args ...Flt) FltSeq     { return FltSeq(args) }
func ReturnImgSeq(args ...Img) ImgSeq     { return ImgSeq(args) }
func ReturnByteSeq(args ...Byte) ByteSeq  { return ByteSeq(args) }
func ReturnDyteSeq(args ...DByte) DyteSeq { return DyteSeq(args) }
func ReturnRuneSeq(args ...Rune) RuneSeq  { return RuneSeq(args) }
func ReturnStringSeq(args ...Str) StrSeq  { return StrSeq(args) }

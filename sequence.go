package main

// SEQUENCE OF ITEMS
// TODO: implement slice tricks: https://ueokande.github.io/go-slice-tricks/
type (
	Seq []Item // cons∷f(*…)→v|pure∷f(v)→*…|unit∷f(v *ₜ)→v
)

func FMapSeq(s Seq, f Fnc) (r Seq) {
	r = make(Seq, 0, len(s))
	for n, i := range s {
		r[n] = Fnc(func(args ...Item) Item {
			if len(args) > 0 {
				return f(append(Seq{i}, args...)...)
			}
			return f(s)
		})
	}
	return r
}

func FoldSeq(s Seq, f Fnc) Seq {

	r := Seq{}

	for _, i := range s {

		e := f(i)

		if e != nil {
			r = append(r, e)
		}
	}

	return r
}

func SequenceIdentity() Seq { return Seq{} }
func EmptySequence() Seq    { return Seq{} }

// SEQUENCE
func (v Seq) Ident() Item    { return v }
func (s Seq) Null() Item     { return Seq{} }
func (s Seq) Unit() Item     { return Seq{Seq{}} }
func (s Seq) Couple() Lnk    { return Link(s.First(), s.Second()) }
func (s Seq) Type() Identity { return Sequence }
func (s Seq) Symbol() Str {
	if len(s) == 0 {
		return Str("()")
	}
	return Str("(") + s.First().Type().Symbol() + (")")
}
func (s Seq) Shape() T {
	if len(s) > 0 {
		return T{s[0].Type()}
	}
	return T{}
}

func (s Seq) Append(args ...Item) Seq { return append(s, args...) }
func (v Seq) Len() Int                { return Int(len(v)) }
func (v Seq) Pick(i Int) Item {
	if len(v) > int(i) {
		return v[i]
	}
	return nil
}

func (v Seq) TakeN(n int) Item {
	l := len(v)
	if n < l {
		return v[:n]
	}
	if n > l {
		return append(v, make(Seq, l-n, l-n)...)
	}
	return v
}

func (v Seq) Range(s, e Int) Seq {
	if len(v) > int(e) {
		return v[s:e]
	}
	return nil
}
func (v Seq) FoldEager(acc Item, f Fnc) (i Item) {
	for _, elem := range v {
		i = f(acc, elem)
	}
	return i
}
func (v Seq) Single() Bool { return len(v) == 1 }
func (v Seq) Empty() Bool  { return len(v) == 0 }
func (v Seq) First() Item {
	if len(v) > 0 {
		return v[0]
	}
	return nil
}
func (v Seq) Second() Item {
	if len(v) > 1 {
		return v[1]
	}
	return nil
}
func (v Seq) Head() Item {
	if len(v) > 0 {
		return v[0]
	}
	return nil
}
func (v Seq) Tail() Seq {
	if len(v) > 0 {
		return v[1:]
	}
	return nil
}
func (v Seq) Last() Item {
	if len(v) > 0 {
		return v[len(v)-1]
	}
	return nil
}
func (v Seq) Front() Seq {
	if len(v) > 0 {
		return v[:len(v)-1]
	}
	return nil
}
func (v Seq) Flip() Seq {
	for i, j := 0, len(v)-1; i < j; i, j = i+1, j-1 {
		v[i], v[j] = v[j], v[i]
	}
	return v
}
func (v Seq) List() Lst { return ComposeL(v...) }
func (v Seq) Pair() Lnk {
	if len(v) > 0 {
		if len(v) > 1 {
			return Link(v[0], v[1:])
		}
		return Link(v[0], nil)
	}
	return Link(nil, nil)
}
func ComposeS(args ...Item) Seq { return Seq(args) }

// TODO: iterates on empty, eppends arguments
//func (s Seq) Cons(args ...Item) (i Item, c Cnt) {
//	if len(args) > 0 {
//		return s.Head(), s.Tail().Cons
//	}
//	return s.Head(), s.Tail().Cons
//}
func ConcatS(s Seq, sqs ...Seq) Seq {
	for _, seq := range sqs {
		return append(s, seq...)
	}
	return s
}
func PrependS(s Seq, args ...Item) Seq { return append(args, s...) }
func AppendS(s Seq, args ...Item) Seq  { return append(s, args...) }

package main

// SEQUENCE OF ITEMS
// TODO: implement slice tricks: https://ueokande.github.io/go-slice-tricks/
type Seq []Item             // cons∷f(*…)→v|pure∷f(v)→*…|unit∷f(v *ₜ)→v
func SequenceIdentity() Seq { return Seq{} }
func EmptySequence() Seq    { return Seq{} }

// SEQUENCE
func (v Seq) Ident() Item    { return v }
func (s Seq) Null() Item     { return Seq{} }
func (s Seq) Unit() Item     { return Seq{Seq{}} }
func (s Seq) Couple() Lnk    { return Link(s.First(), s.Second()) }
func (s Seq) Type() Identity { return Sequence }
func (s Seq) Signature() T {
	if len(s) > 0 {
		return T{s[0].Type()}
	}
	return T{}
}
func (s Seq) Cons(args ...Item) (i Item, c Cnt) { // type & instance constructor
	return i, c
}

func (s Seq) Append(args ...Item) Seq { return append(s, args...) }
func (v Seq) Len() Int                { return Int(len(v)) }
func (v Seq) Pick(i Int) Item {
	if len(v) > int(i) {
		return v[i]
	}
	return nil
}

func (v Seq) Take(n int) Item {
	l := len(v)
	if n < l {
		return v[:n]
	}
	if n > l {
		return append(v, make(Seq, l-n, l-n))
	}
	return v
}

func (v Seq) Range(s, e Int) Seq {
	if len(v) > int(e) {
		return v[s:e]
	}
	return nil
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
func (v Seq) List() Lst { return StackL(v...) }
func (v Seq) Pair() Lnk {
	if len(v) > 0 {
		if len(v) > 1 {
			return Link(v[0], v[1:])
		}
		return Link(v[0], nil)
	}
	return Link(nil, nil)
}
func Serialize(args ...Item) Seq { return Seq(args) }
func FMapS(s Seq, f Fnc) Seq {

	var r = make(Seq, 0, len(s))

	for _, o := range s {
		r = append(r, f(o))
	}
	return r
}

func ApplyS(s Seq, f func(Seq, ...Item) (Item, Seq)) Cnt {

	return Cnt(func(args ...Item) (Item, Cnt) {
		if len(args) > 0 {
			if len(args) > 1 {
				o, m := f(s, args...)
				return o, ApplyS(m, f)
			}
			o, m := f(s, args[0])
			return o, ApplyS(m, f)
		}
		o, m := f(s)
		return o, ApplyS(m, f)
	})
}

func (a Seq) Continue(args ...Item) (i Item, c Cnt) { return i, c }
func PrependS(s Seq, args ...Item) Seq              { return append(args, s...) }
func AppendS(s Seq, args ...Item) Seq               { return append(s, args...) }
func ConcatS(s Seq, sqs ...Seq) Seq {
	for _, seq := range sqs {
		return append(s, seq...)
	}
	return s
}

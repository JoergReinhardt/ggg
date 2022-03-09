package main

// SEQUENCE
func (v Seq) Ident() Item { return v }
func (s Seq) Null() Item  { return Seq{} }
func (s Seq) Unit() Item  { return Seq{Seq{}} }
func (s Seq) Couple() Lnk { return Couple(s.First(), s.Second()) }
func (s Seq) Type() T     { return T{Sequence} }
func (s Seq) Signature() T {
	if len(s) > 0 {
		return T{s[0].Type()}
	}
	return T{}
}
func (s Seq) Cons(args ...Item) (i Item, c Continue) { // type & instance constructor
	return i, c
}
func (s Seq) Symbol() Str {
	if len(s) > 0 {
		return Str("[" + s.Signature()[0].Symbol() + "]")
	}
	return Str("[]")
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

func (v Seq) Range(s, e Int) Sequential {
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
func (v Seq) List() Lst { return Stack(v...) }
func (v Seq) Pair() Lnk {
	if len(v) > 0 {
		if len(v) > 1 {
			return Couple(v[0], v[1:])
		}
		return Couple(v[0], nil)
	}
	return Couple(nil, nil)
}
func Collect(args ...Item) Seq { return Seq(args) }
func MapSequence(s Seq, f Fnc) Seq {

	var r = make(Seq, 0, len(s))

	for _, o := range s {
		r = append(r, f(o))
	}
	return r
}

func ApplySequence(s Seq, f func(Seq, ...Item) (Item, Seq)) Continue {

	return Continue(func(args ...Item) (Item, Continue) {
		if len(args) > 0 {
			if len(args) > 1 {
				o, m := f(s, args...)
				return o, ApplySequence(m, f)
			}
			o, m := f(s, args[0])
			return o, ApplySequence(m, f)
		}
		o, m := f(s)
		return o, ApplySequence(m, f)
	})
}

func PrependSeq(s Seq, args ...Item) Seq { return append(args, s...) }
func AppendSeq(s Seq, args ...Item) Seq  { return append(s, args...) }
func ConcatSeq(s Seq, sqs ...Seq) Seq {
	for _, seq := range sqs {
		return append(s, seq...)
	}
	return s
}

// LINKED LIST
func (Lst) Type() T { return T{List} }
func (l Lst) Symbol() Str {
	if l.First() != nil {
		return Str("(") + l.First().Type().Symbol() + Str(")")
	}
	return Str("()")
}
func (l Lst) Signature() T {
	if l.First() != nil {
		return T{List, T{l.First().Type()}}
	}
	return T{List, T{EmptyList()}}
}
func (l Lst) Ident() Item                  { return l }
func (l Lst) First() Item                  { h, _ := l(); return h }
func (l Lst) Second() Item                 { _, t := l(); return t }
func (l Lst) Empty() Bool                  { h, t := l(); return h == nil && t == nil }
func (l Lst) Next() (Item, Iterator)       { return l() }
func (l Lst) Extend(args ...Item) Iterator { return l.Prepend(args...) }
func (l Lst) Concat(args ...Item) Iterator { return l.Extend(args...) }
func (l Lst) Append(args ...Item) Iterator { return ConcatLists(l, ConsList(EmptyList(), args...)) }
func (l Lst) Prepend(args ...Item) Lst {
	if len(args) > 0 {
		return ConsList(l, args...)
	}
	return l
}
func (l Lst) Cons(args ...Item) (i Item, c Iterator) {
	if len(args) > 0 {
		return ConsList(l, args...)()
	}
	return l()
}
func (l Lst) Couple() Lnk {
	var h, t = l()
	return Couple(h, t.Couple())
}
func Stack(args ...Item) Lst {
	return Lst(func() (Item, Lst) {
		if len(args) > 0 {
			if len(args) > 1 {
				return args[0], Stack(args[1:]...)
			}
			return args[0], nil
		}
		return nil, nil
	})
}

func ConcatLists(left, right Lst) Lst {
	if left == nil {
		return right
	}
	return Lst(func() (o Item, c Lst) {
		o, c = left()
		return o, ConcatLists(c, right)
	})
}

func ConsList(list Lst, args ...Item) Lst {
	if len(args) > 0 {
		if len(args) > 1 {
			return Lst(func() (Item, Lst) {
				return args[0], ConsList(list, args[1:]...)
			})
		}
		return Lst(func() (Item, Lst) {
			return args[0], list
		})
	}
	return list
}

func FoldList(
	list Lst,
	accu Item,
	fold func(
		list Lst,
		accu Item,
	) (
		result,
		accumulated Item,
		tail Lst,
	),
) Item {
	var temp Item
skip:
	if list == nil {
		return accu
	}

	temp, accu, list = fold(list, accu)

	if temp == nil {
		goto skip
	}

	return Stack(temp, FoldList(list, accu, fold))
}

func EmptyList() Lst {
	return Lst(func() (Item, Lst) {
		return nil, EmptyList()
	})
}

func FMapList(
	list Lst,
	function Fnc,
) Lst {
	if list == nil {
		return FMapList(EmptyList(), function)
	}

	var (
		head Item
		tail Lst
	)

	return Lst(func() (Item, Lst) {

		head, tail = list()

		if head == nil {
			return nil, FMapList(tail, function)
		}

		return function(head), FMapList(tail, function)
	})
}

func ApplyList(
	list Lst,
	apply func(
		list Lst,
		args ...Item) (
		current Item, tail Lst,
	)) Continue {

	if list == nil {
		return nil
	}

	return Continue(func(args ...Item) (current Item, tail Continue) {
		if len(args) > 0 {

			var head, list = apply(list, args...)

			if head == nil {
				return list, nil
			}

			return head, ApplyList(list, apply)
		}

		if current == nil {
			return list, nil
		}

		current, list = apply(list)
		return current, ApplyList(list, apply)
	})
}

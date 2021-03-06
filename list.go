package main

// LINKED LIST
// List a = Empty | ConsL a (List a) deriving (Show, Read, Eq, Ord)
type Lst func() (Item, Lst) // (x,xs)

// LINKED LIST
func (l Lst) Type() Identity { return List }
func (l Lst) Signature() T {
	if l.First() != nil {
		return T{l.First().Type()}
	}
	return T{}
}
func (l Lst) Symbol() Str {
	if l.Empty() {
		return Str("[]")
	}
	return Str("[") + l.First().Type().Symbol() + Str("]")
}
func (l Lst) Ident() Item             { return l }
func (l Lst) Head() Item              { h, _ := l(); return h }
func (l Lst) Tail() Lst               { _, t := l(); return t }
func (l Lst) First() Item             { return l.Head() }
func (l Lst) Second() Item            { return l.Tail().Head() }
func (l Lst) Empty() Bool             { h, t := l(); return h == nil && t == nil }
func (l Lst) Next() (Item, Lst)       { return l() }
func (l Lst) Extend(args ...Item) Lst { return l.Prepend(args...) }
func (l Lst) Concat(args ...Item) Lst { return l.Extend(args...) }
func (l Lst) Append(args ...Item) Lst { return ConcatL(l, ConsL(ListIdentity(), args...)) }
func (l Lst) Sequence() (s Seq) {
	f := func(l Lst, accu Item) (y, r Item, t Lst) {
		r, t = l()
		return nil, append(accu.(Seq), r), t
	}
	return FoldL(l, Seq{}, f).(Seq)
}
func (l Lst) Prepend(args ...Item) Lst {
	if len(args) > 0 {
		return ConsL(l, args...)
	}
	return l
}
func (l Lst) ConsL(args ...Item) (i Item, c Lst) {
	if len(args) > 0 {
		return ConsL(l, args...)()
	}
	return l()
}
func (l Lst) Couple() Lnk {
	var h, t = l()
	return Link(h, t.Couple())
}
func (l Lst) Cons(args ...Item) (i Item, c Cnt) {
	if len(args) > 0 {
	}
	return l, l.Cons
}
func ComposeL(args ...Item) Lst {
	return Lst(func() (Item, Lst) {
		if len(args) > 0 {
			if len(args) > 1 {
				return args[0], ComposeL(args[1:]...)
			}
			return args[0], nil
		}
		return nil, nil
	})
}

func ConcatL(left, right Lst) Lst {
	if left == nil {
		return right
	}
	return Lst(func() (o Item, c Lst) {
		o, c = left()
		return o, ConcatL(c, right)
	})
}

func ConsL(list Lst, args ...Item) Lst {
	if len(args) > 0 {
		if len(args) > 1 {
			return Lst(func() (Item, Lst) { return args[0], ConsL(list, args[1:]...) })
		}
		return Lst(func() (Item, Lst) { return args[0], list })
	} //???just return the list???
	return list
}

func FoldL(
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

	return ComposeL(temp, FoldL(list, accu, fold))
}

func EmptyList() Lst { return ListIdentity() }
func ListIdentity() Lst {
	return Lst(func() (Item, Lst) {
		return nil, ListIdentity()
	})
}

func FMapL(
	list Lst,
	function Fnc,
) Lst {
	if list == nil {
		return FMapL(ListIdentity(), function)
	}

	var (
		head Item
		tail Lst
	)

	return Lst(func() (Item, Lst) {

		head, tail = list()

		if head == nil {
			return nil, FMapL(tail, function)
		}

		return function(head), FMapL(tail, function)
	})
}

func ApplyL(
	list Lst,
	apply func(
		list Lst,
		args ...Item) (
		current Item, tail Lst,
	)) Cnt {

	if list == nil {
		return nil
	}

	return Cnt(func(args ...Item) (current Item, tail Cnt) {
		if len(args) > 0 {

			var head, list = apply(list, args...)

			if head == nil {
				return list, nil
			}

			return head, ApplyL(list, apply)
		}

		if current == nil {
			return list, nil
		}

		current, list = apply(list)
		return current, ApplyL(list, apply)
	})
}

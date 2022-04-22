package main

//// COUPLED PAIR
///
// PAIR COMPOSITION ITERATION & TRAVERSAL (happy, happy, joy, joy!)
/// iterates over pairs. second will either be a pair in the first place, or
// possibly another kind of iterator, in which case a pair of first and second
// of that iterator, reboxed as pair, will be returned as second. if second is
// neither pair, nor other kind of iterator, asymetric pair, containing second
// as its first, missing its second half, will be returned.
//
// if pair is asymetric, i.e last pair in recursion misses one of its halfs,
// last returned element will be the last value asymetricly boxed in a new
// pair, missing its second half.

type (
	Lnk func() (Item, Item) // (a,b)
)

//TODO: continue to implement return of l&r on empty call and contruction of pairs on parameter application
func LinkIdentity() Lnk { return Lnk(func() (l, r Item) { return nil, nil }) }
func EmptyLink() Lnk    { return Lnk(func() (l, r Item) { return nil, nil }) }

func (t Lnk) Ident() Item         { return t }
func (t Lnk) Left() Item          { l, _ := t(); return l }
func (t Lnk) Right() Item         { _, r := t(); return r }
func (t Lnk) Swap() Lnk           { l, r := t(); return Link(r, l) }
func (t Lnk) Empty() Bool         { l, r := t(); return l == nil && r == nil }
func (t Lnk) Sequence() Seq       { l, r := t(); return Seq{l, r} }
func (u Lnk) Type() Identity      { return Pair }
func (u Lnk) LeftType() Identity  { return u.Left().Type() }
func (u Lnk) RightType() Identity { return u.Right().Type() }
func (u Lnk) Signature() T        { f, s := u(); return T{Pair, T{f.Type(), s.Type()}} }
func (u Lnk) Symbol() Str         { return u.Left().Type().Symbol() }
func (u Lnk) Continue(args ...Item) (Item, Cnt) {
	if len(args) > 0 {
		return ComposeLnk(u, args...), nil
	}
	return u.Left(), u.Continue
}

func (u Lnk) FMap(f Fnc) Ident      { return Link(Ident(u.Left).FMap(f), Ident(u.Right).FMap(f)).Ident }
func (u Lnk) FMapLeft(f Fnc) Ident  { return Link(Ident(u.Left).FMap(f), Ident(u.Right)).Ident }
func (u Lnk) FMapRight(f Fnc) Ident { return Link(Ident(u.Left), Ident(u.Right).FMap(f)).Ident }

////////////////////////////////////////////////////////////////////////////////
func Link(l, r Item) Lnk { return Lnk(func() (Item, Item) { return l, r }) }

func ComposeLnk(l Lnk, args ...Item) Lnk {

	f, s := l()

	if f != nil {
		if s != nil { // symetric

			if len(args) > 0 {
				if len(args) > 1 {
					if len(args) > 2 {

						return Link(f, Link(s,
							ComposeLnk(Link(
								args[0], args[1],
							), args[2:]...)))
					}
					return Link(f, Link(s,
						Link(args[0], args[1])))
				}
				return Link(f, Link(s,
					Link(args[0], nil)))
			}
		}

		if len(args) > 0 { // asymetric
			if len(args) > 1 {
				if len(args) > 2 {

					return Link(f, ComposeLnk(
						Link(args[0], args[1]),
						args[2:]...))
				}
				return Link(f, Link(args[0], args[1]))
			}
			return Link(f, Link(args[0], nil))
		}
	}
	return l
}

func (t Lnk) GeneratePairs() (Item, Lnk) {

	f, p := t()

	if f != nil {
		if p != nil {
			if Pair.Contains(p.Type().Signature()[0].(Cat)) {
				return f, p.(Lnk)
			}
			return f, Link(p, nil)
		}
		return f, nil
	}
	return nil, nil
}

func (t Lnk) Concat(args ...Item) Lnk { return ComposeLnk(t, args...) }
func (t Lnk) Cons(args ...Item) (Item, Cnt) {
	if len(args) > 0 {
		return t.Cons(args...)
	}
	return t, t.Cons
}

func (t Lnk) List() Lst {
	return Lst(func() (Item, Lst) {
		f, s := t()
		return f, Link(s, nil).List()
	})
}

func MapFLink(l Lnk, f Fnc) Lnk {
	o, p := l.GeneratePairs()
	if p != nil {
		return Link(f(o), MapFLink(p, f))
	}
	return Link(f(o), f(p))
}

// RIGHT COUPLED PAIR
func LinkR(l, r Item) Lnk { return Link(r, l) }

func ComposeR(l Lnk, args ...Item) Lnk {

	f, s := l()

	if f != nil {
		if s != nil {

			if len(args) > 0 {
				if len(args) > 1 {
					if len(args) > 2 {

						return LinkR(LinkR(ComposeR(LinkR(
							args[len(args)-2], args[len(args)-1],
						), args[:len(args)-3]...), f), s)
					}
					return LinkR(f, LinkR(s, LinkR(
						args[len(args)-2], args[len(args)-1])))
				}
				return LinkR(f, LinkR(s, args[len(args)-1]))
			}
		}

		if len(args) > 0 {
			if len(args) > 1 {
				if len(args) > 2 {

					return ComposeR(ComposeR(ComposeR(LinkR(
						args[len(args)-2], args[len(args)-1],
					), args[:len(args)-3]...), f))
				}
				return LinkR(f, LinkR(args[len(args)-2],
					args[len(args)-1]))
			}
			return LinkR(f, LinkR(nil, args[len(args)-1]))
		}
	}
	return l
}

////////////////////////////////////////////////////////////////////////////////
//// TUPLE
///
// tuple constructor expects the first and second argument to be instances of
// functions, that take, or return the flattened tuple as argument and take, or
// return a generic instance of 'Ident' respectively. generic return values and
// argument sets, are expected to be properly typed.
//
// third argument is expected to be the tuples defining flat type signature,
// containing all its members sequentially, in the order they where passed.

////////////////////////////////////////////////////////////////////////////////
/// LEFT AND RIGHT BOUND
func LinkLeft(t Lnk, args ...Item) Lnk {
	if len(args) > 0 {
		f, s := t()
		fst := args[0]
		if s == nil {
			if f == nil {
				if len(args) > 1 {
					scn := args[1]
					if len(args) > 2 {
						args = args[2:]
						return LinkLeft(func() (Item, Item) {
							return fst, scn
						}, args...)
					}
					return func() (Item, Item) {
						return fst, scn
					}
				}
			}
			if len(args) > 1 {
				scn := args[1]
				if len(args) > 2 {
					args = args[2:]
					return func() (Item, Item) {
						return fst, LinkLeft(func() (Item, Item) {
							return f, scn
						}, args...)
					}
				}
				return func() (Item, Item) {
					return fst, Lnk(func() (Item, Item) {
						return f, scn
					})
				}
			}
			return func() (Item, Item) {
				return f, fst
			}
		}
		if len(args) > 1 {
			scn := args[1]
			if len(args) > 2 {
				args = args[2:]
				return func() (Item, Item) {
					return fst, Lnk(func() (Item, Item) {
						return scn, LinkLeft(Lnk(func() (Item, Item) {
							return f, s
						}), args...)
					})
				}
			}
			return func() (Item, Item) {
				return fst, Lnk(func() (Item, Item) {
					return scn, Lnk(func() (Item, Item) { return f, s })
				})
			}
		}
		return func() (Item, Item) {
			return fst, Lnk(func() (Item, Item) { return f, s })
		}
	}
	return t
}

func LinkRight(t Lnk, args ...Item) Lnk {
	if len(args) > 0 {
		l, r := t()
		fst := args[len(args)-1]
		if l == nil {
			if r == nil {
				if len(args) > 1 {
					scn := args[len(args)-2]
					if len(args) > 2 {
						args = args[:len(args)-2]
						return LinkRight(func() (Item, Item) {
							return scn, fst
						}, args...)
					}
					return func() (Item, Item) {
						return scn, fst
					}
				}
			}
			if len(args) > 1 {
				scn := args[len(args)-2]
				if len(args) > 2 {
					args = args[2:]
					return LinkRight(func() (Item, Item) {
						return LinkRight(func() (Item, Item) {
							return scn, fst
						}, args...), r
					})
				}
				return func() (Item, Item) {
					return Lnk(func() (Item, Item) {
						return scn, fst
					}), r
				}
			}
			return func() (Item, Item) {
				return fst, r
			}
		}
		if len(args) > 1 {
			scn := args[len(args)-2]
			if len(args) > 2 {
				args = args[:len(args)-2]
				return LinkRight(func() (Item, Item) {
					return LinkRight(func() (Item, Item) {
						return Lnk(func() (Item, Item) {
							return scn, fst
						}), l
					}, args...), r
				})
			}
			return LinkRight(func() (Item, Item) {
				return Lnk(func() (Item, Item) {
					return Lnk(func() (Item, Item) {
						return scn, fst
					}), l
				}), r
			})
		}
		return LinkRight(func() (Item, Item) {
			return Lnk(func() (Item, Item) {
				return fst, l
			}), r
		})
	}
	return t
}

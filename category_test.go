package main

import (
	"fmt"
	"strconv"
	"strings"
	"testing"
)

var (
	h Item
	l Lst
	b = &strings.Builder{}
)

func TestCategoryFlag(t *testing.T) {

	fmt.Printf("%v\n", Sum.Symbol())
	fmt.Printf("%s\n", None.Symbol())

	c := Identity.Sequence()
	fmt.Printf("%v\n", c)
}

func TestListCons(t *testing.T) {
	l := Stack(Identity.Sequence()...)
	for {
		h, l = l()
		if l == nil {
			break
		}
		b.WriteString("\n")
		b.WriteString(string(h.(Cat).Symbol()))
	}
	fmt.Printf("Sequence buildt from ident ∷\t\t%v\n", b.String())
	b.Reset()
	for i, f := range Identity.Sequence() {
		l = ConsList(l, Couple(Int(i), f))
	}
	for {
		if l == nil {
			break
		}
		h, l = l()
		b.WriteString(strconv.Itoa(int(h.(Lnk).First().(Int))) + " " +
			string(h.(Lnk).Second().(Cat).Symbol()) + " ")
	}
	fmt.Printf("test list on ident.split() ∷ \t\t%v\n", b.String())
	b.Reset()
}
func TestListAppend(t *testing.T) {
	l = ConcatLists(Stack(Identity.Sequence()...), Stack(Identity.Sequence().Flip()...))
	for {
		if l == nil {
			break
		}
		h, l = l()
		b.WriteString(string(h.(Cat).Symbol()) + " ")
	}
	fmt.Printf("Append flipped ident to ident ∷\t\t%v\n", b.String())
	b.Reset()
}
func TestListMap(t *testing.T) {
	l = ConcatLists(Stack(Identity.Sequence()...), Stack(Identity.Sequence().Flip()...))
	///
	l = FMapList(
		Stack(Identity.Sequence()...),
		Fnc(func(args ...Item) Item {
			arg := args[0]
			return arg.(Cat) | Category
		}),
	)
	for {
		if l == nil {
			break
		}
		h, l = l()
		b.WriteString(string(h.(Cat).Symbol()) + " ")
	}
	fmt.Printf("test List map ∷\t\t%v\n", b.String())
	b.Reset()
}

func TestApply(t *testing.T) {
	l = ConcatLists(Stack(Identity.Sequence()...), Stack(Identity.Sequence().Flip()...))
	c := ApplyList(
		Stack(Identity.Sequence()...),
		func(l Lst, args ...Item) (head Item, tail Lst) {
			head, tail = l()
			if acc, ok := head.(Lst); ok {
				head, tail = tail()
				acc = ConsList(acc, head)
				return head, ConsList(tail, acc)
			}
			seq := Stack(head.(Cat))
			return seq, ConsList(tail, seq)
		},
	)
	for {
		if c == nil {
			break
		}
		h, c = c()
	}
	l = h.(Lst)
	for {
		if l == nil {
			break
		}
		h, l = l()
		b.WriteString(string(h.(Cat).Symbol()) + " ")
	}
	fmt.Printf("tests ApplyList on ident ∷\t\t%v\n", b.String())
	b.Reset()
}
func TestFoldEager(t *testing.T) {
	c := FoldList(Stack(Identity.Sequence()...),
		Couple(Int(0), nil),
		func(
			lst Lst,
			acc Item, args ...Item) (h Item, a Item, l Lst) {
			if len(args) > 0 {
				l = Stack(args...)
				return nil, Couple(acc, None), l
			}
			h, l = lst()
			i, v := acc.(Lnk)()
			i, v = i.(Int)+1, h.(Cat)
			return nil, Couple(i, v), l
		})
	h, c = c()
	i, v := h.(Lnk)()
	fmt.Printf("test eager fold ∷\t\t%v %v\n", i, v)
	h, c = c(Identity.Sequence().Flip()...)
	i, v = h.(Lnk)()
	fmt.Printf("test eager fold flipped ∷\t\t%v %v\n", i, v)
}
func TestFoldLazy(t *testing.T) {
	c := FoldList(Stack(Identity.Sequence()...),
		Couple(Int(0), nil),
		func(
			lst Lst,
			acc Item, args ...Item) (h Item, a Item, l Lst) {
			h, l = lst()
			i, v := acc.(Lnk)()
			i = i.(Int) + 1
			v = h.(Cat) | Class
			acc = Couple(i, v)

			return acc, nil, l
		})
	h, c = c()
	fmt.Printf("test fold lazy, initial head and continuation ∷\t\t%v\n%v\n", h, c)
	i, v := h.(Lnk)()
	fmt.Printf("test lazy fold, first head in resulting list ∷\t\t%v %v\n", i, v)
}

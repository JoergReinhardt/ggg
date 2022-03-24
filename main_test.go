package main

import "fmt"

func ExampleFoldR() {
	var (
		s = IntSeq{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

		m = MapF(Inst(s), Define(func(as ...Item) Item {
			var is = as[0].(IdMap)().(IntSeq)
			if len(as) > 1 { // either concat arguments
				return Seq(append(as, as[1:]...))
			}
			if len(is) > 0 { // or yield first elem
				return is[0]
			}
			return IntSeq{}
		}, s.Type(), Integer, T{Slice | Integer}))

		f = FoldMap(m, Fnc(func(as ...Item) Item {
			i := as[0].(Int)
			if len(as) > 0 {
				return m(append([]Item{i}, as[1:]...)...)
			}
			return m(i)
		}))
	)
	fmt.Printf("s: %T∷%v\n", s, s)
	fmt.Printf("f: %v∷%v\n", f, string(f.Type().Symbol()))
	fmt.Printf("m: %v∷%v\n", m, string(m.Type().Symbol()))
	var h Item
	h, f = f()
	fmt.Printf("f(): %v %v\n", h, f) h, f = f()
	fmt.Printf("f(): %v %v\n", h, f)
	fmt.Printf("m(): %v\n", m())
	h = m()
	fmt.Printf("ma: %v\n", h)
	// Output: we'll see
}

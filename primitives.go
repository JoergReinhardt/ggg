package main

import "strings"

// FUNCTION $ ATTRIBUTE IDENTITY TYPES
//
// executable data types share certain aspects and distinct themselves in the
// ways they implement those attributes.  function attributes are part of
// function type definition. since every expression in functional programming
// is implemented by a function, function Attributes also are an integral part
// of every general expression, since anything expressed, needs some
// application of a function to act up on, in order to be evaluated.
//
// since functions are first class members all by themselves, functions may
// so all higher order functional operators are also attributes of the
// functions they operate on.
//
type (
	// FUNCTION TYPE ATTRIBUTES
	Fat Flag // ← aspects of the function type…
	Fix Flag // ← ‥one of which is its fixity

	// GENERIC FUNCTION SIGNATURES
	//  instances of Fnc implement Identity.  during evaluation of type
	//  expressions, all functions are expected to implement a generic
	//  signature,
	Fnc func(as ...Item) (a Item) //∷fₐ… generic (n-ary) function
	Bin func(a, b Item) (t Item)  //∷fₐᵦ binary function ⇔ compose/link/stack
	Una func(a Item) (b Item)     //∷fₐ unary function ⇔ fold method

	// functor instance for generic item
	Ident func() Item // implementet by each items ident method
)

////////////////////////////////////////////////////////////////////////////////
// GENERIC FUNCTIION
//

// FUNCTION TYPE ATTRIBUTES
//
//go:generate stringer -type Fat
const (
	Expression Fat = 1 << iota
	Parametric     // return type depends on parameter type
	Parameter      // argument type expected to be passed
	Polymorph      // return type has type variety
	Constrain      // constraints on parameter type|value
	Operator       // binary operator
	Argument       // variable bound and to be passed
	Nullary        // takes no arguments (boxed values)
	Pattern        // type shape as nested identities
	Partial        // expression with some bound parameters
	Reverse        // operator that reverses operand order
	Lambda         // anonymous ad-hoc funtion
	Fixity         // relative position of operands & operator
	Result         // variable that will be returned
	Assign         // link name to result
	Scope          // syntacticly visible variables
	Apply          // function and its arguments as parameter
	Curry          // argument application to curryed as arguments
	Unary          // operation expecting single operand
	Nary           // multi parameter functions (constructor)
	Bind           // bind argument to parameter
	Eval           // evaluate fully parameterized expression
	Map            // map function to item
	Dot            // map function to function (curry backwards)

	Attributes = Expression | Parametric | Parameter | Polymorph |
		Constrain | Operator | Argument | Nullary | Pattern | Partial |
		Reverse | Lambda | Fixity | Result | Assign | Scope | Apply |
		Curry | Unary | Nary | Bind | Eval | Map | Dot

	Operators = Operator | Assign | Apply | Curry | Bind | Map | Dot | Reverse

	Nop Fat = 0 // ⇐ Category.Function
)

func (t Fat) Ident() Item    { return t }
func (t Fat) Type() Identity { return Function }
func (t Fat) Shape() T       { return T{Attributes} }
func (t Fat) Symbol() Str    { return Str(t.Symbol()) }

// FIXITY ATTRIBUTE ////////////////////////////////////////////////////////////
//
// fixity demarks the side(s) function arguments are expected to be applied
// change the fixity of the resulting function relative to the contextual
// expression, the function operator is been applied in.
//
//go:generate stringer -type Fix
const (
	Suffix Fix = 1 << iota
	Infix
	Postfix

	Fixes = Suffix | Infix | Postfix
)

func (t Fix) Ident() Item    { return t }
func (t Fix) Type() Identity { return Function }
func (t Fix) Shape() T       { return T{Fixes} }
func (t Fix) Symbol() Str    { return Str(t.Symbol()) }

// FUNCTION ///////////////////////////////////////////////////////////////////

func newFncDef(
	f func(...Item) Item,
	r Identity,
	p ...Identity,
) T {
	return T{Function, T{T(p), Fnc(f), r}}
}

func newPolymorph(pattern ...T) T {
	var (
		fs, ps, rs = make(T, 0, len(pattern)), make(T, 0, len(pattern)), make(T, 0, len(pattern))
	)
	if len(pattern) > 0 {
		for _, p := range pattern {
			if p[0].Type() == nil {
				if p[0].(Cat) == Function {
					if len(p) == 2 {
						p = p[1]
					}
				}
			}
			if len(p) == 3 {
				ps = append(ps, p[0])
				fs = append(fs, p[1])
				rs = append(rs, p[2])
			}
		}
	}
	return T{Function, T{ps, fs, rs}}
}

// class Functor f where
//    fmap :: (a -> b) -> f a -> f b

// NOP ////////////////////////////////////////////////////////////////////////
// no-op does not perform any operation and returns function with empty identity,
// return type and pattern.
func EmptyFnc() Fnc { return Fnc(func(...Item) Item { return nil }) }

// generic function type methods
func (f Fnc) Ident() Item    { return f }
func (f Fnc) Type() Identity { return Function }
func (f Fnc) Shape() T {
	return T{T{Variadic, Argument}, Function, Category}
}
func (f Fnc) Symbol() Str {
	return Str(strings.Join([]string{"⊥|*|[*]", "⨍", "*"}, " → "))
}

// suspended function application is a type expression, that applies parameters
// in function invocation context and tries to apply them to functions set of
// parameters.
//
// if parameter types presentet in invocation match up to
// constitute a valid set of function parameters, i.e. a type expression that
// denotes a call with matching arguments, that type expression will be yielded
// in order to replace the function invocation and applied arguments.
//
// function suspension is fully referentially transparent and can (probably?
// TODO: ask some haskell nerd, if that is actually true) be conscidered the co
// monad to monadic funcion application.

// FUNCTION COMPOSITION AND APPLICATION
//($) :: (a -> b) -> a -> b
// suspended application
func SuspendApply(f Fnc, a Item) Fnc {
	return Fnc(func(as ...Item) Item { return f(append(as, a)...) })
}

// REVERSE SUSPENDED FUNCTION APPLICATION
//(&) : a -> (a -> b) -> b
func ReverseApply(a Item, f Fnc) Fnc {
	return Fnc(func(as ...Item) Item { return f(append([]Item{a}, as...)...) })
}

// FUNCTION CONCATENATION (currying)
//(.) :: (b -> c) -> (a -> b) -> (a -> c)
// chained concatenation
func DotConcat(f, g Fnc) Fnc {
	return Fnc(func(as ...Item) Item { return g(f(as...)) })
}

// REVERSED FUNCTION CONCATENATION
func AndConcat(f, g Fnc) Fnc {
	return Fnc(func(bs ...Item) Item { return f(g(bs...)) })
}

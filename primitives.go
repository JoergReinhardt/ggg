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

func (t Fat) Ident() Item                         { return t }
func (t Fat) Type() Identity                      { return Function }
func (t Fat) Signature() T                        { return T{Attributes} }
func (t Fat) Symbol() Str                         { return Str(t.Symbol()) }
func (t Fat) Continue(xs ...Item) (i Item, c Cnt) { return i, c }

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

func (t Fix) Ident() Item                         { return t }
func (t Fix) Type() Identity                      { return Function }
func (t Fix) Signature() T                        { return T{Fixes} }
func (t Fix) Symbol() Str                         { return Str(t.Symbol()) }
func (t Fix) Continue(xs ...Item) (i Item, c Cnt) { return i, c }

// FUNCTOR /////////////////////////////////////////////////////////////////////
//
// Ident function type is the most generic functor instance lifting arbitrary
// item into the realm of higher order functions by:
//
//    * suspending value evaluation
//    * implementing FMap, Concat, Append
//    * implementing continuation on single instance, and or all elements of
//	composal, including such that are concatenated, and|or appended, after
//	the function has been mapped.
//    * 'Ident' is implementet by default in every items 'Ident' method, which
//	can be expressed (see instance methode expression) as instance of type 'Ident'
func (i Ident) Symbol() Str      { return Functor.Symbol() + i().Type().Symbol() }
func (i Ident) Signature() T     { return T{Ident(i.Ident)} }
func (i Ident) Type() Identity   { return Functor }
func (i Ident) Ident() Item      { return i() }
func (i Ident) FMap(f Fnc) Ident { return Ident(func() Item { return f(i()) }) }
func (i Ident) Continue(args ...Item) (Item, Cnt) {
	if len(args) > 0 {
		return i, Seq(args).Continue
	}
	return i, nil
}

// FUNCTION ///////////////////////////////////////////////////////////////////
// class Functor f where
//    fmap :: (a -> b) -> f a -> f b

// NOP ////////////////////////////////////////////////////////////////////////
// no-op does not perform any operation and returns function with empty identity,
// return type and pattern.
func EmptyFnc() Fnc {
	return Define(func(...Item) Item {
		return EmptyFnc()
	}, Function, T{}, T{})
}

// FUNCTION IDENTITY //////////////////////////////////////////////////////////
// returns all its arguments unaltered and in order.
func IdentityFunction() Fnc { // when applied to arguments, function identity
	// leaves them unchanged
	return Fnc(func(xs ...Item) Item {
		if len(xs) > 0 {
			if len(xs) > 1 {
				return Seq(xs)
			}
			return xs[0]
		}
		return T{Function, T{Category, T{Category}}}
	})
}

// GENERIC FUNCTION DEFINITION ////////////////////////////////////////
//  functions are defined by an actual function definition implementing
//  signature of the generic function, some identity as the functions
//  name|symbol, another identity to denote the return value type and a
//  signature pattern of identities to define the set of parameters expected by
//  the function.
func Define(
	f func(...Item) Item,
	i, r Identity,
	p ...Identity,
) Fnc {
	return Fnc(func(as ...Item) Item {
		if len(as) > 0 {
			return f(as...)
		}
		return T{i, r, T(p)}
	})
}
func (f Fnc) Continue(as ...Item) (i Item, c Cnt) {
	if len(as) > 0 { //‥arguments where passed…
		return nil, Seq(as).Continue
	}
	return f.Signature(), nil
}

func (f Fnc) Ident() Item        { return f() }
func (f Fnc) Type() Identity     { return Function }
func (f Fnc) Signature() T       { return f().(T) }
func (f Fnc) Identity() Identity { return f.Signature()[0] }
func (f Fnc) Return() Identity   { return f.Signature()[2] }
func (f Fnc) Pattern() T         { return f.Signature()[3].(T) }
func (f Fnc) Symbol() Str {
	sts := make([]string, 0, len(f.Pattern())+1)
	for _, p := range f.Pattern() {
		sts = append(sts, string(p.Symbol()))
	}
	sts = append(sts, string(f.Return().Symbol()))
	return Str("(" + strings.Join(sts, " → ") + ")")
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
func Suspend(f Fnc, args ...Item) Fnc {
	return Define(func(...Item) (i Item) {
		return Ident(func() Item { return f(args...) })
	}, T{f, Seq(args)}, f.Return())
}

// FUNCTION COMPOSITION AND APPLICATION
//($) :: (a -> b) -> a -> b
// suspended application
func SuspendApply(f Fnc, a Item) Fnc {
	return Define(
		func(as ...Item) Item { return f(append(as, a)...) },
		T{Infix, T{Reverse | Apply, T{a.Type(), f}}}, f.Return(), f.Pattern())
}

// REVERSE SUSPENDED FUNCTION APPLICATION
//(&) : a -> (a -> b) -> b
func ReverseApply(a Item, f Fnc) Fnc {
	return Define(
		func(as ...Item) Item { return f(append([]Item{a}, as...)...) },
		T{Infix, T{Reverse | Apply, T{a.Type(), f}}}, f.Return(), f.Pattern())
}

// FUNCTION CONCATENATION (currying)
//(.) :: (b -> c) -> (a -> b) -> (a -> c)
// chained concatenation
func DotConcat(f, g Fnc) Fnc {
	return Define(
		func(as ...Item) Item { return g(f(as...)) },
		T{Infix, T{Apply | Dot, T{f, g}}}, f.Return(), g.Pattern(),
	)
}

// REVERSED FUNCTION CONCATENATION
func AndConcat(f, g Fnc) Fnc {
	return Define(
		func(bs ...Item) Item { return f(g(bs...)) },
		T{AndConcat(f, g), Infix, T{Apply | Dot | Reverse, T{f, g}}}, g.Return(), f.Pattern())
}

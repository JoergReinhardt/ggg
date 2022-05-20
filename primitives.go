package main

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
	Fnc func(...Item) Item //∷fₐ… generic (n-ary) function
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
	Unary          // operation expecting single operand
	Nary           // multi parameter functions (constructor)
	Eval           // evaluate fully parameterized expression
	Map            // map function to item
	Dot            // map function to function (curry backwards)

	Attributes = Expression | Parametric | Parameter | Polymorph |
		Constrain | Operator | Argument | Nullary | Pattern | Partial |
		Reverse | Lambda | Fixity | Result | Assign | Scope | Apply |
		Unary | Nary | Eval | Map | Dot

	Operators = Operator | Assign | Apply | Map | Dot | Reverse

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

func Def(
	f func(...Item) Item,
	r Identity,
	p ...Identity,
) Fnc {

	return Fnc(func(args ...Item) Item {
		if len(args) > 0 {
			return f(args...)
		}
		return T{r, Fnc(f), T(p)}
	})

}

// NOP ////////////////////////////////////////////////////////////////////////
// no-op does not perform any operation and returns function with empty identity,
// return type and pattern.
func EmptyFnc() Fnc { return Fnc(func(...Item) Item { return nil }) }

// generic function type methods
func (f Fnc) Ident() Item        { return f }
func (f Fnc) Type() Identity     { return Function }
func (f Fnc) Shape() T           { return f().(T) }
func (f Fnc) Return() Identity   { return f.Shape()[0] }
func (f Fnc) Identity() Identity { return f.Shape()[1] }
func (f Fnc) Params() Identity   { return f.Shape()[2] }
func (f Fnc) Symbol() Str        { return Function.Symbol() }

//($) ∷ (a -> b) -> a -> b
//(&) ∷ a -> (a -> b) -> b

//(.) ∷ (b -> c) -> (a -> b) -> (a -> c)

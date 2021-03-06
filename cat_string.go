// Code generated by "stringer -type Cat"; DO NOT EDIT.

package main

import "strconv"

func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[Function-1]
	_ = x[Class-2]
	_ = x[Data-4]
	_ = x[List-8]
	_ = x[Sequence-16]
	_ = x[Pair-32]
	_ = x[Tuple-64]
	_ = x[Record-128]
	_ = x[Conditional-256]
	_ = x[Comparative-512]
	_ = x[Choice-1024]
	_ = x[Optional-2048]
	_ = x[Variadic-4096]
	_ = x[Alternative-8192]
	_ = x[Applicative-16384]
	_ = x[Continoid-32768]
	_ = x[Monoid-65536]
	_ = x[Monad-131072]
	_ = x[Arrow-262144]
	_ = x[Optic-524288]
	_ = x[Error-1048576]
	_ = x[Nothing-0]
}

const _Cat_name = "NothingFunctionClassDataListSequencePairTupleRecordConditionalComparativeChoiceOptionalVariadicAlternativeApplicativeContinoidMonoidMonadArrowOpticError"

var _Cat_map = map[Cat]string{
	0:       _Cat_name[0:7],
	1:       _Cat_name[7:15],
	2:       _Cat_name[15:20],
	4:       _Cat_name[20:24],
	8:       _Cat_name[24:28],
	16:      _Cat_name[28:36],
	32:      _Cat_name[36:40],
	64:      _Cat_name[40:45],
	128:     _Cat_name[45:51],
	256:     _Cat_name[51:62],
	512:     _Cat_name[62:73],
	1024:    _Cat_name[73:79],
	2048:    _Cat_name[79:87],
	4096:    _Cat_name[87:95],
	8192:    _Cat_name[95:106],
	16384:   _Cat_name[106:117],
	32768:   _Cat_name[117:126],
	65536:   _Cat_name[126:132],
	131072:  _Cat_name[132:137],
	262144:  _Cat_name[137:142],
	524288:  _Cat_name[142:147],
	1048576: _Cat_name[147:152],
}

func (i Cat) String() string {
	if str, ok := _Cat_map[i]; ok {
		return str
	}
	return "Cat(" + strconv.FormatInt(int64(i), 10) + ")"
}

// Code generated by "stringer -type Fix"; DO NOT EDIT.

package main

import "strconv"

func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[Suffix-1]
	_ = x[Infix-2]
	_ = x[Postfix-4]
}

const (
	_Fix_name_0 = "SuffixInfix"
	_Fix_name_1 = "Postfix"
)

var (
	_Fix_index_0 = [...]uint8{0, 6, 11}
)

func (i Fix) String() string {
	switch {
	case 1 <= i && i <= 2:
		i -= 1
		return _Fix_name_0[_Fix_index_0[i]:_Fix_index_0[i+1]]
	case i == 4:
		return _Fix_name_1
	default:
		return "Fix(" + strconv.FormatInt(int64(i), 10) + ")"
	}
}

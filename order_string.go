// Code generated by "stringer -type Order"; DO NOT EDIT.

package main

import "strconv"

func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[MLesser - -2]
	_ = x[Lesser - -1]
	_ = x[Equal-0]
	_ = x[Greater-1]
	_ = x[MGreater-2]
}

const _Order_name = "MLesserLesserEqualGreaterMGreater"

var _Order_index = [...]uint8{0, 7, 13, 18, 25, 33}

func (i Order) String() string {
	i -= -2
	if i < 0 || i >= Order(len(_Order_index)-1) {
		return "Order(" + strconv.FormatInt(int64(i+-2), 10) + ")"
	}
	return _Order_name[_Order_index[i]:_Order_index[i+1]]
}

package main

func PlusOne(digits []int) []int {
	for i := len(digits) - 1; i >= 0; i-- {
		if digits[i] < 9 {
			digits[i]++
			break
		} else {
			digits[i] = 0
		}
	}
	if digits[0] == 0 {
		digits = append(digits, 0)
		for i := len(digits) - 1; i > 0; i-- {
			digits[i] = digits[i-1]
		}
		digits[0] = 1
	}
	return digits
}

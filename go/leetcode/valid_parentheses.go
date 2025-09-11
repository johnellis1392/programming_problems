package main

func IsValid(s string) bool {
	var stack []rune
	for _, c := range s {
		if c == '(' || c == '[' || c == '{' {
			stack = append(stack, c)
		} else if len(stack) == 0 {
			return false
		} else if (c == ')' && stack[len(stack)-1] == '(') ||
			(c == ']' && stack[len(stack)-1] == '[') ||
			(c == '}' && stack[len(stack)-1] == '{') {
			stack = stack[:len(stack)-1]
		} else {
			return false
		}
	}
	return len(stack) == 0
}

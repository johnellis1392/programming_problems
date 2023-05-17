package main

func StrStr(haystack, needle string) int {
	if len(needle) == 0 || len(needle) > len(haystack) {
		return -1
	}
	n, m := len(haystack), len(needle)
	for i := 0; i < n-m+1; i++ {
		if haystack[i:i+m] == needle {
			return i
		}
	}
	return -1
}

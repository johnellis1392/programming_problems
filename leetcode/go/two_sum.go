package main

func TwoSum(nums []int, target int) []int {
	m := make(map[int]int)
	for i, v := range nums {
		if j, ok := m[v]; ok {
			return []int{j, i}
		} else {
			m[target-v] = i
		}
	}
	return []int{}
}

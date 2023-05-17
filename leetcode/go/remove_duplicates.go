package main

func RemoveDuplicates(nums []int) int {
	i, k := 0, 1
	for i < len(nums)-1 {
		if nums[i] == nums[i+1] {
			nums = append(nums[:i], nums[i+1:]...)
		} else {
			i++
			k++
		}
	}
	return k
}

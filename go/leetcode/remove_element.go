package main

func RemoveElement(nums []int, val int) int {
	k := len(nums)
	i := 0
	for i < k {
		if nums[i] == val {
			temp := nums[i]
			nums[i] = nums[k-1]
			nums[k-1] = temp
			k--
		} else {
			i++
		}
	}
	return k
}

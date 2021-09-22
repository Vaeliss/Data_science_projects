''' sorting algos with python '''

import random

my_list = []
for i in range(50000):
	my_list.append(random.randint(0,100))

print(my_list)
my_list1 = my_list[:]
my_list2 = my_list[:]
my_list3 = my_list[:]


def bubble_sort(list_to_sort):
	''' sort in place comparing every couple of adjacent items, N times, where N is the number of items in the list (ottimizzato) '''
	for k in range(len(list_to_sort)):
		for i in range(len(list_to_sort)-1-k):
			if list_to_sort[i]>list_to_sort[i+1]:
				list_to_sort[i],list_to_sort[i+1] = list_to_sort[i+1],list_to_sort[i] 


# bubble_sort(my_list)
# print(my_list)
# Runtime è O(N^2)


def merge_sort(list_to_sort):
	if len(list_to_sort)<=1:
		return list_to_sort
	mid = len(list_to_sort) // 2
	left_list = merge_sort(list_to_sort[:mid])
	right_list = merge_sort(list_to_sort[mid:])
	return merge(left_list,right_list)



def merge(left_list,right_list):
	'''divides the list in half recursively until every list has only 1 item, after that it merges two adjacent lists sorting them,recursively, until all is sorted'''
	result_list = []
	while left_list and right_list:
		if left_list[0]<right_list[0]:
			item_to_add = left_list.pop(0)
			result_list.append(item_to_add)
		else:
			item_to_add = right_list.pop(0)
			result_list.append(item_to_add)
	if left_list:
		result_list.extend(left_list)
	elif right_list:
		result_list.extend(right_list)
	return result_list

# my_list1 = merge_sort(my_list1) 
# print(my_list1)
# (runtime è O(Nlog(N)))


def quicksort(list_to_sort,start,end):
	''' choses a random item of the list and divides it into two lists, one that contains numbers which are lesser and one in which are greater (LT e GT). 
	Does so recusively until every list has one or zero items, and put them togheter ''' 
	if start >= end:
		return
	pivot_idx = random.randint(start,end)
	pivot_element = list_to_sort[pivot_idx]
	list_to_sort[end],list_to_sort[pivot_idx] = list_to_sort[pivot_idx],list_to_sort[end]
	pointer = start
	for i in range(start,end):
		if list_to_sort[i]<pivot_element:
			list_to_sort[i],list_to_sort[pointer] = list_to_sort[pointer],list_to_sort[i]
			pointer += 1
	list_to_sort[end], list_to_sort[pointer] = list_to_sort[pointer],list_to_sort[end]
	quicksort(list_to_sort,start,pointer-1)
	quicksort(list_to_sort,pointer+1,end)

quicksort(my_list2,0,len(my_list2)-1)
print(my_list2)


def radix_sort(list_to_sort):
	pass
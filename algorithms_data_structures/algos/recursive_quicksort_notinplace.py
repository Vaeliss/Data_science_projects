# version not in place of recursive quicksort sorting algo

from random import randint

my_list = []
for i in range(200):
    my_list.append(randint(0,200))

print(my_list)

def quicksort(list_to_sort,start,end):

    if len(list_to_sort)<=1:
        return list_to_sort

    pivot_idx = randint(start,end)
    pivot_element = list_to_sort[pivot_idx]
    lesser_than = []
    greater_than = []
    for i in range(end):
        if list_to_sort[i] < pivot_element:
            lesser_than.append(list_to_sort[i])
        else:
            greater_than.append(list_to_sort[i])

    lesser_than = quicksort(lesser_than,start,len(lesser_than)-1)
    greater_than = quicksort(greater_than,start,len(greater_than)-1)

    return lesser_than + [pivot_element] + greater_than

my_list = quicksort(my_list,0,len(my_list)-1)
print(my_list)
print(len(my_list))


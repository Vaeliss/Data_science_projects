class Node():

	def __init__(self,data,next_node=None):
		self.data = data
		self.next_node = next_node

	def get_data(self):
		return self.data

	def get_next(self):
		return self.next_node

	def set_next(self,node):
		self.next_node = node


class LinkedList():
	
	def __init__(self,head_node_value = None):
		self.head_node = Node(head_node_value)

	def get_head_node(self):
		return self.head_node

	def add_node(self,value):
		new_node = Node(value)
		new_node.set_next(self.head_node)
		self.head_node = new_node

	def print_list(self):
		current_node = self.head_node
		while(current_node):
			print(current_node.get_data())
			current_node = current_node.get_next()

	def remove_node(self,value):
		current_node = self.head_node
		if current_node.get_data() == value:
			self.head_node = self.head_node.get_next()
		while(current_node.get_next()):
			if current_node.get_next().get_data() == value:
				current_node.set_next(current_node.get_next().get_next())
				return
			else:
				current_node = current_node.get_next()

class Stack():

	def __init__(self,head_node = None):
		self.head_node = head_node

	def is_empty(self):
		return self.head_node == None

	def push(self,value):
		new_node = Node(value)
		new_node.set_next(self.head_node)
		self.head_node = new_node

	def pop(self):
		if(not self.is_empty()):
			ret = self.head_node.get_data()
			self.head_node = self.head_node.get_next()
			return ret
		else:
			return None

	def top(self):
		if(not self.is_empty()):
			return self.head_node.get_data()
		else:
			return None

	def print_stack(self):
		current_node = self.head_node
		while(current_node):
			print(current_node.get_data())
			current_node = current_node.get_next()
		print("\n")



class Queue():

	def __init__(self,head_node=None):
		self.head_node = None
		self.rear_node = None

	def is_empty(self):
		return self.head_node == None

	def enqueue(self,value):
		new_node = Node(value)
		if(self.is_empty()):
			self.head_node = new_node
		else:
			self.rear_node.set_next(new_node)
		self.rear_node = new_node

	def dequeue(self):
		if(not self.is_empty()):
			node_to_remove = self.head_node
			self.head_node = self.head_node.get_next()
			return node_to_remove.get_data()
		else:
			return None

	def front(self):
		if(not self.is_empty()):
			return self.head_node.get_data()
		else:
			return None

	def print_queue(self):
		current = self.head_node
		while(current):
			print(current.get_data())
			current = current.get_next()


class Book():

	def __init__(self,id,title,author,year):
		self.id = id
		self.title = title
		self.author = author
		self.year = year

	def print_book(self):
		print(f"id: {self.id}\tTitle: {self.title}\tAuthor: {self.author}\tYear: {self.year}")


class HashTable():

	def __init__(self,size):
		self.size = size
		self.buckets = [[] for i in range(self.size)]

	def string_hash(self,title):
		value = 0
		for i in range(len(title)):
			value += ord(title[i])
		return value % self.size

	def add(self,book):
		bucket_num = self.string_hash(book.title)
		self.buckets[bucket_num].append(book)

	def print_table(self):
		for i in range(self.size):
			print(f"Bucket {i}: ")
			for book in self.buckets[i]:
				book.print_book()
			print("\n")


class BinaryTree():

	def __init__(self,root_value):
		self.root_value = root_value
		self.left_node = None
		self.right_node = None

	def add(self,value):
		if(value < self.root_value):
			if(self.left_node == None):
				self.left_node = BinaryTree(value)
				return
			else:
				self.left_node.add(value)
		if(value > self.root_value):
			if(self.right_node == None):
				self.right_node = BinaryTree(value)
				return
			else:
				self.right_node.add(value)

	def research(self,value):
		if(value < self.root_value):
			if(self.left_node == None):
				return False
			else:
				return self.left_node.research(value)
		elif(value > self.root_value):
			if(self.right_node == None):
				return False
			else:
				return self.right_node.research(value)
		elif(value == self.root_value):
			return True

	def print_preorder(self):
		print(self.root_value, end=" ")
		if(self.left_node):
			self.left_node.print_preorder()
		if(self.right_node):
			self.right_node.print_preorder()

	def print_inorder(self):
		if(self.left_node):
			self.left_node.print_inorder()
		print(self.root_value, end=" ")
		if(self.right_node):
			self.right_node.print_inorder()

	def print_postorder(self):
		if(self.left_node):
			self.left_node.print_postorder()
		if(self.right_node):
			self.right_node.print_postorder()
		print(self.root_value, end=" ")


class MinHeap():

	def __init__(self):
		self.priority_list = []

	def parent(self,idx):
		return (idx-1)//2

	def left(self,idx):
		return idx*2+1

	def right(self,idx):
		return idx*2+2

	def insert(self,value):
		idx = len(self.priority_list)
		self.priority_list.append(value)
		while(idx > 0 and self.priority_list[self.parent(idx)] > self.priority_list[idx]):
			self.priority_list[self.parent(idx)],self.priority_list[idx] = self.priority_list[idx],self.priority_list[self.parent(idx)]
			idx = self.parent(idx)

	def heapify(self,idx):
		left = self.left(idx)
		right = self.right(idx)
		smallest = idx
		if(left < len(self.priority_list) and self.priority_list[left] < self.priority_list[idx]):
			smallest = left
		if(right < len(self.priority_list) and self.priority_list[right] < self.priority_list[smallest]):
			smallest = right
		if(smallest != idx):
			self.priority_list[idx],self.priority_list[smallest] = self.priority_list[smallest],self.priority_list[idx]
			self.heapify(smallest)

	def remove(self):
		ret = None
		if(len(self.priority_list) > 0):
			ret = self.priority_list[0]
			self.priority_list[0] = self.priority_list[len(self.priority_list)-1]
			del self.priority_list[-1]
			self.heapify(0)
		return ret


class Graph:

	def __init__(self,size):
		self.size = size
		self.adj_matrix = [0 for i in range(self.size**2)]

	def add_edge(self,src,dst,value):
		self.adj_matrix[self.size*src+dst] = value

	def add_undirected_edge(self,src,dst,value):
		self.add_edge(src,dst,value)
		self.add_edge(dst,src,value)

	def print_graph(self):
		print("\n    ",end="")
		for i in range(self.size):
			print(i,end=" ")
		print("\n  __",end="")
		for i in range(self.size):
			print("__",end="")
		print("\n")
		for i in range(self.size):
			print(str(i)+" | ",end="")
			for j in range(self.size):
				print(self.adj_matrix[i*self.size+j],end=" ")
			print("\n")

	def dfs(self,start,visited):
		visited[start] = True
		print(start,end=" ")
		for i in range(self.size):
			if self.adj_matrix[start*self.size+i] != 0 and not visited[i]:
				self.dfs(i,visited)

	def dfs_visit(self,start):
		visited = [False for i in range(self.size)]
		print(f"DFS from {start}: ",end="")
		self.dfs(start,visited)
		print("\n")

	def bfs_visit(self,start):
		visited = [False for i in range(self.size)]
		visited[start] = True
		queue = Queue()
		print(f"BFS from {start}: ",end="")
		queue.enqueue(start)
		elem = queue.dequeue()
		while(elem!=None):
			print(f"{elem} ",end="")
			for i in range(self.size):
				if(self.adj_matrix[elem*self.size+i]!=0 and not visited[i]):
					visited[i] = True
					queue.enqueue(i)	
			elem = queue.dequeue()
		print("\n")
# testing


#LINKED LIST
'''
my_linked_list = LinkedList(55)
my_linked_list.add_node(22)
my_linked_list.add_node(33)
my_linked_list.add_node(11)
my_linked_list.add_node(44)
my_linked_list.print_list()
print("\n")
my_linked_list.remove_node(33)
my_linked_list.print_list()
'''

#STACK
'''
my_stack = Stack()
my_stack.push(22)
my_stack.push(33)
my_stack.push(11)
my_stack.push(44)
my_stack.push(55)
my_stack.print_stack()
print(my_stack.pop())
print(my_stack.top())
print("\n")
my_stack.print_stack()
'''

#QUEUE
'''
my_queue = Queue()
my_queue.enqueue(22)
my_queue.enqueue(33)
my_queue.enqueue(11)
my_queue.enqueue(44)
my_queue.enqueue(55)
my_queue.print_queue()
print("\n"+str(my_queue.dequeue()))
print(str(my_queue.front())+"\n")
my_queue.print_queue()
'''

#HASH TABLE (BIBLIOTECA)
'''
book1 = Book(1,"signore degli anelli","j.r.r. tolkien",1952)
book2 = Book(2,"harry potter e il calice di fuoco","j.k. rowling",2003)
book3 = Book(3,"il trono di spade","g.r.r. martin",2010)
book4 = Book(4,"la repubblica","platone",-320)
book5 = Book(5,"la divina commedia","dante alighieri",1250)
book6 = Book(6,"il decameron","boccaccio",1190)
book7 = Book(7,"la grande scommessa","sconosciuto",2011)
book8 = Book(8,"prinicipi","ray dalio",2014)
book9 = Book(9,"the intelligent investor","ben graham",1958)
book10 = Book(10,"il pifferaio","stefano dei rossi",2013)
my_table = HashTable(4)
my_table.add(book1)
my_table.add(book2)
my_table.add(book3)
my_table.add(book4)
my_table.add(book5)
my_table.add(book6)
my_table.add(book7)
my_table.add(book8)
my_table.add(book9)
my_table.add(book10)
my_table.print_table()
'''

#BINARY TREE
'''
my_tree = BinaryTree(7)
my_tree.add(12)
my_tree.add(3)
my_tree.add(1)
my_tree.add(6)
my_tree.add(16)
print(my_tree.research(16))
print(my_tree.research(12))
print("Pre order: ",end="")
my_tree.print_preorder()
print("\nIn order: ",end="")
my_tree.print_inorder()
print("\nPost order: ",end="")
my_tree.print_postorder()
'''

#MINHEAP
'''
my_heap = MinHeap()
nums = [5,12,11,9,3,5,4,3,3,2,1,5,4,3,7]
for num in nums:
	my_heap.insert(num)
element = my_heap.remove()
while(element):
	print(element,end=" ")
	element = my_heap.remove()
'''

#GRAPH

my_graph = Graph(8)
my_graph.add_undirected_edge(0,3,2)
my_graph.add_undirected_edge(1,2,4)
my_graph.add_undirected_edge(0,1,1)
my_graph.add_undirected_edge(6,5,3)
my_graph.add_undirected_edge(1,7,6)
my_graph.add_undirected_edge(4,7,9)
my_graph.add_edge(2,3,7)
my_graph.print_graph()
my_graph.dfs_visit(2)
my_graph.bfs_visit(2)
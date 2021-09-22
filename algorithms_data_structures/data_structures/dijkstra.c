#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <math.h>

#define TRUE 1
#define FALSE 0

typedef int BOOLEAN;

struct _CELL{
	int element;
	int label;
	struct _CELL* next;
};
typedef struct _CELL CELL;

struct _LinkedList{
	CELL *front, *rear;
	int size;
};
typedef struct _LinkedList LinkedList;


struct _Graph{
	LinkedList **vertici;
	int size;	
};
typedef struct _Graph Graph;


LinkedList* linked_list_new(){
	LinkedList* ret = (LinkedList*)malloc(sizeof(LinkedList));
	*ret = (LinkedList){NULL,NULL,0};
	return ret;
}

void linked_list_add(LinkedList* pList,int element,int label){
	CELL *new_cell = (CELL*)malloc(sizeof(CELL));
	new_cell->element = element;
	new_cell->label = label;
	new_cell->next = NULL;
	if(pList->front==NULL)
		pList->front = new_cell;
	else
		pList->rear->next = new_cell;
	pList->rear = new_cell;
	pList->size++;
}

int linked_list_search(LinkedList* pList,int element){
	CELL *it = pList->front;
	while(it){
		if(it->element == element)
			return it->label;
		it = it->next;
	}
	return -1;
}

BOOLEAN linked_list_deque(LinkedList *pList,int *element){
	assert(pList);
	if(!(pList->front==NULL)){
		*element = pList->front->element;
		CELL *tmp = pList->front;
		pList->front = pList->front->next;
		free(tmp);
		return TRUE;
	} else return FALSE;
}

Graph* graph_new(int num_ver){
	Graph* ret = (Graph*)malloc(sizeof(Graph));
	ret->vertici = (LinkedList**)malloc(sizeof(LinkedList*)*num_ver);
	ret->size = num_ver;
	int i;
	for(i=0;i<ret->size;++i)
		ret->vertici[i] = linked_list_new();
	return ret;
}

void graph_set(Graph *pGraph,int src,int dst,int label){
	assert(pGraph);
	linked_list_add(pGraph->vertici[src],dst,label);
}

int graph_get(Graph *pGraph,int src,int dst){
	assert(pGraph);
	return linked_list_search(pGraph->vertici[src],dst);
}

void graph_print(Graph *pGraph){
	assert(pGraph);
	int i;
	for(i=0;i<pGraph->size;++i){
		printf("%d :",i);
		CELL *it = pGraph->vertici[i]->front;
		while(it){
			printf(" -> %2d (%d)",it->element,it->label);
			it = it->next;
		}
		printf("\n");
	}
}

void graph_add_directed_edge(Graph *pGraph,int src,int dst,int label){
	graph_set(pGraph,src,dst,label);
}

void graph_add_edge(Graph *pGraph,int src,int dst,int label){
	graph_set(pGraph,src,dst,label);
	graph_set(pGraph,dst,src,label);
}

void _graph_dfs(Graph *pGraph,int v,int visited[]){
	visited[v] = TRUE;
	printf("%d ",v);
	CELL *it = pGraph->vertici[v]->front;
	while(it){
		if(!visited[it->element])
			_graph_dfs(pGraph,it->element,visited);
		it = it->next;
	}
}

void graph_dfs(Graph *pGraph,int start){
	int* visited = (int*)malloc(sizeof(int)*pGraph->size);
	int i;
	for(i=0;i<pGraph->size;++i)
		visited[i] = FALSE;
	printf("DFS from %d: ",start);
	_graph_dfs(pGraph,start,visited);
	free(visited);
	printf("\n");
}

void graph_bfs(Graph *pGraph,int start){
	int *visited = (int*)malloc(sizeof(int)*pGraph->size);
	int i;
	for(i=0;i<pGraph->size;++i)
		visited[i] = FALSE;
	LinkedList *my_queue = linked_list_new();
	visited[start] = TRUE;
	linked_list_add(my_queue,start,0);
	printf("BFS from %d: ",start);
	int my_elem;
	while(linked_list_deque(my_queue,&my_elem)){
		printf("%d ",my_elem);
		CELL *it = pGraph->vertici[my_elem]->front;
		while(it){
			if(!(visited[it->element])){
				visited[it->element] = TRUE;
				linked_list_add(my_queue,it->element,it->label);
			}
			it = it->next;
		}
	}
	printf("\n");
	free(visited);
}

int parent(int idx){
	return (idx-1)/2;	
}

int left(int idx){
	return idx*2+1;
}

int right(int idx){
	return idx*2+2;
}

struct _VerDistance{
	int vertex;
	int distance;
};
typedef struct _VerDistance VerDistance;

void swap_VerDistance(VerDistance *a,VerDistance *b){
	VerDistance tmp = *a;
	*a = *b;
	*b = tmp;
}

struct _MinHeap{
	VerDistance* elements;
	int len,size;
};
typedef struct _MinHeap MinHeap;

BOOLEAN minheap_is_empty(MinHeap* pHeap){
	return pHeap->len == 0;
}


MinHeap* minheap_new(int size){
	MinHeap* ret = (MinHeap*)malloc(sizeof(MinHeap));
	*ret = (MinHeap){(VerDistance*)malloc(sizeof(VerDistance)*size),0,size};
	return ret;
}

void minheap_grow(MinHeap* pHeap){
	if(pHeap->len < pHeap->size) return;
	else{
	VerDistance* tmp = (VerDistance*)malloc(sizeof(VerDistance)*pHeap->size*2);
	memcpy(tmp,pHeap->elements,sizeof(VerDistance)*pHeap->size);
	free(pHeap->elements);
	pHeap->elements = tmp;
	pHeap->size *= 2;	
	}
}

void minheap_free(MinHeap* pHeap){
	free(pHeap->elements);
	free(pHeap);
}

void minheap_insert(MinHeap* pHeap,int vertex,int distance){
	assert(pHeap);
	minheap_grow(pHeap);
	pHeap->elements[pHeap->len].distance = distance;
	pHeap->elements[pHeap->len].vertex = vertex;
	int i = pHeap->len;
	while(i>0 && pHeap->elements[parent(i)].distance < pHeap->elements[i].distance){
		swap_VerDistance(&pHeap->elements[parent(i)], &pHeap->elements[i]);
		i = parent(i);
	}
	pHeap->len++;
}

void minheap_heapify(MinHeap* pHeap,int start){
	int l = left(start);
	int r = right(start);
	int biggest = start;
	if(l < pHeap->len && pHeap->elements[l].distance > pHeap->elements[start].distance)
		biggest = l;
	if(r < pHeap->len && pHeap->elements[r].distance > pHeap->elements[biggest].distance)
		biggest = r;
	if(biggest != start){
		swap_VerDistance(&pHeap->elements[start],&pHeap->elements[biggest]);
		minheap_heapify(pHeap,biggest);
	}
}

BOOLEAN minheap_remove(MinHeap* pHeap,int* current_vertex,int* current_distance){
	if(pHeap->len>0){
		*current_vertex = pHeap->elements[0].vertex;
		*current_distance = pHeap->elements[0].distance;
		pHeap->elements[0] = pHeap->elements[--pHeap->len];
		minheap_heapify(pHeap,0);
		return TRUE;
	} else return FALSE;
}

BOOLEAN is_in_array(int *arr,int len,int elem){
	for(int i=0;i<len;++i){
		if(arr[i] == elem)
			return TRUE;
	} return FALSE;
}


typedef struct {
	int vertex;														
	int distance;
}BinaryRel;

void print_BinaryRel(BinaryRel* rel){								
	printf("( %d %d ) ",rel->vertex,rel->distance);
}

typedef struct{
	BinaryRel* data;									
	int size,len;												
}Map;

Map* map_new(int size){											
	Map* ret = (Map*)malloc(sizeof(Map));	
	*ret = (Map){(BinaryRel*)malloc(sizeof(BinaryRel)*size),size,0};	
	return ret;													
}

void map_grow(Map* pMap){												
	if(pMap->len < pMap->size) return;									
	BinaryRel* tmp = (BinaryRel*)malloc(sizeof(BinaryRel)*2*pMap->size);	
	memcpy(tmp,pMap->data,sizeof(BinaryRel)*pMap->size);				
	free(pMap->data);												
	pMap->data = tmp;											
	pMap->size *= 2;										
}

BOOLEAN map_contains_key(Map* pMap,int k){					
	int i;															
	for(i=0;i<pMap->len;i++){									
		if(pMap->data[i].vertex == k)								
			return TRUE;										
	} return FALSE;												
}

void map_add(Map* pMap,int vertex,int distance){						
	assert(pMap);									
	if(!map_contains_key(pMap,vertex)){	
		map_grow(pMap);										
		pMap->data[pMap->len].vertex = vertex;
		pMap->data[pMap->len].distance = distance;
		pMap->len++;				
	}
}

void map_update(Map* pMap,int vertex,int distance){
	assert(pMap);
	for(int i=0;i<pMap->len;++i)
		if(pMap->data[i].vertex == vertex)
			pMap->data[i].distance = distance;
}

int map_lookup(Map* pMap,int k){					
	int i;
	for(i=0;i<pMap->len;i++){								
		if(pMap->data[i].vertex == k)		
			return pMap->data[i].distance;	
	}
	return -200;		
}

void map_print(Map* pMap){
	int i;
	for(i=0;i<pMap->len;i++){			
		print_BinaryRel(&(pMap->data[i]));						
	}
	printf("\n");
}

Map* dijkstras(Graph* pGraph,int start){
	int current_vertex;
	int current_distance;
	int new_distance;
	Map* distances = map_new(pGraph->size);
	for(int i=0;i<pGraph->size;++i)
		map_add(distances,i,INFINITY);
	map_update(distances,start,0);
	MinHeap* my_heap = minheap_new(10);
	minheap_insert(my_heap,start,0);
	while(!minheap_is_empty(my_heap)){
		minheap_remove(my_heap,&current_vertex,&current_distance);
		CELL *it = pGraph->vertici[current_vertex]->front;
		while(it){
			new_distance = current_distance + it->label;
			if(new_distance < map_lookup(distances,it->element)){
				map_update(distances,it->element,new_distance);
				minheap_insert(my_heap,it->element,new_distance);
			}
			it = it->next;
		}
	}
	return distances;
}



int main(void){
	Graph* my_graph = graph_new(10);
	graph_add_edge(my_graph,0,2,1);
	graph_add_edge(my_graph,0,1,4);
	graph_add_edge(my_graph,2,4,3);
	graph_add_edge(my_graph,2,5,7);
	graph_add_edge(my_graph,1,7,9);
	graph_add_edge(my_graph,1,3,3);
	graph_add_edge(my_graph,1,6,5);
	graph_add_edge(my_graph,3,6,1);
	graph_add_edge(my_graph,3,8,4);
	graph_add_edge(my_graph,6,8,2);
	graph_add_edge(my_graph,8,9,1);
	int starting_point = 0;
	Map *distances = dijkstras(my_graph,starting_point);
	map_print(distances);
	return 0;
}

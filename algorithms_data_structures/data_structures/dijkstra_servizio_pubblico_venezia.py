import pprint
from math import inf


class Graph:

    def __init__(self):
        self.adj_list = {}

    def add_directed_edge(self,src,dst,weight):
        self.adj_list.setdefault(src,[])
        self.adj_list[src].append((dst,weight))

    def add_edge(self,src,dst,weight):
        self.add_directed_edge(src,dst,weight)
        self.add_directed_edge(dst,src,weight)

    def print_adj_list(self):
        pprint.pprint(self.adj_list)

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
        while(idx > 0 and self.priority_list[self.parent(idx)][1] > self.priority_list[idx][1]):
            self.priority_list[self.parent(idx)],self.priority_list[idx] = self.priority_list[idx],self.priority_list[self.parent(idx)]
            idx = self.parent(idx)

    def heapify(self,idx):
        left = self.left(idx)
        right = self.right(idx)
        smallest = idx
        if(left < len(self.priority_list) and self.priority_list[left][1] < self.priority_list[idx][1]):
            smallest = left
        if(right < len(self.priority_list) and self.priority_list[right][1] < self.priority_list[smallest][1]):
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

    def is_empty(self):
        return len(self.priority_list) == 0



def dijkstras(graph,start):
    distances = {}
    for vertex in graph.adj_list.keys():
        distances[vertex] = [inf,None]
    distances[start] = [0,None]
    visited = []
    heap = MinHeap()
    heap.insert((start,0))
    while not heap.is_empty():
        current_vertex,current_distance = heap.remove()
        if current_vertex not in visited:
            visited.append(current_vertex)
            for neighbor,edge_weight in graph.adj_list[current_vertex]:
                new_distance = current_distance + edge_weight
                if new_distance < distances[neighbor][0]:
                    distances[neighbor][0] = new_distance
                    distances[neighbor][1] = current_vertex
                    heap.insert((neighbor,new_distance))
    return distances

def dijkstras_target(graph,start,target):
    ''' ottiene il percorso più breve fra i vertici start e target, ritornando percorso e tempo di percorrenza stimato'''
    distances = {}
    for vertex in graph.adj_list.keys():
        distances[vertex] = [inf,None]
    distances[start] = [0,None]
    visited = []
    heap = MinHeap()
    heap.insert((start,0))
    while not heap.is_empty():
        current_vertex,current_distance = heap.remove()
        if current_vertex == target:
            break
        if current_vertex not in visited:
            visited.append(current_vertex)
            try:
                for neighbor,edge_weight in graph.adj_list[current_vertex]:
                    new_distance = current_distance + edge_weight
                    if new_distance < distances[neighbor][0]:
                        distances[neighbor][0] = new_distance
                        distances[neighbor][1] = current_vertex
                        heap.insert((neighbor,new_distance))
            except:
                print("Source doens't exist in the database")
                return None,None
    try:
        if distances[target][0] != inf:
            distance = distances[target][0]
            path = []
            elem = target
            path.append(target)
            while(distances[elem][1] != None):
                path.append(distances[elem][1])
                elem = distances[elem][1]
            path.reverse()
            return distance,path
        else:
            print("A path wasn't found.")
    except:
        print("The destination you have selected doesn't exist in the database")
        return None,None
        


my_graph = Graph()
connections = [("p.le roma","ferrovia",5),("p.le roma","santa marta",5),("santa marta","san basilio",5),
               ("san basilio","sacca fisola",3),("san basilio","tronchetto",10),("p.le roma","tronchetto",7),
               ("san basilio","zattere",3),("sacca fisola","palanca",4),("zattere","palanca",3),
               ("palanca","redentore",3),("redentore","zitelle",3),("zitelle","san giorgio",3),
               ("san giorgio","san zaccaria",5),("santo spirito","zattere",5),("san zaccaria","san marco",4),
               ("san marco","salute",2),("salute","giglio",3),("giglio","accademia",4),("accademia","ca' rezzonico",3),
               ("ca' rezzonico","san tomà",5),("san tomà","sant'angelo",2),("sant'angelo","san silvestro",2),
               ("san silvestro","rialto",4),("rialto","rialto mercato",3),("rialto mercato","ca' d'oro",3),
               ("ca' d'oro","san stae",3),("san stae","san marcuola",3),("san marcuola","riva de biasio",4),
               ("riva de biasio","ferrovia",3),("riva de biasio","guglie",3),("ferrovia","guglie",5),
               ("guglie","tre archi",3),("tre archi","sant'alvise",7),("sant'alvise","orto",4),
               ("orto","f.te nove",5),("f.te nove","murano colonna",5),("f.te nove","cimitero",3),
               ("murano faro","burano",35),("murano faro","f.te nove",10),("murano faro","torcello",30),("burano","torcello",5),
               ("burano","treporti",15),("treporti","punta sabbioni",25),("punta sabbioni","lido",20),
               ("lido","san zaccaria",10),("lido","sant'elena",5),("sant'elena","giardini",5),
               ("giardini","arsenale",4),("san zaccaria","san servolo",7),("san servolo","san lazzaro",7),
               ("f.te nove","marcopolo",40),("p.le roma","marcopolo",30),("lido","marcopolo",60),
               ("san zaccaria","marcopolo",60),("lido","pellestrina",60),("pellestrina","chioggia",25),
               ("p.le roma","chioggia",100),("chioggia","marcopolo",100),("san zaccaria","arsenale",4),
               ("murano colonna","cimitero",4),("f.te nove","sant'erasmo",30),("murano serenella","murano colonna",7),
               ("murano serenella","murano venier",5),("murano venier","murano museo",5),("murano museo","murano navagero",5),
               ("murano navagero","murano faro",5),("mazzorbo","murano faro",25),("mazzorbo","torcello",10),
               ("f.te nove","ospedale",5),("ospedale","celestia",5),("celestia","bacini",5),("bacini","san pietro",10),
               ("san pietro","certosa",5),("lido","certosa",12),("f.te nove","vignole",25),("vignole","sant'erasmo",10),
               ("sant'erasmo","lazzaretto nuovo",5),("san samuele","accademia",3),("crea","guglie",3),
               ("zattere","fusina",60)]
for connection in connections:
    my_graph.add_edge(connection[0],connection[1],connection[2])


punto_partenza = "santa marta"
punto_arrivo = "burano"
distance,path = dijkstras_target(my_graph,punto_partenza,punto_arrivo)
if distance and path:
    print(f"Time from {punto_partenza} to {punto_arrivo}: {distance} minutes\nPath: {path}")

print("\n\n")
distances = dijkstras(my_graph,punto_partenza)
print(f"Da {punto_partenza}:")
for k,v in distances.items():
    print(f"Per arrivare a {k} ci vogliono {v[0]} minuti")

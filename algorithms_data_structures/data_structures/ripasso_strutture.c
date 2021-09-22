/* ripasso strutture dati*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define TRUE 1
#define FALSE 0
typedef int BOOLEAN;											


// scommentare le varie implementazioni per testarle/usarle, sotto ogni STUTTURA IMPLEMENTAZIONE TIPO c'è l'inizio del commento multi riga (/* fino a */), ricordarsi si cancellare anche quello finale (*/) per far funzionare il codice 
// i nomi delle variabili (di solito) sono evocativi dei tipi di variabili o della loro utilità. per esempio un puntatore ad una struttura inizierà quasi sempre con p (minuscola) seguita dal nome abbreviato della struttura (per esempio pMap)

// in fondo alle implementazioni si trova il main con tutti quanti i test disponibili per le varie strutture ed implementazioni

// LEGENDA (approx, può leggermente variare con nuovi commenti/correzioni)
//		PILA linea 31
//		CODA linea 151
// 		CONTAINER e SET linea 358
//		MAPPA linea 630
//		MULTIMAPPA linea 780
//		HASH TABLE linea 919
//		ALBERI linea 1074
// 		main linea 1192



// PILA IMPLEMENTAZIONE ARRAY
/*
struct _STACK{
	int* elements;												//puntatore ad int, array di int
	int size,head;												//size è la dimensione dell'array dinamico, head è l'indice del primo posto libero nella pila
};
typedef struct _STACK STACK;									//così non bisogna scrivere ogni volta struct (struct _STACK si può ora scrivere semplicemente come STACK)


void grow_stack(STACK* pS){										//funzione usata per l'ingrandimento dell'array durante l'aggiunta di elementi alla pila (se necessario)
	if(pS->elements == NULL){									//se il puntatore/l'array non è stato inizializzato
		pS->elements = (int*)malloc(sizeof(int)*pS->size);		//allocalo dinamicamente
		pS->head = 0; 											//l'array appena creato è vuoto quindi head viene settato a 0
	}
	else if(pS->head<pS->size) return;							// se l'array non è pieno ritorna senza far nulla
	else{														//nel caso in cui sia pieno
		int* tmp = (int*)malloc(sizeof(int)*pS->size*2);		//si crea un array temporaneo avente grandezza doppia rispetto al precedente
		memcpy(tmp,pS->elements,sizeof(int)*pS->size);			//si copia il contenuto della pila nel nuovo array creato
		free(pS->elements);										//si libera la memoria allocata per l'array precedente
		pS->elements = tmp;										//si assegna il puntatore al nuovo array creato, in questo modo abbiamo raddopiato la grandezza dell'array della pila ed è ora semipieno
		pS->size*=2;											//si raddoppia l'int che segna la grandezza dell'array, avendola raddoppiata
	}
}

BOOLEAN is_empty(STACK* pS){									//funzione che indica se la pila è vuota
	return pS->head==0;
}

int size(STACK* pS){											//funzione che ritorna la quantità di elementi inseriti nella pila
	return pS->head;	
}

void push(STACK* pS,int element){								//funzione che aggiunge un elemento in cima alla pila
	assert(pS);													//controlla che il puntatore alla struttura non sia NULL
	grow_stack(pS);												//chiama la funzione di crescita dell'array, che (se necessario) viene ampliato per far spazio al nuovo elemento da inserire (e a successivi)
	pS->elements[pS->head++] = element;							//l'elemento viene inserito nella posizione corrispondente all'head dell'array, head viene subito dopo aumentata di 1, ad indicare la prossima posizione libera
}

BOOLEAN pop(STACK* pS,int* element){							//funzione che rimuove un elemento dalla cima della pila (come ci si aspetta da una struttura LIFO) 
	assert(pS);													//controlla che il puntatore alla struttura non sia NULL
	if(!is_empty(pS)){											//se la struttura non è vuota:
		*element = pS->elements[--pS->head];					//assegna l'elemento in cima alla pila (ossia in posizione head-1) alla variabile puntata dal puntatore ad int. Head viene nello stesso passaggio diminuito di 1, avendo rimosso un elemento
		return TRUE;											//ritorna vero se è stato rimosso un elemento, ossia quando la struttura non è vuota
	} else return FALSE;										//se la struttura è vuota ritorna falso, non ci sono elementi da rimuovere dalla cima della pila
}


BOOLEAN top(STACK* pS,int* element){							//funzione che copia l'elemento in cima alla pila ma non lo rimuove a differenza di pop()
	assert(pS);
	if(!is_empty(pS)){
		*element = pS->elements[pS->head - 1];					//a differenza di pop, head non viene diminuita di uno, stiamo solo accedendo all'indice head-1, ma non viene riassegnata head
		return TRUE;
	} else return FALSE;
}

void print_stack(STACK* pS){									//funzione per stampare a video tutti gli elementi della pila, seguendo l'ordine di uscita dalla pila (struttura LIFO)
	int i;
	for(i=pS->head-1;i>=0;i--)									//si parte dall'indice head-1, dove c'è l'elemento in cima alla pila, e si arriva fino all'elemento all'indice 0, ossia quello alla base della pila
		printf("%d ",pS->elements[i]);							//stampa dell'i-esimo elemento
	printf("\n");												//alla fine si va a caporiga
}
*/






// PILA COME LINKED LIST
/*
struct CELL{
	int element;												//elemento (in sto caso int ma potrebbe essere una qualsiasi struttura) della singola cella
	struct CELL* next;											//puntatore alla prossima cella
};

struct _STACK{
	struct CELL* head;											//la stuttura STACK non è altro che una struttura contenente un puntatore all'ultima cella della lista concatenata (la "cima" della pila)
};
typedef struct _STACK STACK;									//trucco di prima per evitare di scrivere sempre struct 

BOOLEAN is_empty(STACK* pS){									//funzione che ritorna un boolean indicante se la pila è vuota o meno
	return pS->head == NULL;									
}

void push(STACK* pS,int element){											//crea una nuova cella in cima alla pila
	struct CELL* new_cell = (struct CELL*)malloc(sizeof(struct CELL));		//creazione nuova cella
	new_cell->element = element;											//assegna come elemento della cella l'intero passato per valore
	new_cell->next = pS->head;												//asegna come prossima cella di quella appena creata quella che era la cima della pila
	pS->head = new_cell;													//ora la cima della pila è la nuova cella creata
}

BOOLEAN pop(STACK* pS,int* element){										//rimuove una cella dalla cima della pila (se c'è)
	if(!is_empty(pS)){														//se non è vuota la pila....
		*element = pS->head->element;										//copia l'elemento della cima della pila nella variabile intera che era stata passsata per riferimento alla funzione
		pS->head = pS->head->next;											//assegna la cima della pila alla cella seguente
		return TRUE;														
	} else return FALSE;													//se la pila era vuota e quindi non è stata rimossa alcuna cella, ritorna falso
}

BOOLEAN top(STACK* pS,int* element){							//si va a vedere qual è l'elemento della cella in cima alla pila, senza rimuovere la cella come succedeva in pop()
	if(!is_empty(pS)){
		*element = pS->head->element;							//a differenza di pop() infatti manca il passaggio di riassegnazione della cima della pila, la pila rimane immutata
		return TRUE;
	} else return FALSE;										//anche in sto caso se è stato possibile trovare il valore in cima alla pila ritorna vero, se invece la pila era vuota, ritorna falso
}

void print_stack(STACK* pS){									//stampa gli elementi della pila in ordine LIFO
	assert(pS);													//controlla che il puntatore non sia NULL
	struct CELL* it = pS->head;									//creo un iteratore, ossia voglio attraversare la pila ma senza andarla a modificare
	while(it){													//fintanto che l'iteratore (il puntatore alla cella) non è NULL...
		printf("%d ",it->element);								//...stampa l'elemento della cella..
		it = it->next;											//e una vola fatto assegna l'iteratore alla cella successiva
	}															//il ciclo termina quando l'iteratore, che era assegnato alla cella in fondo alla pila, viene assegnato alla cella seguente, che non esistendo è NULL 
	printf("\n");												//infine per formattazione adeguata si va a caporiga
} 
*/




// QUEUE IMPLEMENTATA COME ARRAY SEMPLICE
/*
struct _QUEUE{
	int* elements;												//array di interi (potrebbe essere array di qualsiasi struttura)
	int head,size;												//head è il primo posto libero in coda alla fila, size è la grandezza dell'array dinamico
};																//in questa implementazione è stato scelto di fare incolonnare gli elementi in posizione head, e tenere il primo della coda in posizione 0, si può fare anche il contrario...
//																//....in quel caso enqueue e deque sono scambiati come implementazione ovviamente
typedef struct _QUEUE QUEUE;									//solito trucco per evitare di scrivere struct ogni volta

BOOLEAN is_empty(QUEUE* pQ){									//funzione che indica se la coda è vuota
	return pQ->head == 0;										//se la coda è vuota la prima posizione libera in cui incolonnare un elemento è 0 logicamente
}

void grow_queue(QUEUE* pQ){										//funzione per ampliare se necessario l'array, o crearlo
	if(pQ->elements == NULL){									//nel caso in cui non sia mai stata allocata la memoria necessaria per l'array
		pQ->elements = (int*)malloc(sizeof(int)*pQ->size);		//viene allocata
		pQ->head = 0;											//la coda appena creata ha primo posto disponibile 0 (è vuota)
	} else if (pQ->head<pQ->size) return;						//nel caso,invece, che la coda non sia del tutto piena la funzione ritorna senza far nulla
	else{														//nell'ultimo caso, se la coda è piena...
		int* tmp = (int*)malloc(sizeof(int)*2*pQ->size);		//..si crea un nuovo array di grandezza doppia...
		memcpy(tmp,pQ->elements,sizeof(int)*pQ->size);			//..si copiano gli elementi del vecchio array in quello nuovo
		free(pQ->elements);										//si libera la memoria relativa al vecchio array
		pQ->elements = tmp;										//si assegna ufficialmente l'array della coda a quello appena creato
		pQ->size *= 2;											//avendo l'array appena creato grandezza doppia rispetto al precedente si raddoppia size
	}
}

void enqueue(QUEUE* pQ,int element){							//funzione per inserire un elemento in coda
	assert(pQ);													//si controlla che il puntatore alla struttura non sia NULL o ci sia qualche problema
	grow_queue(pQ);												//si chiama la funzione che controlla se l'array è già stato creato / deve essere ampliato o vada già tutto bene
	pQ->elements[pQ->head++] = element;							//si inserisce l'elemento in fondo alla coda(in posizione head dell'array) e si aumenta head di uno, essendo la posizione libera in fondo alla coda una posizione più in là
}

BOOLEAN deque(QUEUE* pQ,int* element){							//funzione per rimuovere un elemento da davanti la coda, come ci si aspetta da una struttura FIFO
	assert(pQ);													
	if(!is_empty(pQ)){											//se la coda non è vuota...
		*element = pQ->elements[0];								//copia l'elemento in fronte alla coda nella variabile intera passata alla funzione, tramite puntatore
		int i;													//a questo punto, avendo liberato la posizione 0 della coda, tutti gli elementi dell'array fanno "un passo avanti"
		pQ->head--;												//anche head diminuisce di uno
		for(i=0;i<pQ->head;i++){
			pQ->elements[i] = pQ->elements[i+1];				//ogni elemento passa dalla posizione i+1 esima, alla posizione i-esima
		}
		return TRUE;											//se è avvenuta la rimozione(cioè sempre quando la stuttura non è vuota), ritorna vero
	} else return FALSE;										//se non è avvenuta alcuna rimozione(ossia la struttura era vuota), ritorna falso
}

BOOLEAN front(QUEUE* pQ,int* element){							//funzione che guarda chi c'è in prima posizione alla coda, senza rimuoverlo
	assert(pQ);
	if(!is_empty(pQ)){
		*element = pQ->elements[0];								//a differenza di deque, non avviene lo spostamento di una posizione di alcun elemento, neanche di head, la coda è immutata
		return TRUE;
	} else return FALSE;
}

void print_queue(QUEUE* pQ){									//funzione che stampa a video gli elementi della coda, nell'ordine che ci si aspetta da una struttura FIFO 
	assert(pQ);
	int element;												//(dovrebbe essere chiamata pià precisamente print_clear_queue)
	while(deque(pQ,&element)) printf("%d ",element);			//infatti in realtà usando deque questa funzione non è proprio di stampa ma è di stampa/svuotamento, ossia gli elementi vengono stampati man mano che vengono rimossi
	printf("\n");												//al termine di questa funzione la coda passata risulterà quindi vuota
} 																//se si volesse fare una funzione solo di stampa basterebbe attraversare l'array dalla posizione 0 alla posizione head(non compresa) tramite ciclo for
*/




// QUEUE IMPLEMENTATA COME ARRAY CIRCOLARE						//per evitare di far scalare di una posizione tutti gli elementi dell'array nel caso di deque/enqueue(a seconda dell'implementazionee) si può pensare a implementare tramite array circolare
/*												
struct _QUEUE{													
	int* elements;												
	int front,back,size;										//per far ciò si usa un doppio riferimento, non più solo alla fronte o alla fine della coda, ma sia al fronte(front) che alla fine(back) della coda
};																//front è la posizione del primo elemento della coda,back è la posizione successiva rispetto all'ultimo elemento della coda, ossia la prima posizione libera
typedef struct _QUEUE QUEUE;

int increment(int num,int size){								//funzione necessaria per l'implementazione di un array circolare
	return (num+1)%size;										//tramite questa operazione quando num+1 è size la funzione ritorna 0,quindi si può immaginare che la posizione successiva a size-1 dell'array sia 0, di fatto rendendolo circolare
}

BOOLEAN is_empty(QUEUE* pQ){									//ritorna un boolean indicante se la coda è vuota
	return (pQ->front == pQ->back);								//se la posizione del primo elemento della coda è la prima posizione libera (back) vuol dire che la prima posizione è libera, la coda è vuota
}

BOOLEAN is_full(QUEUE* pQ){										//ritorna un boolean indicante se la coda è piena
	return increment(pQ->back,pQ->size) == pQ->front;			//se la posizione successiva rispetto a back è la posizione della prima persona nella coda, allora la coda circolare è piena
}

int length(QUEUE* pQ){											//ritorna un intero indicante quanti elementi ci sono nella coda
	int back = pQ->back;										//vogliamo operare con back ma senza modificarlo quindi facciamo una copia
	if (back < pQ->front) back += pQ->size;						//se back (essendo circolare l'array) è in una posizione che ha indice minore rispetto alla fronte della coda, ci aggiungiamo la grandezza dell'array,sennò rimane uguale
	return back - pQ->front;									//a questo punto in entrambi i casi questa espressione vale la quantità di elementi nella coda
}


BOOLEAN deque(QUEUE* pQ,int* element){							//rimuove il primo elementa della coda
	if(!is_empty(pQ)){											//se non è vuota la coda..
		*element = pQ->elements[pQ->front];						//..copia l'elemento in fronte alla coda nella variabile intera passata per riferimento
		pQ->front = increment(pQ->front,pQ->size);				//si sposta il fronte della coda al posto successivo, si DEVE usare la funzione increment e non gli operatori ++ perchè l'array è circolare quindi è..
		return TRUE;											//..possibile che l'incremento riporti la posizionea a 0
	} else return FALSE;
}

void grow_queue(QUEUE* pQ){										//come al solito quando si implementa una struttura tramite array è necessaria una funzione per accrescerlo quando è pieno, o crearlo se non è stato inizializzato
	if(pQ->elements == NULL){									//se non è stato inizializzato, lo inizializza
		pQ->elements = (int*)malloc(sizeof(int)*pQ->size);
		pQ->front = 0;
		pQ->back = 0;											//unica differenza è che ci sono due riferimenti, entrambi da porre inizialmente a 0 (la coda è vuota)
	} else if (is_full(pQ)){									//se l'array è pieno è necessario aumentarne la grandezza, oltre a questo ne approfittiamo per riordinare il tutto,mettendo il primo elemento in posizione 0 e gli altri a seguire
		int new_back = 0;										//mi serve un intero per tenere conto di quanti elementi vado a copiare nel nuovo array
		int* tmp = (int*)malloc(sizeof(int)*2*pQ->size);		//inizializzo il nuovo array, di dimensione doppia
		while(deque(pQ,&(tmp[new_back]))) new_back++;			//svuoto tramite la funzione deque il vecchio array in quello nuovo, a ogni elemento che passo aumento new_back di uno
		free(pQ->elements);										//libero la memoria del vecchio array
		pQ->elements = tmp;										//si assegna il puntatore dell'array della coda al nuovo puntatore relativo al nuovo array
		pQ->front = 0;											//come detto il fronte della coda è 0, abbiamo riordinato
		pQ->back = new_back;									//back della coda diventa new_back
		pQ->size *= 2;											//e infine bisogna ricordarsi di raddoppiare size avendo raddoppiato la grandezza dell'array
	}
}

void enqueue(QUEUE* pQ,int element){							//funzione per mettere in coda un nuovo elemento
	assert(pQ);													
	grow_queue(pQ);												//come sempre quando si inserisce un nuovo elemento in un array si controlla se è vuoto/pieno/da aumentare di grandezza
	pQ->elements[pQ->back] = element;							//l'elemento viene aggiunto in fondo alla coda
	pQ->back = increment(pQ->back,pQ->size);					//anche in questo caso il fondo della coda viene spostato di una posizione, si DEVE usare increment e NON ++, per lo stesso motivo spiegato in deque
}


BOOLEAN front(QUEUE* pQ,int* element){							//funzione per vedere chi è primo nella coda, senza rimuoverlo
	if(!is_empty(pQ)){											
		*element = pQ->elements[pQ->front];						//viene solo copiato l'elemento, senza cambiamenti alla coda
		return TRUE;
	} else return FALSE;
}


void print_queue(QUEUE* pQ){									//funzione per stampare a video gli elementi della coda, dal primo all'ultimo (FIFO)
	int i;														
	for(i=pQ->front;i!=pQ->back;i=increment(i,pQ->size)){		//si parte dal primo elemento della coda(in posizione front) e si arriva all'ultimo (in posizione back-1), l'incremento viene fatto CON INCREMENT, non con ++, essendo l'array circolare
		printf("%d ",pQ->elements[i]);
	}
	printf("\n");
} 
*/





//QUEUE implementata come linked list a due riferimenti
/*
struct CELL{													// usuale definizione di singola cella per una lista concatenata
	int element;												// come sempre, l'elemento in questo caso è un intero, ma potrebbe essere qualsiasi struttura
	struct CELL* next;
};

struct _QUEUE{													// la struttura coda non è altro che una struttura in cui vi sono due puntatori a celle, la prima cella della coda e l'ultima
	struct CELL* front;											// nella cella front c'è il primo elemento della coda
	struct CELL* rear;											// nella cella rear c'è l'ultimo elemento della coda
};																// avendo due riferimenti potremo avere accesso immediato sia per la funzione deque che per la funzione enqueue, aventi entrambe quindi complessità O(1)
typedef struct _QUEUE QUEUE;

BOOLEAN is_empty(QUEUE* pQ){									// funzione che ritorna se la coda è vuota
	return pQ->front == NULL;									// se la coda è vuota allora la cella front è NULL
}

BOOLEAN is_full(QUEUE* pQ){										// funzione che ritorna se la coda è piena (poco utile)
	return FALSE;												// una lista concatenata non è mai piena
}

void enqueue(QUEUE* pQ,int element){									//funzione che aggiunge una cella in fondo alla coda, contenente l'elemento desiderato
	struct CELL* new_cell = (struct CELL*)malloc(sizeof(struct CELL));	//creazione nuova cella
	new_cell->element = element;										//si pone come elemento della cella l'elemento desiderato (passato alla funzione)
	new_cell->next = NULL;												//essendo in fondo alla coda non avrà una cella successiva quindi si pone next a NULL
	if(is_empty(pQ)){													//se la coda è vuota..
		pQ->front = new_cell;											//...allora la cella appena creata sarà la prima della coda(quindi front)
	} else{																//se invece non è vuota..
		pQ->rear->next = new_cell;										//..allora la cella appena creata è la successiva rispetto all'attuale ultima cella della coda
	}
	pQ->rear = new_cell;												//in entrambi i casi alla fine la cella è la nuova fine della coda
}

BOOLEAN deque(QUEUE* pQ,int* element){							// funzione che rimuove una cella dalla prima posizione della coda (come ci si aspetta da una struttura FIFO)
	if(!is_empty(pQ)){											
		*element = pQ->front->element;							// il primo elemento viene copiato nella variabile intera passata per riferimento 
		pQ->front = pQ->front->next;							// dopodiché la fronte della coda diventa la cella che era successiva rispetto alla fonte
		return TRUE;
	} else return FALSE;										// se la coda era vuota allora la funzione ritorna falso
}

BOOLEAN front(QUEUE* pQ,int* element){							// si va a vedere il primo elemento della coda, senza rimuoverlo
	if(!is_empty(pQ)){
		*element = pQ->front->element;							// come deque() ma non si rimuove nulla, la coda rimane uguale
		return TRUE;
	} else return FALSE;
}

void print_queue(QUEUE* pQ){									// funzione che stampa a video gli elementi della coda, in ordine FIFO
	struct CELL* it = pQ->front;								// creo un iteratore, partendo dal primo elemento della coda(front)
	while(it){													//fintanto che l'iteratore non è NULL..
		printf("%d ",it->element);								//per ogni cella stampo l'elemento
		it = it->next;											//passando alla cella successiva
	}															//quando l'iteratore arriva a rear, stampa il suo elemento e viene poi assegnato a NULL, uscendo dal ciclo
	printf("\n");
} */





// IMPLEMENTAZIONE DI CONTAINER E SET COME ARRAY
/*
struct _CONTAINER{
	int* elements;
	int head,size;													//size è la grnadezza dell'array,head la prima posizione libera dell'array
};
typedef struct _CONTAINER CONTAINER;


CONTAINER* container_new(int size){									//funzione che crea un nuovo container e ne restituisce il puntatore
	CONTAINER* res = (CONTAINER*)malloc(sizeof(CONTAINER));			
	*res = (CONTAINER){ (int*)malloc(sizeof(int)*size),0,size};
	return res;
}

void free_container(CONTAINER* pCont){								//funzione che libera la memoria relativa a un container
	free(pCont->elements);											//libera prima l'array (il puntatore)
	free(pCont);													//poi il puntatore alla struttura stessa
}

void grow_container(CONTAINER* pCont){								//come sempre per l'implementazione con array è necessario avere una funzione per l'ampliamento di questo quando si riempie
	if(pCont->head < pCont->size) return;							//se non è ancora pieno ritorna senza far nulla
	else{															//se è pieno..
		int* tmp = (int*)malloc(sizeof(int)*2*pCont->size);
		memcpy(tmp,pCont->elements,sizeof(int)*pCont->size);		//come al solito, nuovo array,copia il vecchio nel nuovo,libera il vecchio,assegna a quello nuovo,raddoppia la grandezza
		free(pCont->elements);
		pCont->elements = tmp;
		pCont->size *= 2;
	}
}

void container_add(CONTAINER* pCont,int element){					//aggiunge un elemento all'array
	assert(pCont);
	grow_container(pCont);											//niente di nuovo, controlla che il puntatore sia valido, cresce l'array se necessario,inserisce l'elemento nella prima posizione libera(head) e aumenta head di uno (++)
	pCont->elements[pCont->head++] = element;						
}

BOOLEAN container_contains(CONTAINER* pCont,int element){			// controlla se un elemento è contenuto nel container
	int i;													
	for(i=0;i<pCont->head;i++){										// scorre lungo tutti gli elementi dell'array
		if(pCont->elements[i] == element)							// se uno di questi è l'elemento cercato..
			return TRUE;											// ..ritorna vero
	}
	return FALSE;													// altrimenti ritorna falso
}

int container_to_array(CONTAINER* pCont,int** dst){					// funzione per ottenere un array contenente tutti gli elementi del container
	*dst = (int*)malloc(sizeof(int)*pCont->head);					// alloca la memoria necessaria per la costruzione dell'array
	memcpy(*dst,pCont->elements,sizeof(int)*pCont->head);			// copia l'array del container nell'array creato
	return pCont->head;												// ritorna la lunghezza dell'array
}

void container_print(CONTAINER* pCont){								// stampa a video gli elementi contenuti nel container
	int i;
	for(i=0;i<pCont->head;i++)
		printf("%d ",pCont->elements[i]);							// solita iterazione lungo tutto l'array,stampandone gli elementi
	printf("\n");
}

struct _SET{														// in C, non essendo implementata l'ereditarierà fra strutture(classi), si usa questo trucco per implementare strutture che sono dei sottotipi di altre.. 
	CONTAINER parent;												//..usando le funzioni della struttura "parente" tramite casting (vedere lezione)
};
typedef struct _SET SET;

SET* set_new(int size){												//in questa funzione e le successive si usa ciò descritto sopra, si usano le funzione della struttura parente tramite casting,senza doverle ridefinire
	return (SET*)container_new(size);
}

void set_free(SET* pSet){
	free_container((CONTAINER*)pSet);
}

void set_grow(SET* pSet){
	grow_container((CONTAINER*)pSet);
}

BOOLEAN set_contains(SET* pSet,int element){
	return container_contains((CONTAINER*)pSet,element);
}

void set_add(SET* pSet,int element){
	if(!set_contains(pSet,element)){								// l'unica differenza è che l'insiemee/set non ammettendo duplicati, prima di aggiungere un elemento, controlla se già c'è, in tal caso, non lo riaggiunge
		container_add((CONTAINER*)pSet,element);
	}
}

int set_to_array(SET* pSet,int** dst){								// funzione per ottenere un array contenente tutti gli elementi del set
	return container_to_array((CONTAINER*)pSet,dst);
}

void set_print(SET* pSet){										
	container_print((CONTAINER*)pSet);							
} 

SET* set_union(SET* pSet1, SET* pSet2){								// unione di due insiemi, esattamente uguale all'idea matematica di unione fra insiemi
	SET* res = set_new(pSet1->parent.size);							// creo l'insieme unione
	int i;
	for(i=0;i<pSet1->parent.head;i++)								// scorro lungo tutti gli elementi del primo set, aggiungendoli all'insieme creato
		set_add(res,pSet1->parent.elements[i]);
	for(i=0;i<pSet2->parent.head;i++)								// lo stesso per il secondo insieme, ovviamente non vengono aggiunti duplicati dato in add() si controlla se l'elemento è già presente prima di aggiungerlo al set
		set_add(res,pSet2->parent.elements[i]);
	return res;														// ritorno il puntatore all'insieme unione
}

SET* set_intersect(SET* pSet1,SET* pSet2){							// intersezione di due insiemi, anche questo affine all'idea matematica di intersezione
	SET* res = set_new(pSet1->parent.size);							// creo l'insieme intersezione
	int i;															
	for(i=0;i<pSet1->parent.head;i++){								// scorro lungo gli elementi del primo insieme...
		if(set_contains(pSet2,pSet1->parent.elements[i]))			// ...controllando che siano presenti anche nel secondo insieme
			set_add(res,pSet1->parent.elements[i]);					// .. se così è, aggiungo l'elemento all'insieme intersezione
	}
	return res;														// ritorno il puntatore all'insieme intersezione
}

SET* set_subtract(SET* pSet1,SET* pSet2){							// sottrazione fra insiemi
	SET* res = set_new(pSet1->parent.size);							// creo l'insieme sottrazione
	int i;
	for(i=0;i<pSet1->parent.head;i++){								// scorro lungo tutti gli elementi del primo insieme..
		if(!set_contains(pSet2,pSet1->parent.elements[i]))			// ..se l'elemento NON (!) appartiene anche al secondo insieme.. 
			set_add(res,pSet1->parent.elements[i]);					// allora lo aggiungo all'insieme sottrazione
	}
	return res;														// ritorno il puntatore all'insieme sottrazione
}
*/


// IMPLEMENTAZIONE DI CONTAINER E SET COME LINKED LIST
/*
struct CELL{														// solita struttura cella per liste concatenate
	int element;
	struct CELL* next;
};

struct CONTAINER{													// la lista è a doppio riferimento
	struct CELL* front;
	struct CELL* rear;
	int size;
};																	// non usando typedef ecc ecc sarà necessario scrivere ogni volta struct CONTAINER a differenza di quanto fatto prima per array, si poteva benissimo fare typedef ed evitarlo

struct CONTAINER* container_new(){													// funzione per creare un nuovo contenitore
	struct CONTAINER* res = (struct CONTAINER*)malloc(sizeof(struct CONTAINER));	// alloca la memoria necessaria 
	*res = (struct CONTAINER){NULL,NULL,0};											// come al solito, quando appena creata una struttura, si pongono a NULL i riferimenti essendo per ora vuota, size a 0 per lo stesso motivo
	return res;																		// ritorna il puntatore alla struttura appena creata
}

void container_free(struct CONTAINER* pCnt){						// funzione usata per liberare la memoria di una struttura che non sarà più usata
	struct CELL* it = pCnt->front;									// costruisco un iteratore partendo dall'inizio della lista
	while(it){														// fintanto che it non è NULL..
		struct CELL* tmp = it;										// creo un puntatore temporaneo alla cella che devo liberare perché devo ancora lavorare con it
		it = it->next;												// pongo l'iteratore alla prossima cella (giunti all'ultima viene posto a NULL)		
		free(tmp);													// libero la memoria associata alla cella appena superata
	}
	free(pCnt);														// alla fine,liberata la memoria di tutte le singole celle, libero la memoria del puntatore alla struttura
}

void container_add(struct CONTAINER* pCnt,int element){						// funzione per aggiungere un elemento al contenitore
	struct CELL* new_cell = (struct CELL*)malloc(sizeof(struct CELL));		// creazione nuova cella
	new_cell->element = element;											// assegnazione dell'elemento desiderato come elemento della nuova cella
	new_cell->next = NULL;													// dato che metterò questa cella in fondo alla fila, la cella successiva a quella nuova non esiste ancora, si assegna quindi next a NULL
	if(pCnt->front == NULL)													// se il contenitore è vuoto, ossia la prima cella del contenitore è NULL...
		pCnt->front = new_cell;												// allora la cella appena creata sarà la prima cella del contenitore
	else																	// se invece c'è già qualche cella..
		pCnt->rear->next = new_cell;										// ..vuol dire che la cella nuova sarà la seguente di quella che fino ad ora era l'ultima
	pCnt->rear = new_cell;													// in entrambi i casi appena descritti la nuova cella diventa la nuova ultima (in posizione rear)
	pCnt->size++;															// avendo aggiunto una cella aumento l'intero che tiene conto della dimensione del contenitore di uno
}

BOOLEAN container_contains(struct CONTAINER* pCnt,int element){		// funzione per determinare se un elemento è contenuto nel contenitore
	struct CELL* it = pCnt->front;									// creo un iteratore
	while(it){														// come al solito itero lungo la lista
		if(it->element == element)
			return TRUE;											// se l'elemento è effettivamente l'elemento di una delle celle lungo cui sto iterando ritorno vero
		it = it->next;
	}
	return FALSE;													// se sono arrivato fino in fondo alla lista senza trovare l'elemento allora questo non fa parte del contenitore, ritorno falso
}

int container_to_array(struct CONTAINER* pCnt,int** dst){			// funzione per ottenere un array contenente tutti gli elementi del contenitore
	*dst = (int*)malloc(sizeof(int)*pCnt->size);					// alloco la memoria necessaria per contenere tutti gli elementi (che sono una quantità uguale alla grandezza (size) del contenitore)
	struct CELL* it = pCnt->front;									// di nuovo si costruisce un iteratore tramite modi già visti
	int i = 0;														// si inizializza un intero che tiene conto di quante celle abbiamo attraversato, potendolo quindi usare con indice dell'array quando si aggiunge
	while(it){
		(*dst)[i++] = it->element;									// si aggiunge l'elemento di ogni cella all'array, per ogni aggiunta i viene aggiornato al valore successivo(++)
		it = it->next;
	}
	return pCnt->size;												// ritorno la lunghezza dell'array
}

void container_print(struct CONTAINER* pCnt){						// stampa gli elementi del contenitore
	struct CELL* it = pCnt->front;
	while(it){
		printf("%d ",it->element);									// nulla di nuovo, itero lungo la lista e stampo ogni elemento delle celle
		it = it->next;
	}
	printf("\n");
}

struct _SET{														// esattamente uguale a quanto scritto per l'implmentazione con array
	struct CONTAINER parent;
};
typedef struct _SET SET;

SET* set_new(){
	return (SET*)container_new();
}

SET* set_free(SET* pSet){
	container_free((struct CONTAINER*)pSet);
}

BOOLEAN set_contains(SET* pSet,int element){
	return container_contains((struct CONTAINER*)pSet,element);
}

void set_add(SET* pSet,int element){
	if(!set_contains(pSet,element))									// anche in sto caso prima di aggiungere un elemento al set si controlla se già c'è, in tal caso non lo riaggiunge
		container_add((struct CONTAINER*)pSet,element);
}

int set_to_array(SET* pSet, int** dst){
	return container_to_array((struct CONTAINER*)pSet,dst);
}

void set_print(SET* pSet){
	container_print((struct CONTAINER*)pSet);
}

SET* set_union(SET* pSet1,SET* pSet2){								// unione fra insiemi
	SET* res = set_new();											// creazione insieme unione
	struct CELL* it = pSet1->parent.front;							// creo iteratore per scorrere lungo il primo insieme partendo dalla sua prima cella(front)
	while(it){
		set_add(res,it->element);									//aggiungo ogni elemento del primo set al set unione
		it = it->next;
	}
	it = pSet2->parent.front;										// l'iteratore è ora usato per scorrere la seconda lista (il secondo set)
	while(it){
		set_add(res,it->element);									// aggiungo ogni elemento del secondo set al set unione, ovviamente i doppioni vengono scartati per come è implementata set_add()
		it = it->next;
	}
	return res;														// ritorno il puntatore al set unione
}

SET* set_intersect(SET* pSet1,SET* pSet2){							// intersezione fra insiemi
	SET* res = set_new();											// creazione insieme intersezione
	struct CELL* it = pSet1->parent.front;							// iteratore usato per scorrere lungo il primo set
	while(it){
		if (set_contains(pSet2,it->element))						// se anche il secondo set contiene l'elemento della cella attuale..
			set_add(res,it->element);								// ..allora aggiungo l'elemento al set intersezione
		it = it->next;
	}
	return res;														// ritorno il puntatore al set intersezione
}

SET* set_subtract(SET* pSet1,SET* pSet2){							// sottrazione fra insiemi
	SET* res = set_new();											// creazione set sottrazione
	struct CELL* it = pSet1->parent.front;							// solito iteratore, scorro lungo il primo set
	while(it){
		if (!set_contains(pSet2,it->element))						// se l'elemento della cella NON (!) è contenuto nel secondo set..
			set_add(res,it->element);								// ..allora lo aggiungo al set sottrazione
		it = it->next;
	}
	return res;														// ritorno il puntatore al set sottrazione
} 
*/
// N.B: si può rendere l'insieme/set agnostico rispetto a come è implementata la struttura container
// qui non l'ho fatto come a lezione, per approfondimento vedere lezione opportuna
// semplicemente le funzioni e le strutture hanno nomi uguali, quindi a seconda di cosa si commenta e di cosa si tiene scommentato, si usa quell'implementazione
// nel main basta scommentare/commentare una di due righe per usare l'implementazione desiderata




// IMPLEMENTAZIONE MAPPA TRAMITE ARRAY
/*
typedef struct {
	int key;														// relazione binaria, a una chiave è associata un'unica stringa di lunghezza 20
	char value[20];
}BinaryRel;

void print_BinaryRel(BinaryRel* rel){								// funzione per stampare a video una relazione binaria (una coppia chiave-valore)
	printf("( %d %s ) ",rel->key,rel->value);
}

typedef struct{
	BinaryRel* data;												// array di relazioni binarie
	int size,len;													// grandezza dell'array (size) e primo posto libero all'interno dell'array(len)
}Map;

Map* map_new(int size){													// funzione per creare una mappa
	Map* ret = (Map*)malloc(sizeof(Map));								// allocazione della memoria necessaria
	*ret = (Map){(BinaryRel*)malloc(sizeof(BinaryRel)*size),size,0};	// si inizializza la mappa, allocando la memoria necessaria per l'array di relazioni binarie, scegliendo la grandezza iniziale (size) e ponendo len a 0 essendo vuota per ora
	return ret;															// ritorna il puntatore alla mappa creata
}

void map_grow(Map* pMap){													// essendo un implementazione di una struttura tramite array, serve come sempre la funzione per accrescere dinamicamente l'array quando necessario
	if(pMap->len < pMap->size) return;										// se l'array non è pieno ritorna senza far nulla
	BinaryRel* tmp = (BinaryRel*)malloc(sizeof(BinaryRel)*2*pMap->size);	// altrimenti la funzione continua, crea un nuovo array di dimensione doppia(2*size)
	memcpy(tmp,pMap->data,sizeof(BinaryRel)*pMap->size);					// copia il contenuto del vecchio array nel nuovo
	free(pMap->data);														// libera la memoria relativa al vecchio array
	pMap->data = tmp;														// l'array della mappa è quello nuovo
	pMap->size *= 2;														// avendo raddoppiato la grandezza dell'array, size viene raddoppiato
}

BOOLEAN map_contains_key(Map* pMap,int k){							// funzione per controllare se una mappa contiene una chiave specifica
	int i;															
	for(i=0;i<pMap->len;i++){										// si opera una ricerca lineare all'interno dell'array..
		if(pMap->data[i].key == k)									// ..se la chiave di una delle relazioni binarie dell'array è uguale alla chiave che stiamo cercando..
			return TRUE;											//..ritorna vero
	} return FALSE;													// se invece ho iterato lungo tutto l'array senza trovare la stessa chiave, ritorno falso, la mappa non contiene la chiave cercata
}

void map_add(Map* pMap,BinaryRel* rel){								// aggiunge una relazione binaria all'array, a lezione si usa passare per valore direttamente la relazione binaria, ma seguendo la parola di Giunta, le strutture le passo per riferimento
	assert(pMap);													// controlla che il puntatore sia valido
	if(!map_contains_key(pMap,rel->key)){							// dato che in una mappa una chiave può appararire solo una volta, controllo che non ci sia già una relazione binaria con quella chiave
		map_grow(pMap);												// se non c'è, allora posso inserire la relazione binaria, invoco la funzione di accrescimento dell'array dato che potrebbe essere già pieno
		pMap->data[pMap->len++] = *rel;								// aggiungo la relazione (*rel) nella prima posizione libera dell'array(len) e aumento len di uno (++)
	}
}

char* map_lookup(Map* pMap,int k){									// funzione che ritorna il valore associato a una chiave, o NULL se non trova la chiave
	int i;
	for(i=0;i<pMap->len;i++){										// ancora una volta ricerca lineare nell'array
		if(pMap->data[i].key == k)									// ..se trova la chiave
			return pMap->data[i].value;								// ..ne restituisce il valore associato
	}
	return NULL;													// altrimenti ritorna NULL
}

void map_print(Map* pMap){											// funzione che stampa a video tutte le relazioni binarie della mappa
	int i;
	for(i=0;i<pMap->len;i++){										// iterazione lungo tutto l'array della mappa
		print_BinaryRel(&(pMap->data[i]));							// stampa ad una ad una tutte le relazioni binarie sfruttando la funzione print_BinaryRel implementata prima
	}
	printf("\n");
} 
*/





// IMPLEMENTAZIONE MAPPA COME LINKED LIST
/*
typedef struct {											// stessa struttura della relazione binaria, definita anche per l'implementazione con array. si usa anche la stessa funzione per la stampa a video
	int key;
	char value[20];
}BinaryRel;

void print_BinaryRel(BinaryRel* rel){
	printf("( %d %s ) ",rel->key,rel->value);
}

struct _Cell{												// cella della lista concatenata, in questo caso a differenza degli altri visti il dato non è un int ma una relazione binaria
	BinaryRel data;
	struct _Cell* next;
};
typedef struct _Cell Cell;

typedef struct{												// lista concatenata a due riferimenti, si tiene traccia sia della prima cella (front) che dell'ultima (rear) 
	Cell* front;
	Cell* rear;
	int size;												// si tiene conto anche della quantità di celle inizializzate
}Map;

Map* map_new(){												// funzione per creare una nuova mappa
	Map* ret = (Map*)malloc(sizeof(Map));					// allocazione della memoria necessaria
	*ret = (Map){NULL,NULL,0};								// inizializza la mappa, non avendo per ora celle sia front che rear sono NULL e size è 0
	return ret;												// ritorna il puntatore alla mappa
}

BOOLEAN map_contains_key(Map* pMap,int k){					// funzione per controllare se una chiave è già presente all'interno della mappa
	Cell* it = pMap->front;									// creo un iteratore partendo dalla prima cella della mappa
	while(it){
		if(it->data.key == k)								// se la chiave della relazione binaria di una delle celle è la chiave cercata..
			return TRUE;									// ..ritorna vero
		it = it->next;
	}
	return FALSE;											// sennò ritorna falso
}

void map_add(Map* pMap,BinaryRel* rel){						// funzione per aggiungere una relazione binaria alla mappa
	assert(pMap);
	if(!map_contains_key(pMap,rel->key)){					// se la mappa non contiene già una relazione binaria avente come chiave la chiave della relazione binaria che vogliamo aggiungere..
		Cell* new_cell = (Cell*)malloc(sizeof(Cell));		//..allora crea una nuova cella
		new_cell->data = *rel;								//..il cui elemento è la relazione binaria
		new_cell->next = NULL;								//..e la prossima cella è NULL, dovendo ponere la nuova cella in fondo alla lista concatenata
		if (pMap->front == NULL){							// se la lista era vuota..
			pMap->front = new_cell;							// ..la nuova cella diventa anche la prima cella della lista
		} else{												// se invece la lista non era vuota..
			pMap->rear->next = new_cell;					// ..allora la nuova cella è la cella successiva rispetto a quella che per ora era segnata come ultima
		}
		pMap->rear = new_cell;								// in entrambi i casi la nuova cella diventa la nuova cella terminale della lista
		pMap->size++;										// e avendo aggiunto una cella la grandezza della mappa viene aumentata di uno
	}
}

char* map_lookup(Map* pMap,int k){							// funzione che ritorna il valore associato a una chiave, se la mappa contiene una relazione binaria in cui c'è la chiave richiesta
	assert(pMap);
	Cell* it = pMap->front;									// come sempre, si costruisce un iteratore
	while(it){
		if(it->data.key == k)								// che controlla lungo tutta la lista concatenata se la relazione binaria di una delle celle ha come chiave quella cercata
			return it->data.value;							// se così è, ritorna il valore associato a quella chiave
		it = it->next;
	}
	return NULL;											// altrimenti ritorna NULL
}

void map_print(Map* pMap){									// stampa le relazioni binarie della mappa
	assert(pMap);
	Cell* it = pMap->front;									// iteratore
	while(it){
		print_BinaryRel(&(it->data));						// stampa singolarmente le relazioni binarie di ogni cella, partendo dalla prima all'ultima
		it = it->next;	
	}
	printf("\n");
}
*/





// MULTIMAPPA IMPLEMENTATA COME ARRAY  
/*
 
typedef char T[20];									// questo typedef è utile per usare come sinonimo di "tipo array di char con lunghezza 20" semplicemente la lettera T

typedef struct {									//(BinaryRel esprime una relazione binaria fra una chiave e un array di valori, a differenza della mappa in cui ad ogni chiave è associato un unico valore)
	int key;										// probabilmente andrebbe chiamata diversamente da binary la relazione, ma non ho voglia di correggere il codice. anche se tecnicamente se si pensa a binario nel senso sopra dovrebbe essere ok
	T* values;										// questo T* values è un array di stringhe di 20 caratteri
	int size,len;									// l'array dei valori è a sua volta allocato dinamicamente, quindi mi serve una grandezza size e un "indice" di posizione (len) che indica il primo posto disponibile nell'array
}BinaryRel;											// l'implementazione fatta a lezione è diversa nel senso che viene definita una struttura ValArray in cui non è direttamente associata la chiave, funzionano entrambi i modi


// ATTENZIONE LE PROSSIME FUNZIONI SONO PER GESTIRE LA STRUTTURA BINARYREL, ossia gli elementi chiave-valori, che poi saranno elementi della multimappa, le prossime funzioni non riguardano direttamente la multimappa

BinaryRel* BinaryRel_new(int key,int size){							// crea una nuova struttura BinaryRel
	BinaryRel* ret = (BinaryRel*)malloc(sizeof(BinaryRel));			// allocazione della memoria necessaria
	*ret = (BinaryRel){key,(T*)malloc(sizeof(T)*size),size,0};		// si alloca anche la memoria necessaria per l'array di valori (di stringhe di lunghezza 20)
	return ret;														// ritorna il puntatore alla struttura creata
}

void BinaryRel_grow(BinaryRel* pBR){						// come sempre quando c'è un array allocato dinamicamente serve avere una funzione che ne raddoppi la capienza quando necessario
	if(pBR->len < pBR->size) return;
	T* tmp = (T*)malloc(sizeof(T)*2*pBR->size);
	memcpy(tmp,pBR->values,sizeof(T)*pBR->size);
	free(pBR->values);
	pBR->values = tmp;
	pBR->size *= 2;
}

void BinaryRel_free(BinaryRel* pBR){					// libera la memoria associata a una struttura BinaryRel
	assert(pBR);
	free(pBR->values);									// libera prima la memoria associata all'array di valori
	free(pBR);											// poi quella del puntatore alla struttura stessa
}

BOOLEAN BinaryRel_contains(BinaryRel* pBR,T value){		// funzione che controlla se un valore è già all'interno dell'array di valori. Questo non penso sia necessario in una multimappa ma ad ogni chiave, anche se possono essere associati..
	int i;												// ..molteplici valori, in questa implementazione si vuole (come quella a lezione) che questi valori siano unici, per esempio non posso associare alla chiave 1 due volte la parola "elefante"
	for(i=0;i<pBR->len;i++){
		if(strcmp(pBR->values[i],value) == 0)			// itera lungo tutto l'array dei valori, se trova il valore cercato..
			return TRUE;								// ..ritorna vero
	}
	return FALSE;										// sennò falso
}

void BinaryRel_add(BinaryRel* pBR,T value){				// aggiunge un valore nell'array dei valori
	assert(pBR);
	if(!BinaryRel_contains(pBR,value)){					// ma come detto sopra lo fa solo se quel valore non è già compreso nell'array (non vogliamo doppioni di valori)
		BinaryRel_grow(pBR);							
		strcpy(pBR->values[pBR->len++],value);			//viene copiato il valore (value) al termine dell'array di valori, e viene ovviamente aumentato len di uno(++), avendo aggiuto un valore
	}
}

void BinaryRel_print(BinaryRel* pBR){					// stampa una struttura chiave - valori
	assert(pBR);
	printf("[key: %d values: ( ",pBR->key);				// inizialmente stampo la chiave
	int i;
	for(i=0;i<pBR->len;i++){
		printf(" %s ",pBR->values[i]);					// poi ad uno ad uno stampo i valori in relazione alla chiave
	}
	printf(")]\n");
}

// QUI TERMINANO LE FUNZIONI DI GESTIONE DELLA STRUTTURA BINARYREL E COMINCIANO QUELLE PROPRIE DELLA MULTIMAPPA 

typedef struct{
	BinaryRel* data;								// array di "relazioni binarie" chiave-valori (plurale, come ci si aspetta da una multimappa)
	int size,len;									// come sempre quando si implementa una struttura tramite array dinamico servono size e len;
}mMap;


mMap* mmap_new(int size){												// funzione per la creazione di una nuova multimappa
	mMap* ret = (mMap*)malloc(sizeof(mMap));							// allocazione della memoria necessaria per la multimappa
	*ret = (mMap){(BinaryRel*)malloc(sizeof(BinaryRel)*size),size,0};	// assegnaezione della multimappa, in particolare si assegna la memoria necessaria all'array di relazioni binarie
	return ret;															// ritorna il puntatore alla multimappa
}

void mmap_free(mMap* pMap){												// libera la memoria associata a una multimappa
	int i;
	for(i=0;i<pMap->len;i++)											// libera singolarmente gli array di valori per ogni i-esima chiave
		free(pMap->data[i].values);
	free(pMap->data);													// libera la memoria associata all'array di relazioni binarie
	free(pMap);															// e infine quella del puntatore alla multimappa
}

void mmap_grow(mMap* pMap){												// come sempre, implementazione tramite array dinamico richiede una funzione di crescita dell'array
	if(pMap->len < pMap->size) return;
	BinaryRel* tmp = (BinaryRel*)malloc(sizeof(BinaryRel)*2*pMap->size);
	memcpy(tmp,pMap->data,sizeof(BinaryRel)*pMap->size);				// questi passaggi sono analoghi ai soliti per le altre strutture, allocazione,copia,liberazione,assegnazione,duplicazione grandezza
	free(pMap->data);
	pMap->data = tmp;
	pMap->size *= 2;
}

int mmap_contains_key(mMap* pMap,int k){								// controlla se una chiave fa già parte di una delle relazioni binarie della multimappa
	int i;
	for(i=0;i<pMap->len;i++){
		if(pMap->data[i].key == k)										// a differenza degli altri casi se effettivamente la chiave viene trovata.. 
			return i;													// ..viene restituito l'indice della relzione binaria relativa, all'intero dell'array della multimappa
	} return -1;														// se invece non viene trovata la chiave, la funzione ritorna -1 (ossia un valore impossibile per un indice di un array)				
}

void mmap_add(mMap* pMap,BinaryRel* rel){								// funzione per aggiungere una relazione binaria nella multimappa (o solo nuovi valori associati a una chiave nel caso che la chiave sia già presente)
	assert(pMap);
	int idx = mmap_contains_key(pMap,rel->key);							// controllo se la chiave della relazione binaria che sto cercando di aggiungere già esiste nella multimappa
	if(idx==-1){														// se non esiste, contains_key() ritorna -1, quindi..
		mmap_grow(pMap);												//..dopo aver chiamato mmap_grow per assicurarmi di avere spazio nell'array..
		pMap->data[pMap->len++] = *rel;									//..semplicemente copio la relazione binaria come nuovo elemento dell'array di relazioni binarie e aumento len di uno
	} else{																// se invece la chiave era già presente, allora..
		int j;
		for(j=0;j<rel->len;j++)											//..iterando lungo l'array di valori..
			BinaryRel_add(&(pMap->data[idx]),rel->values[j]);			//..vado ad aggiungere ogni valore all'array di valori già precedentemente presente associato alla chiave desiderata
	}
}

int mmap_lookup(mMap* pMap,int k,T** dst){								// funzione che permette di ottenere un array di valori associati a una specifica chiave
	int i,j;
	for(i=0;i<pMap->len;i++){
		if(pMap->data[i].key == k){										// se effettivamente la chiave esiste all'interno della multimappa
			*dst = (T*)malloc(sizeof(T)*pMap->data[i].len);				// allora alloco memoria adeguata per l'array di valori
			for(j=0;j<pMap->data[i].len;j++)							
				strcpy((*dst)[j],pMap->data[i].values[j]);				// e copio ogni valore nell'array appena creato (essendo stringhe i valori devo usare strcpy e non =, dato che le stringhe in C sono array)
			return pMap->data[i].len;									// ritorno quindi la lunghezza dell'array appena creato
		}	
	}
	return -1;															// se invece non c'è la chiave cercata nella multimappa allora ritorno una grandezza assurda, ossia -1
}

void mmap_print(mMap* pMap){											// funzione usata per stampare le "relazioni binarie" (coppie chiave - valori) di una multimappa
	int i;
	for(i=0;i<pMap->len;i++){											// scorro lungo l'array delle relazioni binarie
		BinaryRel_print(&(pMap->data[i]));								// e le stampo una ad una tramite la funzione implementata precedentemente per la stampa di una struttura BinaryRel
	}
} 
*/





// IMPLEMENTAZIONE DI HASH TABLE PER LA RICERCA OTTIMALE IN UNA MAPPA DI LIBRI
/*
struct _Book{															// struttura libro vista a lezione
	int id;
	char title[100];
	char author[100];
	int year;
};
typedef struct _Book Book;

void print_book(Book* pBook){											// funzione per stamapre una linea che riassume le caratteristiche di un libro
	printf("[%d] \"%s\" written by %s (%d)\n",pBook->id,pBook->title,pBook->author,pBook->year);
}

int book_binary_search(Book arr[],int l,int r,char* k){					// funzione di ricerca binaria adattata per una struttura Book
	if (r>=l){
		int m = l + (r-l)/2;
		int cmp = strcmp(arr[m].title,k);
		if(cmp==0)	
			return m;
		else if(cmp<0)
			return book_binary_search(arr,m+1,r,k);
		else
			return book_binary_search(arr,l,m-1,k);
	}
	return -1;
}

struct _Map{															// implementazione di una mappa ordinata, simile all'implementazione della mappa tramite array sopra, commento solo le differenze
	Book* books;														// array di libri
	int len,size;
};
typedef struct _Map Map;

Map* map_new(int size){
	Map* ret = (Map*)malloc(sizeof(Map));
	*ret = (Map){(Book*)malloc(sizeof(Book)*size),0,size};
	return ret;
}

void map_free(Map* pMap){
	assert(pMap);
	free(pMap->books);
	free(pMap);
}

void map_grow(Map* pMap){
	if(pMap->len < pMap->size) return;
	Book* tmp = (Book*)malloc(sizeof(Book)*2*pMap->size);
	memcpy(tmp,pMap->books,sizeof(Book)*pMap->size);
	free(pMap->books);
	pMap->books = tmp;
	pMap->size *= 2;
}

BOOLEAN map_contains_key(Map* pMap,char* k){
	assert(pMap);
	int pos = book_binary_search(pMap->books,0,pMap->len-1,k);			// a differenza di sopra, dato che l'inserimento mantiene un ordine basato sul titolo dei libri, si usa la ricerca binaria per trovare un libro, non la lineare
	if(pos == -1)
		return FALSE;
	else
		return TRUE;
}

void map_add(Map* pMap,Book* pBook){
	assert(pMap);
	if (!map_contains_key(pMap,pBook->title)){							// se non è già presente un libro con lo stesso titolo nella mappa lo inserisce
		map_grow(pMap);
		int i = pMap->len-1;
		while(i>=0 && strcmp(pBook->title,pMap->books[i].title)<0){		// in particolare queste linee di codice servono a mantenere l'array ordinato rispetto al titolo dei libri (alfabetico)
			pMap->books[i+1] = pMap->books[i];
			i--;
		} 
		pMap->books[i+1] = *pBook;					
		pMap->len++;													// dopo aver aggiunto il libro si aumenta len di uno
	}
}

Book* map_lookup(Map* pMap,char* key){
	assert(pMap);
	int pos = book_binary_search(pMap->books,0,pMap->len-1,key);		// anche in sto caso si usa la ricerca binaria per trovare un libro tramite il titolo
	if(pos==-1)
		return NULL;													// se il libro non è stato trovato allora la funzione ritorna NULL
	else
		return &(pMap->books[pos]);										// se lo trova invece restituisce un puntatore al libro adeguato
}

void map_print(Map* pMap){
	int i;
	for(i=0;i<pMap->len;i++)
		print_book(&(pMap->books[i]));
}

int string_hash(char* my_string,int num){								// funzione di hash, data una stringa e un numero (il numero di buckets desiderato) restituisce un valore fra 0 e il numero di buckets -1
	int ret = 0;
	int i;
	for(i=0;i<strlen(my_string);i++)
		ret += my_string[i];											// sfrutta il fatto che ad ogni char è associato un valore intero (ASCII table)
	return ret % num;
}

struct _HashTable{
	Map** buckets;														// essenzialmente l'hash table è un array di puntatori a mappe 
	int num_buckets;													// num buckets determina quanti "scaffali" ho per i libri
};
typedef struct _HashTable HashTable;

HashTable* hashtable_new(int num_buckets,int size_buckets){						//funzione per la creazione di una nuova hash table
	HashTable* ret = (HashTable*)malloc(sizeof(HashTable));						// allocazione memoria necessaria per la hash table
	*ret = (HashTable){(Map**)malloc(sizeof(Map*)*num_buckets),num_buckets};	// inizializza la hash table, ossia crea l'array di puntatori alle mappe e stabilisce il numero di buckets
	int i;
	for(i=0;i<num_buckets;i++)													
		ret->buckets[i] = map_new(size_buckets);								// ogni mappa/bucket viene inizializzata tramite la funzione map_new(), l'array della mappa in particolare avrà size iniziale uguale a size_buckets, a scelta quando si chiama la funzione
	return ret;																	// ritorna il puntatore alla hash table creata		
}

void hashtable_free(HashTable* pHT){									// funzione per liberare la memoria associata ad una hash table
	int i;
	for(i=0;i<pHT->num_buckets;i++)	
		map_free(pHT->buckets[i]);										// prima libera la memoria associata agli array di libri di ogni bucket
	free(pHT->buckets);													
	free(pHT);															
}

BOOLEAN hashtable_contains(HashTable* pHT,char* key){					// funzione che ritorna un booleano che indica se una chiave (in sto caso una stringa, il titolo del libro) è già presente nella hash table
	assert(pHT);
	int h = string_hash(key,pHT->num_buckets);							// per trovare in quale buckets eventualmente è il libro bisogna vedere quale valore restituisce la funzione di hash a partire dal titolo del libro
	return map_contains_key(pHT->buckets[h],key);						// se nel bucket adeguato c'è un libro avente quel titolo resituisce vero, sennò falso
}

void hashtable_add(HashTable* pHT,Book* pBook){							// aggiunge un libro nella hash table
	assert(pHT);
	int h =  string_hash(pBook->title,pHT->num_buckets);				// usa la funzione di hash per individuare in quale bucket inserire il libro
	map_add(pHT->buckets[h],pBook);										// lo inserisce, ovviamente se già c'era un libro con quel titolo il libro non viene inserito, per come è costruita map_add()
}

Book* hashtable_lookup(HashTable* pHT,char* key){						// funzione che restituisce il libro appartenente alla hash table avente il titolo cercato 
	assert(pHT);
	int h = string_hash(key,pHT->num_buckets);							// la funzione di hash indica in quale bucket cercare, questo processo velocizza la ricerca
	return map_lookup(pHT->buckets[h],key);								// map_lookup() tramite ricerca binaria restituisce il libro cercato o NULL se non c'è il libro
}

void hashtable_print(HashTable* pHT){									// funzione che stampa a video il contenuto della hash table
	int i;
	for(i=0;i<pHT->num_buckets;i++){									
		// se si vuole visualizzare in quale buckets finiscono i vari libri (verificando l'ordine all'interno di questi) scommentare le prossime due linee di codice
		// if(pHT->buckets[i]->len!=0)
		//	printf("[%d] ",i);
		map_print(pHT->buckets[i]);										// per ogni bucket (che sono mappe) stampa i libri di quel bucket
	}
} 
*/



// IMPLEMENTAZIONE BINARY TREE
/*
struct _BinTreeNode{													// nodo di albero
	int data;															// l'elemento del nodo in questo caso è un intero
	struct _BinTreeNode* left;											// a differenza delle celle delle linked list i nodi di un albero binario hanno due riferimenti, a un figlio sinistro...
	struct _BinTreeNode* right;											// ..e uno destro
};																		// molte delle funzioni usate negli alberi sono ricorsive essendo gli alberi delle strutture autosimili, ossia sono simili ad una parte di se stessi

typedef struct _BinTreeNode BinTree;
typedef struct _BinTreeNode BinTreeNode;

BinTreeNode* bintree_node_new(int data){								// funzione per creare un nuovo nodo
	BinTreeNode* ret = (BinTreeNode*)malloc(sizeof(BinTreeNode));		// alloca la memoria necessaria
	*ret = (BinTreeNode){data,NULL,NULL};								// inizializza il nodo, il cui elemeneto è quello passato alla funzioni e risulta privo di figli (left e right sono NULL)
	return ret;															// ritorna il puntatore al nodo
}

BinTree* bintree_new(int root_data){									// funzione per creare un nuovo albero
	return bintree_node_new(root_data);									// in sostanza è uguale alla funzione per creare un nodo di un albero, non cambia nulla, in ogni caso ritorna un puntatore a un nodo, che noi decidiamo essere la radice
}

void bintree_free(BinTree* pTree){										// funzione per liberare la memoria allocata per una struttura albero (in modo ricorsivo)
	if(pTree->left) bintree_free(pTree->left);							// se il nodo passato ha un nodo sinistro allora viene chiamata ricorsivamente la funzione sul nodo sinistro
	if(pTree->right) bintree_free(pTree->right);						// lo stesso per la parte destra dell'albero
	free(pTree);														
}

void bintree_print_postorder(BinTree* pTree){							// attraversa l'albero in post ordine, stampandone gli elementi, funzione ricorsiva
	if(pTree == NULL)return;											// questo è il caso base, quando un nodo è nullo la funzione ritorna
	bintree_print_postorder(pTree->left);								
	bintree_print_postorder(pTree->right);
	printf("%d ", pTree->data);
}

void bintree_print_inorder(BinTree* pTree){								// attraversa l'albero in ordine, stampandone gli elementi, funzione ricorsiva
	if(pTree == NULL) return;											// caso base
	bintree_print_inorder(pTree->left);									
	printf("%d ",pTree->data);
	bintree_print_inorder(pTree->right);
}

void bintree_print_preorder(BinTree* pTree){							// attraversa l'albero in pre ordine, stampandone gli elementi, funzione ricorsiva
	if(pTree == NULL) return;											// caso base
	printf("%d ",pTree->data);
	bintree_print_preorder(pTree->left);
	bintree_print_preorder(pTree->right);
}

//FUNZIONI BST, da qui in poi si suppone che l'albero sia ordinato,abbia una gerarchia, nel senso che i nodi a sinistra di un nodo hanno valore minore, quelli alla destra maggiore

BOOLEAN bintree_contains(BinTree* pTree,int key){						// ricerca un valore all'interno dell'albero, in modo ricorsivo
	if (pTree==NULL) return FALSE;										// se il nodo è NULL allora ritorna falso, questo è il caso base, se si arriva a una delle foglie e neanche quella ha come valore quello cercato allora il valore non è nell'albero
	else if (key<pTree->data) bintree_contains(pTree->left,key);		// se il valore cercato è minore rispetto all'elemento contenuto nel nodo attuale, allora si ricerca il valore nel sottoalbero SINISTRO
	else if (key>pTree->data) bintree_contains(pTree->right,key);		// se il valore cercato è maggiore rispetto all'elemento contenuto nel nodo attuale, allora si ricerca il valore nel sottoalbero DESTRO
	return TRUE;														// se il valore non è nè maggiore nè minore allora vuol dire che è uguale, quindi è stato trovato, quindi la funzione ritorna vero
}

void bintree_insert(BinTree** pTree,int key){								// inserisce il valore nell'albero, mantenendo la gerarchia, in modo ricorsivo (l'implementazione è leggermente diversa da quella fatta a lezione, va a modificare direttamente l'albero passato alla funzione piuttosto che ritornarne uno nuovo
	if (*pTree == NULL) *pTree = bintree_node_new(key);						// caso base, quando il nodo è null allora si crea un nuovo nodo contenente il valore voluto (key)
	else if (key < (*pTree)->data) bintree_insert(&(*pTree)->left,key);		// se il valore è minore rispetto all'elemento del nodo attuale allora si richiama la funzione ricorsivamente per la parte sinistra dell'albero, fintanto che non si arriva al caso base
	else if (key > (*pTree)->data) bintree_insert(&(*pTree)->right,key);	// se il valore è maggiore rispetto all'elemento del nodo attuale allora si richiama la funzione ricorsivamente per la parte destra dell'albero, fintanto che non si arriva al caso base
}

BinTreeNode* bintree_leftmost(BinTree* pTree){							// funzione di supporto utile per la funzione delete successiva, trova il nodo più a sinistra possibile rispetto un nodo iniziale
	BinTreeNode* it = pTree;											// viene costruito un nodo iteratore
	while(it && it->left)												// fintanto che il nodo esiste ed ha un figlio sinistro..
		it = it->left;													// ..pone l'iteratore uguale al figlio sinistro
	return it;															// il ciclo termina quando l'iteratore arriva ad un nodo che non ha figli sinistri, e restituisce quel nodo, essendo il nodo più a sinistra possibile
}

void bintree_delete(BinTree** pTree,int key){								// funzione per cancellare un nodo, mantenendo la gerarchia, ci sono 3 casi, vedere sulla lezione appropriata (12) per spiegazione
	if (*pTree == NULL) return;
	if 		(key<(*pTree)->data) bintree_delete(&(*pTree)->left,key);
	else if (key>(*pTree)->data) bintree_delete(&(*pTree)->right,key);
	else {
		// only one or no child
		if((*pTree)->left == NULL){
			BinTreeNode* tmp = (*pTree);
			(*pTree) = (*pTree)->right;
			free(tmp);
			return;
		}
		else if((*pTree)->right == NULL){
			BinTreeNode* tmp = (*pTree);
			(*pTree) = (*pTree)->left;
			free(tmp);
			return;
		}
		// two children
		BinTreeNode* leftmost = bintree_leftmost((*pTree)->right);
		(*pTree)->data = leftmost->data;
		bintree_delete(&(*pTree)->right,leftmost->data);
	}
}

void bintree_length(BinTree* pTree,int* len){					// funzione che ritorna il numero totale di nodi di un albero, ricorsiva, assume che venga passato con len un intero uguale a 0 che viene modificato dalla funzione
	if(pTree==NULL)return;										// caso base, se il nodo è NULL allora ritorna			
	bintree_length(pTree->left,len);							
	(*len)++;													// essenzialmente la funzione attraversa l'albero in order (notare che infatti è molto simile a print_in_order) e ad ogni nodo incontrato aumenta len di uno (++)
	bintree_length(pTree->right,len);							// questa funzione può essere usata per capire che grandezza dare all'array che si ottiene dalla funzione successiva
}

void bintree_to_array(BinTree* pTree,int* dst,int* count){		// funzione che costruisce un array ordinato contenente tutti gli elementi dell'albero, in ordine crescente, sfrutta anche in sto caso la ricorsione
	if(pTree == NULL)return;									// caso base
	bintree_to_array(pTree->left,dst,count);					
	dst[(*count)++] = pTree->data;								// anche in questo caso l'albero viene attraversato in ordine (che nel caso di un albero ordinato vuol dire dall'elemento più piccolo al più grande)
	bintree_to_array(pTree->right,dst,count);					
}

void bintree_to_inverse_array(BinTree* pTree,int* dst,int* count){	// funzione che costruisce un array in ordine inverso rispetto a quello che si costruisce con la funzione precedente, anche questa funzione usa ricorsione
	if(pTree == NULL)return;										// caso base
	bintree_to_inverse_array(pTree->right,dst,count);		
	dst[(*count)++] = pTree->data;									// è facile vedere che rispetto alla funzione precedente questo è l'ordine opposto rispetto all'attraversamento in order di un albero, infatti in sto modo si va da..
	bintree_to_inverse_array(pTree->left,dst,count);				// ..destra a sinistra di un albero, dall'elemento più grande al più piccolo
}
*/





// IMPLEMENTAZIONE GRAFO (PESATO) CON MATRICE DI ADIACENZA
/*
struct _Graph{
	int* adj;
	int size;
};
typedef struct _Graph Graph;

Graph* new_graph(int size){
	Graph* res = (Graph*)malloc(sizeof(Graph));
	*res = (Graph){(int*)malloc(sizeof(int)*size*size),size};
	int i;
	for(i=0;i<size*size;i++)
		res->adj[i] = 0;
	return res;
}

void graph_free(Graph* pGraph){
	assert(pGraph);
	free(pGraph->adj);
	free(pGraph);
}

void adjmat_set(Graph* pGraph,int src,int dst,int weight){
	pGraph->adj[src*pGraph->size+dst] = weight;
}

int adjmat_get(Graph* pGraph,int src,int dst){
	return pGraph->adj[src*pGraph->size+dst];
}

void graph_add_directed_edge(Graph* pGraph,int src,int dst,int weight){
	adjmat_set(pGraph,src,dst,weight);
}

void graph_add_edge(Graph* pGraph,int src,int dst,int weight){
	adjmat_set(pGraph,src,dst,weight);
	adjmat_set(pGraph,dst,src,weight);
}

void graph_print(Graph* pGraph){
	printf("   ");
	int i,j;
	for(i=0;i<pGraph->size;i++)
		printf("%d ",i);
	printf("\n   ");
	for(i=0;i<pGraph->size;i++)
		printf("__");
	for(i=0;i<pGraph->size;i++){
		printf("\n%d | ",i);
		for(j=0;j<pGraph->size;j++)
			printf("%d ",pGraph->adj[i*pGraph->size+j]);
	}
}
*/



// IMPLEMENTAZIONE GRAFO (PESATO) CON LISTA DI ADIACENZA;

struct _CELL{
	int element;
	int weight;
	struct _CELL* next;
};
typedef struct _CELL CELL;

struct _ListHead{
	CELL* front;
	CELL* back;
	int size;
};
typedef struct _ListHead ListHead;

ListHead* list_new(){
	ListHead* res = (ListHead*)malloc(sizeof(ListHead));
	*res = (ListHead){NULL,NULL,0};
	return res;
}

void list_free(ListHead* pList){
	assert(pList);
	CELL* it = pList->front;
	while(it){
		CELL* tmp = it;
		it = it->next;
		free(tmp);
	}
	free(pList);
}

void list_add(ListHead* pList, int elem, int weight){
	CELL* new_cell = (CELL*)malloc(sizeof(CELL));
	new_cell->element = elem;
	new_cell->weight = weight;
	new_cell->next = NULL;
	if(pList->back)
		pList->back->next = new_cell;
	else
		pList->front = new_cell;
	pList->back = new_cell;
	pList->size++;
}

struct _Graph{
	ListHead** adj_list;
	int size;
};
typedef struct _Graph Graph;

Graph* new_graph(int size){
	Graph* res = (Graph*)malloc(sizeof(Graph));
	*res = (Graph){(ListHead**)malloc(sizeof(ListHead*)*size),size};
	int i;
	for(i = 0;i<size;i++)
		res->adj_list[i] = list_new();
	return res;
}

void graph_free(Graph* pGraph){
	int i;
	for(i=0;i<pGraph->size;i++)
		list_free(pGraph->adj_list[i]);
	free(pGraph->adj_list);
	free(pGraph);
}

void graph_add_directed_edge(Graph* pGraph,int src,int dst,int weight){
	list_add(pGraph->adj_list[src],dst,weight);
}

void graph_add_edge(Graph* pGraph,int src,int dst,int weight){
	list_add(pGraph->adj_list[src],dst,weight);
	list_add(pGraph->adj_list[dst],src,weight);
}

void graph_print(Graph* pGraph){
	int i;
	for(i=0;i<pGraph->size;i++){
		printf("[%d] ",i);
		CELL* it = pGraph->adj_list[i]->front;
		while(it){
			printf("-> (%d, weight: %d) ",it->element,it->weight);
			it = it->next;
		}
		printf("\n");
	}
}

// RICERCHE DFS E BFS NEI GRAFI (PER IMPLEMENTAZIONE CON LISTA DI ADIACENZE)

void _graph_dfs(Graph* pGraph,int v,int visited[]){
	visited[v] = TRUE;
	printf("%d ",v);
	CELL* it = pGraph->adj_list[v]->front;
	while(it){
		if(!visited[it->element]) 
			_graph_dfs(pGraph,it->element,visited);
		it = it->next;
	}
}

void graph_dfs(Graph* pGraph,int start){
	int* visited = (int*)malloc(sizeof(int)*pGraph->size);
	int i;
	for(i=0;i<pGraph->size;i++)
		visited[i] = FALSE;
	printf("DFS from %d: ",start);
	_graph_dfs(pGraph,start,visited);
	free(visited);
	printf("\n");
}

struct _QUEUE{
	CELL* front;
	CELL* back;
	int size;
};
typedef struct _QUEUE QUEUE;

QUEUE* queue_new(){
	QUEUE* res = (QUEUE*)malloc(sizeof(QUEUE));
	*res = (QUEUE){NULL,NULL,0};
	return res;
}

void queue_free(QUEUE* pQ){
	assert(pQ);
	CELL* it = pQ->front;
	while(it){
		CELL* tmp = it;
		it = it->next;
		free(tmp);
	}
	free(pQ);
}

void queue_enqueue(QUEUE* pQ, int elem){
	CELL* new_cell = (CELL*)malloc(sizeof(CELL));
	new_cell->element = elem;
	new_cell->next = NULL;
	if(pQ->front)
		pQ->back->next = new_cell;
	else
		pQ->front = new_cell;
	pQ->back = new_cell;
	pQ->size++;
}

BOOLEAN is_empty(QUEUE* pQ){
	return pQ->front == NULL;
}

BOOLEAN queue_deque(QUEUE* pQ,int* element){
	if(!is_empty(pQ)){
		*element = pQ->front->element;
		pQ->front = pQ->front->next;
		pQ->size--;	
		return TRUE;
	} else return FALSE;
}


void graph_bfs(Graph* pGraph,int start){
	int* visited = (int*)malloc(sizeof(int)*pGraph->size);
	int i;
	for(i=0;i<pGraph->size;i++)
		visited[i] = FALSE;
	QUEUE* queue = queue_new();
	visited[start] = TRUE;
	queue_enqueue(queue,start);
	printf("BFS from %d :",start);
	while(queue_deque(queue,&start)){
		printf("%d ",start);
		CELL* it = pGraph->adj_list[start]->front;
		while(it){
			if(!visited[it->element]){
				visited[it->element] = TRUE;
				queue_enqueue(queue,it->element);	
			}
			it = it->next;
		}
	}
	printf("\n");
	free(visited);
}

int main(void){
	// scommentare per testare, seguendo indicazioni sulle implementazioni dove fornite
	
	//TEST PER STACK 
	/*
	STACK my_stack = {.elements = NULL,.size=2};	//implementazione con array
	// STACK my_stack = {NULL};					//implementazione con linked list
	int arr[] = {6,9,1,0,-4,5,6,3,33,2,11,-98};
	int i,len = sizeof(arr) / sizeof(arr[0]);
	for(i=0;i<len;i++)
		push(&my_stack,arr[i]);
	print_stack(&my_stack);	// essendo una struttura LIFO ci si aspetta che venga stampato in ordine inverso rispetto all'ordine in cui sono stati aggiunti gli elementi, ossia inverso rispetto l'array
	int my_elem;
	top(&my_stack,&my_elem);
	printf("Elemento in cima alla pila: %d\n",my_elem);
	*/
								
	
	
	
	// TEST PER QUEUE
	/*
	 QUEUE my_queue = {.elements=NULL,.size=2};	//implementazione con array, sia semplice che circolare
	// QUEUE my_queue = {NULL,NULL}; // implementazione con lista concatenata con due riferiementi
	int arr[] = {6,9,1,0,-4,5,6,3,33,2,11,-98,-31,114,89,93,-4};
	int i,len=sizeof(arr)/sizeof(arr[0]),my_elem;
	for(i=0;i<len;i++){
		enqueue(&my_queue,arr[i]);
	}
	printf("Coda: ");
	print_queue(&my_queue);		// essendo una struttura FIFO l'ordine di stampa sarà uguale all'ordine di arr
	*/
	
	
	
	//CONTAINER/SET TEST (per implementazione array)
	/*
	struct CONTAINER* my_container = container_new();  						// per implementazione con linked list. commentare la riga se si vuole usare implementazione array e scommentare quella sotto
	// CONTAINER* my_container = container_new(2);							// per implementazione con array. commentare la riga se si vuole usare implementazione linked list e scommentare quella sopra
	int arr[] = {6,9,1,0,-4,5,6,3,3,3,2,11,-98,-31,114,89,932,2,2,2,2,-4,-4};	
	int i,len=sizeof(arr)/sizeof(arr[0]);
	for(i=0;i<len;i++)
		container_add(my_container,arr[i]);
	printf("Container primo set: ");
	container_print(my_container);
	SET* my_set = set_new(2);
	for(i=0;i<len;i++)
		set_add(my_set,arr[i]);
	printf("Primo set: ");
	set_print(my_set);
	SET* my_second_set = set_new(2);
	int arr2[] = {6,3,4,15,-2,16,4,5,6,3,11};
	int j,len2 = sizeof(arr2)/sizeof(arr2[0]);
	for(j=0;j<len2;j++)
		set_add(my_second_set,arr2[j]);
	printf("Secondo set: ");
	set_print(my_second_set);
	SET* union_set = set_union(my_set,my_second_set);
	SET* intersect_set1 = set_intersect(my_set,my_second_set);
	SET* intersect_set2 = set_intersect(my_second_set,my_set);
	SET* subtract_set1 = set_subtract(my_set,my_second_set);
	SET* subtract_set2 = set_subtract(my_second_set,my_set);
	printf("Unione: ");
	set_print(union_set);
	printf("Set1 intersecato Set2 : ");
	set_print(intersect_set1);
	printf("Set2 intersecato Set1 : ");
	set_print(intersect_set2);
	printf("Set1 meno Set2: ");
	set_print(subtract_set1);
	printf("Set2 meno Set1: ");
	set_print(subtract_set2);
	*/
	
	
	
	// TEST MAPPE
	/*
	// Map* my_map = map_new(2);									// implementazione della mappa tramite array
	Map* my_map = map_new();										// implementazione della mappa tramite linked list
	BinaryRel arr[] = {{1,"cane"},{1,"gatto"},{1,"topo"},{2,"gallina"},{3,"cigno"},{4,"canarino"},{2,"gru"},{6,"coccodrillo"},{12,"elefante"},{6,"mantide"},{7,"balena"},{7,"falena"},{5,"pappagallo"},{21,"scimmia"}};
	int i, len = sizeof(arr)/sizeof(arr[0]);
	for(i=0;i<len;i++)
		map_add(my_map,&(arr[i]));
	map_print(my_map);												//nella mappa compare solo una volta ciascuna chiave, quindi alcuni elementi dell'array scritto sopra non fanno parte della mappa (tipo (7,falena) o (1,gatto))
	char my_value[20];
	strcpy(my_value,map_lookup(my_map,21));							//copia il valore ritornato da lookup nella stringa my_value
	printf("Valore associato alla chiave 21: %s\n",my_value);		//stampo la stringa associata alla chiave 21
	printf("La mappa contiene la chiave 8? %s", (map_contains_key(my_map,8)? "TRUE" : "FALSE"));
	*/
	
	
	
	
	// TEST PER MMAP 
	/*
	BinaryRel* my_rel = BinaryRel_new(1,2); // per implementazione tramite array
	char arr[][20] = {"mamma","gatto","topo","frate","seppia","orso","mano","telefono","bombola","cuffie"};							//creo 4 strutture BinaryRel
	int i, len = sizeof(arr) / sizeof(arr[0]);
	for(i=0;i<len;i++)
		BinaryRel_add(my_rel,arr[i]);																								//prima, avente chiave 1 e valori presi da arr
	BinaryRel* my_rel2 = BinaryRel_new(2,2);
	char arr2[][20] = {"calcolatrice","disco","natura","spiaggia","foglio","cellulosa","endorfine"};
	int j, len2 = sizeof(arr2)/sizeof(arr2[0]);
	for(j=0;j<len2;j++)
		BinaryRel_add(my_rel2,arr2[j]);																								// seconda, avente chiave 2 e valori presi da arr2
	BinaryRel* my_rel3 = BinaryRel_new(3,2);
	char arr3[][20] = {"medicina","scienza","calore","televisione","sole","cuore","scorpione","giuramento","scoglio","spirale"};
	int k, len3 = sizeof(arr3)/sizeof(arr3[0]);
	for(k=0;k<len3;k++)
		BinaryRel_add(my_rel3,arr3[k]);																								// terza ,avente chiave 3 e valori presi da arr3
	BinaryRel* my_rel4 = BinaryRel_new(1,2);
	char arr4[][20] = {"canguro","muffa","sandalo","pietra","felpa","zebra","gatto","gatto","mamma","cellulosa"};
	int w, len4 = sizeof(arr4)/sizeof(arr4[0]);
	for(w=0;w<len4;w++)
		BinaryRel_add(my_rel4,arr4[w]);																								// quarta, avente chiave 1 e valori presi da arr4
	mMap* my_mmap = mmap_new(1);
	mmap_add(my_mmap,my_rel);
	mmap_add(my_mmap,my_rel2);
	mmap_add(my_mmap,my_rel3);
	mmap_add(my_mmap,my_rel4);								// i valori che ha la rleazione binaria numero 4, dato che la chiave è la stessa della numero 1 (ossia chiave 1) vengono aggiunti senza creare un'ulteriore chiave 1
	mmap_print(my_mmap);									// notare che pur essendoci doppioni di valori nella chiave 1 per esempio (gatto), questi appaiono una sola volta come valori della chiave, per scelta di implementazione
	printf("\n");
	T* values;												// array di stringhe di 20 caratteri
	int len5 = mmap_lookup(my_mmap,3,&values);				// lo inizializzo e trovo la sua grnadezza len5 tramite mmap_lookup()
	printf("Valori della chiave 3: ");
	for(i=0;i<len5;i++)
		printf("%s ",values[i]);							// stampo quindi uno ad uno i valori dell'array values, ossia i valori associati alla chiave 3
	printf("\n");
	mmap_free(my_mmap); 									// libera la memoria relativa alla multimappa creata
	*/
	
	
	// TEST PER HASH TABLE
	/*
	HashTable *table = hashtable_new(3,5);
	Book books[] = {{ 4, "Moby Dick", "Herman Melville", 1851 }, { 1, "The Hitchhiker's Guide to the Galaxy", "Douglas Adams", 1978 },{ 0, "La Commedia", "Dante Alighieri", 1477 },
					{12,"Il signore degli anelli","j.R.R.Tolkien",1955},{18,"Harry Potter e il Calice di Fuoco","J.K.Rowling",2000},{47,"Il segreto dei Maghi","Trudi Canavan",2003},
					{90,"Il ciclo di Shannara","Terry Brooks",1977},{33,"Cronache del mondo Emerso","Licia Troisi",2004},{65,"The Intelligent Investor","Benjamin Graham",1949},{78,"Random Title","Random Author",2017}};
	int i;
	for(i=0; i<sizeof(books)/sizeof(books[0]); i++) 
		hashtable_add(table, &books[i]);   
	printf("LOOKUP MAP for KEY = Moby Dick \n");   
	Book *book_k = hashtable_lookup(table, "Moby Dick");
	if(book_k)
		print_book(book_k);
	printf("\nALL BOOKS IN THE HASH TABLE: \n");
	hashtable_print(table);
	hashtable_free(table);
	*/
	
	
	
	// TEST BINARY TREE (non ordinato)
	/*
	BinTree* root  = bintree_new(1);
	root->left = bintree_node_new(2);
	root->left->left = bintree_node_new(3);
	root->left->right = bintree_node_new(4);
	root->left->right->left = bintree_node_new(5);
	root->right = bintree_node_new(6);
	root->right->left = bintree_node_new(7);
	root->right->right = bintree_node_new(8);
	root->right->right->left = bintree_node_new(9);
	root->right->right->right = bintree_node_new(10);
	// conviene disegnarlo per capirci qualcosa
	printf("Preorder traversal of binary tree is \n");    
	bintree_print_preorder(root);
	printf("\nInorder traversal of binary tree is \n");
	bintree_print_inorder(root);
	printf("\nPostorder traversal of binary tree is \n");
	bintree_print_postorder(root);
	bintree_free(root);
	*/
	
	
	// TEST BST albero binario ordinato
	/*
	BinTree *root = NULL;
	int a[] = {50, 30, 20, 40, 70, 60, 80,100,904,34,89,45,73,3,-4,7,19,24,-33,65,71,74,43};
	int i;
	for(i=0; i<sizeof(a)/sizeof(a[0]); i++)
		bintree_insert(&root, a[i]);
	int len = 0;
	bintree_length(root,&len);
	printf("Albero iniziale\n");
	printf("Quantita' di elementi inizialmente nell'albero: %d\n",len);   
	printf("Inorder traversal of the given tree \n");
	bintree_print_inorder(root); 
	printf("\nPreorder traversal of the modified tree \n");
	bintree_print_preorder(root);
	// anche in sto caso conviene disegnare l'albero per capirci qualcosa, specie di come avvengono le seguenti cancellazioni di nodi
	printf("\n\n");
	printf("Looking weather 40 is in the tree ");
	printf("%s \n\n", bintree_contains(root, 40) ? "TRUE" : "FALSE" );
	printf("Delete 20\n");
	bintree_delete(&root, 20);
	printf("Inorder traversal of the modified tree \n");
	bintree_print_inorder(root);
	printf("\nPreorder traversal of the modified tree \n");
	bintree_print_preorder(root);
	printf("\n\n");
	printf("Delete 70\n");
	bintree_delete(&root, 70);
	printf("Inorder traversal of the modified tree \n");
	bintree_print_inorder(root);
	printf("\nPreorder traversal of the modified tree \n");
	bintree_print_preorder(root);
	printf("\n\n");
	printf("Delete 100\n");
	bintree_delete(&root, 100);
	printf("Inorder traversal of the modified tree \n");
	bintree_print_inorder(root);
	printf("\nPreorder traversal of the modified tree \n");
	bintree_print_preorder(root);
	printf("\n\n");
	int arr_len = 0,count=0,count2=0;
	bintree_length(root,&arr_len);
	int dst[arr_len];
	int inv_dst[arr_len];
	bintree_to_array(root,dst,&count);
	bintree_to_inverse_array(root,inv_dst,&count2);
	printf("Quantita' di elementi nell'albero: %d\n",arr_len);
	printf("Array in ordine:\n");
	int k;
	for(k=0;k<arr_len;k++)
		printf("%d ",dst[k]);
	printf("\nArray in ordine inverso:\n");
	for(k=0;k<arr_len;k++)
		printf("%d ",inv_dst[k]);
	printf("\n");
	bintree_free(root);
	*/
	
	
	//TEST GRAFO PESATO
	Graph* my_graph = new_graph(10);
	graph_add_edge(my_graph,0,1,2);
	graph_add_edge(my_graph,1,7,6);
	graph_add_edge(my_graph,7,2,3);
	graph_add_edge(my_graph,7,6,4);
	graph_add_edge(my_graph,2,3,1);
	graph_add_edge(my_graph,0,4,3);
	graph_add_edge(my_graph,4,8,3);
	graph_add_directed_edge(my_graph,3,6,7);
	graph_add_directed_edge(my_graph,9,8,2);
	graph_add_directed_edge(my_graph,9,6,7);
	graph_add_directed_edge(my_graph,5,5,1);
	graph_add_directed_edge(my_graph,4,6,4);
	graph_print(my_graph);
	// printf("\n\nPeso dell'arco fra 3 e 4: %d\n",adjmat_get(my_graph,3,4));
	graph_dfs(my_graph,6);
	graph_bfs(my_graph,6);
	graph_free(my_graph);
	return 0;
}

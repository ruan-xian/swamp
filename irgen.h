#ifndef IRGEN
#define IRGEN

#include <stdio.h>
#include <stdlib.h>

struct Node {
    void *val;
    struct Node *next;
};

struct List {
    struct Node *head;
    struct Node *end;
    int len;
};

int shreksays(const char *fmt);

char* concat(char* ptr1, char* ptr2);

bool isEmptyList(struct List *l); 
struct List *newEmptyList();
struct Node *newNode(void *val);
struct List *appendNode(struct List *l, struct Node *n);
struct List *catList(struct List *li, struct List *l2);
struct List *consList(void *val, struct List *l); 
void *getHead(struct List *l); 
struct List *getTail(struct List *l); 

#endif

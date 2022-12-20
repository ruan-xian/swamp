#include <stdio.h>
#include "irgen.h"
#include <string.h>

int shreksays(const char *fmt) {
    printf("%s", fmt);
    return 0;
}

struct List *newEmptyList() {
    struct List *new = malloc(sizeof(struct List));
    memset(new, 0, sizeof(struct List));

    return new;
}

struct Node *newNode(void *val) {
    struct Node *new = malloc(sizeof(struct Node));

    new->val = val;
    new->next = NULL;

    return new;
}

struct List *appendNode(struct List *l, struct Node *n) {
    if (!(l->head)) {
	l->head = n;
	l->end = n;
    } else {
	(l->end)->next = n;
	l->end = n;
    }
    return l;
}

struct List *catList(struct List *l1, struct List *l2) {
    (l1->end)->next = l2->head;
    l1->end = l2->end;

    return l1;
}

struct List *consList(void *val, struct List *l) {
    struct Node *n = newNode(val);
    n->next = l->head;
    l->head = n;

    return l;
}

void *getHead(struct List *l) {
    return (l->head)->val;
}

struct List *getTail(struct List *l) {
    l->head = (l->head)->next;

    return l;
}    

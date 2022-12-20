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

struct List *newEmptyList();
struct Node *newNode();
struct List *appendNode();

#endif

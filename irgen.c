#include <stdio.h>
#include "irgen.h"
#include <string.h>
#include <stdbool.h>

int shreksays(const char *fmt) {
    printf("%s", fmt);
    return 0;
}

char* concat(char* ptr1, char* ptr2) {
    int len_one = strlen(ptr1);
    int len_two = strlen(ptr2);
    int n = len_one + len_two + 1;
    // char new[len_one + len_two + 1];
    char* new = (char*) malloc(n * sizeof(char));

    strncat(new, ptr1, len_one);
    strncat(new, ptr2, len_two);

    return new;

}

const char* int_to_string(int x) {
    char *int_str = malloc(20);
    sprintf(int_str, "%d", x);
    return int_str;
}

const char* float_to_string(float x) {
    int len = snprintf(NULL, 0, "%f", x);
    char *float_str = malloc(len + 1);
    snprintf(float_str, len + 1, "%f", x);
    return float_str;
}

const char* bool_to_string(bool x) {
    char *bool_str = malloc(10);
    if (x == true) {
        bool_str = "True";
    }
    else {
        bool_str = "False";
    }
    return bool_str;
}

struct List *newEmptyList() {
    struct List *new = malloc(sizeof(struct List));
    memset(new, 0, sizeof(struct List));

    return new;
}

struct Node *newNode(void *val) {
    struct Node *new = malloc(sizeof(struct Node));
    memset(new, 0, sizeof(struct Node));

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
    struct List *new = newEmptyList();
    struct Node *curr = l1->head;

    while(curr) {
	struct Node *n = newNode(curr->val);
	appendNode(new, n);
	curr = curr->next;
    }
    curr = l2->head;
    while(curr) {
	struct Node *n = newNode(curr->val);
	appendNode(new, n);
	curr = curr->next;
    }
    
    return new;
}

struct List *consList(void *val, struct List *l) {
    struct List *new = newEmptyList();
    struct Node *n = newNode(val);
    
    appendNode(new, n);
    struct Node *curr = l->head;
    while(curr) {
	struct Node *n = newNode(curr->val);
	appendNode(new, n);
	curr = curr->next;
    }

    return new;
}

void *getHead(struct List *l) {
    return (l->head)->val;
}

struct List *getTail(struct List *l) {
    struct Node *n = l->head;
    if(!(n->next)) {
	return newEmptyList();
    }

    struct List *newlist = newEmptyList(); 
    while (n) {
	struct Node *nn = newNode(n->val);
	appendNode(newlist, nn);
	n = n->next;
    }

    return newlist;
}    

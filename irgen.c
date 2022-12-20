#include <stdio.h>
#include "irgen.h"
#include <string.h>

int shreksays(const char *fmt) {
    printf("%s", fmt);
    return 0;
}

const char* intToString(int x) {
    char *int_str = malloc(20);
    sprintf(int_str, "%d", x);

    return int_str;
}

const char* floatToString(float x) {
    int len = snprintf(NULL, 0, "%f", x);
    char *float_str = malloc(len + 1);
    snprintf(float_str, len + 1, "%f", x);
    
    return float_str;
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

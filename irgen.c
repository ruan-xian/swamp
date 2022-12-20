#include <stdio.h>
#include "irgen.h"
#include <string.h>

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

    // int i;

    // char* copy = ptr1;

    // for (i = 0; i < len_one; i++) {
    //     new[i] = *copy;
    //     copy += 1;
    // }

    // copy = ptr2;

    // for (i; i < (len_one + len_two); i++) {
    //     new[i] = *copy;
    //     copy += 1;
    // }


    return new;

}

const char* intToString(int x) {
    char int_str[20];
    sprintf(int_str, "%d", x);
    return int_str;
}

const char* floatToString(float x) {
    char float_str[50];
    fprintf(float_str, "%f", x);
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

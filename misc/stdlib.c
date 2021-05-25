
#include <stdio.h>
#include <string.h>

typedef char* string;

int print_int(int i) { 
    printf("%d\n", i); 
    return 0; 
}

int print_bool(int b) { 
    if (b == 1) printf("true\n"); 
    else printf("false\n"); 
    return 0; 
}

int print_float(float f) { 
    printf("%f\n", f); 
    return 0; 
}

int print_string(string s) {
    printf("%s\n", s);
    return 0;
}

int read_int() {
    int i;
    scanf("%d", &i);
    return i;
}

int read_bool() {
    int i;
    char* s;
    scanf("%s", s);
    if (strcmp(s, "true")) i = 1;
    else i = 0;
    return i;
}

float read_float() {
    float f;
    scanf("%f", &f);
    return f;
}

string read_string() {
    string s;
    scanf("%s", s);
    return s;
}

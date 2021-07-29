#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MAX_C 200

typedef char* String;

typedef struct person {
  int number;
  int age;
  int cCount;
  int status;
  int counter;
  int connections[MAX_C];
  int con_type[MAX_C];
} Person, *pPerson;

pPerson mallocPerson();
void copyPerson(pPerson dest, pPerson src);
pPerson* mallocNetwork(int size);
pPerson* createNetwork(int size);
void copyNetwork(pPerson* dest, pPerson* source, int size);
pPerson* createCopy(pPerson* src, int size);
void printNetwork(pPerson* network, int size, String fileName);
void fprintPerson(pPerson person, FILE *file);
void genCareHomes(pPerson *base, int size, int* wNet, int* wP, int* hP);
void genHouseHolds(pPerson *base, int size, int* hNew, int* hp, int* childlist, int* cpos);
void genSchools(pPerson *base, int size, int* childlist);
void genWork(pPerson *base, int size, int* wNet, int* wpos);
void genFriends(pPerson *base, int size);
double normalRandom();
void shuffle(int* array, int size);
void swap(int* a, int* b);
void freePerson(pPerson input);
void freeNetwork(pPerson* input, int size);

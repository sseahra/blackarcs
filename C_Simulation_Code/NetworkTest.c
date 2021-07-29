#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include "NetworkGen.h"
#include <time.h>

int main() {
  int size = 20000;
  //int connections = 12;
  String fName = "Net_Test.txt";

  clock_t start, end;
  double cpu_time_used;
  start = clock();
  pPerson* test = createNetwork(size);
  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("Person Size: %ld\nArray size: %ld\nTime: %f\n",sizeof(test[1]),sizeof(test), cpu_time_used);
  printNetwork(test, size, fName);
  
  freeNetwork(test, size);
  return(0);
}

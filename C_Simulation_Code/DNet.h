#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "NetworkGen.h"

void simulate(pPerson* orig, int size, int runs, String file, double data[], int verbose);

void day(pPerson* network, int size, double data[]);

void infect(pPerson person, double beta, double rate, int* final);

void test(pPerson person, double beta, int* final);

void fPrintNetworkStatus(pPerson* network, int size, FILE* fPtr, int run);

void event(pPerson* network, int size, int amount, double beta);

/*
  List of Disease Statuses:
  0 - Susceptible
  1 - Exposed
  2 - Pre-symptomatic
  3 - Symptomatic
  4 - Asymptomatic
  5 - Removed
  6 - Tested
  7 - Tested Removed
*/

/*
  Connection Types:
  0 - Yellow, Orange, or Red Phase
  1 - Yellow or Orange Phase
  2 - Only Yellow Phase
 */

/*
  State Types:
  0 - Yellow
  1 - Orange 
  2 - Red
 */

/* DATA ORDER - HIGHLY IMPORTANT
   0 - Days
   1 - Beta (Infection Chance)
   2 - Alpha (Exposed to Pre-Symptomatic Chance)
   3 - Gamma (Recovery Chance)
   4 - Asymptomatic Chance
   5 - Asymptomatic Infection Rate
   6 - Symptomatic (Voluntary) Test Chance
   7 - Random Test Chance
   8 - Test Success Chance
   9 - Pre-symp Time (Days)
   10 - Average number of days between events 
   ----BELOW NOT YET IMPLEMENTED----
   11 - Border Crossings Per Day
   12 - Importation Infection Chance
   13 - Travel Isolation Time 
   14 - Travel Isolation Type (0 = full house, 1 = just travel, 2 = none)
   15 - Average Travel Time
   16 - Starting Phase (0 = yellow, 1 = orange, 2 = red)
   17 - Orange Case Threshold
   18 - Red Case Threshold 
*/

/*
  typedef struct person {
  int number;
  int age;
  int cCount;
  int status;
  int counter;
  int connections[MAX_C];
  int con_type[MAX_C];
} Person, *pPerson;
*/

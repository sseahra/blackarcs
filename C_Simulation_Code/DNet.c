#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include <sys/stat.h>
#include "DNet.h"

int TI;
int STATE;
FILE* vPtr;
int verbose;
int edit;

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

void simulate(pPerson* orig, int size, int runs, String file, double data[], int ver) {
  srand(time(NULL));
  if(ver == 1){
    verbose = 1;
  }
  
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
  */

  //Open file by provided name
  int compare = strcmp(file, "stdout");
  FILE* fPtr;
  if(compare == 0)
    fPtr = stdout;
  else {
    fPtr = fopen(file, "w");
    if(fPtr==NULL) {
      printf("Failed to open file.\n");
      return;
    }
  }

  //Set the total number of days per simulation
  int days = data[0];

  //Loop through the total number of executions
  for(int i = 0; i < runs; i++) {
    edit = 0;

    //Copy the network
    pPerson* testwork = createCopy(orig, size);

    //Print which run we are on, for keeping track of progress
    int frac = i / (double)runs * 10 + 1;
    printf("[");
    int count;
    for(count = 0; count < frac; count++) {
      printf("#");
    }
    for(; count < 10; count++) {
      printf("-");
    }
    
    printf("] \tRun %d of %d\r",i+1, runs);
    fflush(stdout);

    //Pick the index of a random person to be infected and set infected to 1
    int randval = rand() % size;
    testwork[randval]->status = 4;
    TI = 1;
    STATE = 0;

    //If verbose, save data
    char buffer[100];
    if(verbose == 1) {
      snprintf(buffer, sizeof(buffer), "Simulation_Results/Run_%d.txt", i+1);
      vPtr = fopen(buffer, "w");
      if(fPtr==NULL) {
	printf("Failed to open file.\n");
	return;
      }
    }
    //Loop through the days
    for(int j = 0; j < days; j++) {
      if(verbose == 1) {
	fprintf(vPtr, "%d,", j+1);
      }
      
      //The day
      day(testwork, size, data);

      //Check if a super-spreading event happens
      double eCheck = rand() / ((double) RAND_MAX);
      if((eCheck < 1/data[10]) & (STATE == 0)) {
	//Placeholder average of 50 people with SD of 10 per event
	int amount = round(normalRandom()*10+50);
	event(testwork, size, amount, data[1]);
      }

      //Update the emergency phase
      if(TI > 80) {
	STATE = 2;
      }
      else if(TI > 20) {
	STATE = 1;
      }
      else {
	STATE = 0;
      }
      fprintf(vPtr, "\n");
    }

    //Close verbose file
    if(verbose == 1){
      fclose(vPtr);
    }
    if(edit == 0) {
      remove(buffer);
    }

    //Save and delete the network
    fPrintNetworkStatus(testwork, size, fPtr, i+1);
    freeNetwork(testwork, size);
  }

  //Close the file when done
  fclose(fPtr);
  printf("\n");
}

void day(pPerson* network, int size, double data[]) {
  double rands[size * 2];
  int rand_count = 0;
  for(int i = 0; i < size*2; i++) {
    rands[i] = rand() / ((double) RAND_MAX);
  }

  int final[size];
  for(int i = 0; i < size; i++) {
    final[i] = network[i]->status;
  }
	
  for(int i = 0; i < size; i++) {
    int state = network[i]->status;

    if(state != 0) {
      switch(state) {
      case 1: //Exposed
	if(rands[rand_count] < data[2]) {
	  //Pre-Symp
	  final[i] = 2;
	}
	rand_count += 1;
	break;
	
      case 2: //Pre-symptomatic
	if(network[i]->counter < data[9]) {
	  network[i]->counter += 1;
	}
	else {
	  if(rands[rand_count] < data[4]) {
	    //Asymp
	    final[i] = 4;
	    network[i]->counter = 0;
	  }
	  else{
	    //Symp
	    final[i] = 3;
	    network[i]->counter = 0;
	  }
	  rand_count += 1;
	}
	break;
	
      case 3: //Asymptomatic
	infect( network[i], data[1], data[5], final);

	if(rands[rand_count] < data[3]) {
	  //Removed
	  final[i] = 5;
	}
	else if(rands[rand_count+1] < data[6]) {
	  if(rands[rand_count+2] < data[8]) {
	    //Tested
	    final[i] = 6;
	    TI = TI + 1;
	  }
	}
	rand_count += 3;
	
	break;
	
      case 4: //Symptomatic
	infect(network[i], data[1], data[5], final);

	if(rands[rand_count] < data[3]) {
	  //Removed
	  final[i] = 5;
	}
	else if(rands[rand_count+1] < data[7]) {
	  if(rands[rand_count+2] < data[8]) {
	    //Tested
	    final[i] = 6;
	    TI = TI + 1;
	  }
	}
	rand_count += 3;
	break;
	
      case 6: //Tested	
	if(network[i]->counter == 2) {
	  test(network[i], data[8], final);
	}
	else {
	  network[i]->counter += 1;
	}

	if(rands[rand_count] < data[3]) {
	  //Tested Removed
	  final[i] = 7;
	  TI = TI - 1;
	  rand_count += 1;
	}
	
	break;
	
      case 7: //Tested Removed	
	if(network[i]->counter == 2) {
	  test(network[i], data[8], final);
	}
	else {
	  network[i]->counter += 1;
	}
	break;
      }
    }
  }
  for(int i = 0; i < size; i++) {
    network[i]->status = final[i];
  }
}

void infect(pPerson person, double beta, double rate, int* final) {
  double lrate = 1;
  if(person->status == 3) {
    lrate = rate;
  }

  double randsL[person->cCount];
  for(int i = 0; i < person->cCount; i++) {
    randsL[i] = rand() / ((double) RAND_MAX);
  }

  int listed = 0;

  //Infections for YELLOW
  if(STATE == 0) {
    //Loop through the connections
    for(int i = 0; i < person->cCount; i++) {
      //Conditions of if person is suscetible, if the random works, and if the connection type if valid (NA for Y)
      if((final[person->connections[i]] == 0) & (randsL[i] < (beta*lrate))) {
	
	//If verbose, print out the index of who was infected
	if(edit == 0) {
	  edit = 1;
	}
	
	if((listed == 0) & (verbose == 1)) {
	  fprintf(vPtr, "%d,", (person->number));
	  listed = 1;
	}
	if (verbose == 1) {
	  fprintf(vPtr, "%d,%d,", person->connections[i],person->con_type[i]);
	}

	//Actually infect the person
	final[person->connections[i]] = 1;
      }
    }
  }

  //Infections for ORANGE
  else if(STATE == 1) {
    for(int i = 0; i < person->cCount; i++) {
      if((final[person->connections[i]] == 0) & (randsL[i] < (beta*lrate)) & (person->con_type[i] != 2)) {
	//If verbose, print out the index of who was infected
	if(edit == 0) {
	  edit = 1;
	}
	
	if((listed == 0) & (verbose == 1)) {
	  fprintf(vPtr, "%d,", person->number);
	  listed = 1;
	}
	if (verbose == 1) {
	  fprintf(vPtr, "%d,%d,", person->connections[i],person->con_type[i]);
	}

	//Actually infect the person
	final[person->connections[i]] = 1;
      }
    }
  }

  //Infections for RED
  else if(STATE == 2) {
    for(int i = 0; i < person->cCount; i++) {
      if((final[person->connections[i]] == 0) & (randsL[i] < (beta*lrate)) & (person->con_type[i] == 2)) {
	//If verbose, print out the index of who was infected
	if(edit == 0) {
	  edit = 1;
	}
	
	if((listed == 0) & (verbose == 1)) {
	  fprintf(vPtr, "%d,", person->number);
	  listed = 1;
	}
	if (verbose == 1) {
	  fprintf(vPtr, "%d,%d,", person->connections[i],person->con_type[i]);
	}

	//Actually infect the person
	final[person->connections[i]] = 1;
      }
    }
  }

  if(listed == 1) {
    fprintf(vPtr, "\n");
  }
}

void test(pPerson person, double test, int* final) {
  for(int i = 0; i < person->cCount; i++) {
    double rands = rand() / ((double) RAND_MAX);
    if(rands < test) {
      if((final[person->connections[i]] == 3 ) | (final[person->connections[i] == 4]) | (final[person->connections[i] == 2])) {
	final[person->connections[i]] = 6;
	TI = TI+1;
      }
    }
  }
}

void event(pPerson* network, int size, int amount, double beta) {
  int randsL[amount];
  for(int i = 0; i < amount; i++) {
    randsL[i] = (double)rand() / (double)size;
  }

  int listed = 0;

  //Check if someone is infected
  for (int i = 0; i < amount; i++) {
    int check = network[randsL[i]]->status;

    //If infected, see if infects others at event
    if((check == 1) | (check == 2) | (check == 3)) {
      double randsI[amount];
      
      //Set up randoms
      for(int k = 0; k < amount; k++) {
	randsI[k] = rand() / ((double) RAND_MAX);
      }

      //Infect people
      for(int j = 0; j < amount; j++) {
	if((j != i) & (network[randsL[j]]->status == 0)) {
	  if(randsI[j] < beta) {
	    if(edit == 0) {
	      edit = 1;
	    }
	    
	    if((listed == 0) & (verbose == 1)) {
	      fprintf(vPtr, "%d,", network[randsL[i]]->number);
	      listed = 1;
	    }
	    
	    if (verbose == 1) {
	      fprintf(vPtr, "%d,%d,", network[randsL[j]]->number, 3);
	    }
	    network[randsL[j]]->status = 1;
	  }
	}
      }
    }
  }
  if(listed == 1) {
    fprintf(vPtr, "\n");
  }
}

void fPrintNetworkStatus(pPerson* network, int size, FILE* fPtr, int run) {
  int status_count[8] = {0};
  for(int i = 0; i < size; i++) {
    status_count[network[i]->status] += 1;
  }
  if(status_count[0] < size-1) {
    fprintf(fPtr, "%d, %d, %d, %d, %d, %d, %d, %d, %d, %d", run, status_count[0], status_count[1],
	    status_count[2], status_count[3], status_count[4], status_count[5],
	    status_count[6], status_count[7], size - status_count[0]);
  }
}

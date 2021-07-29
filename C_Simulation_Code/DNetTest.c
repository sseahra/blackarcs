#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include "DNet.h"
#include <time.h>
#include <sys/stat.h>

int main(int argc, char** argv) {
  printf("Starting Network Generation.\n");
  mkdir("Simulation_Results", 0777);
  system("exec rm -r Simulation_Results/*");

  /*Variables
  int size;
  double r0 = 3.1;
  double disease_length = 14;
  double people_total = 12 * disease_length;
  int days = 30;
  int runs = 1000;
  double infect_chance = r0 / people_total;
  double alpha = (1 - pow(0.5,(1/4.3)));
  double gamma = (1 - pow(0.5,(1/5.8)));
  double asymp_chance = 0.20;
  double asymp_rate = 1;
  double symp_test = 0.1;
  double rand_test = 0;
  double test_success = 1.0;
  double pre_time = 2.0;
  */

  //Do we get data from a file? Let's find out
  int length = 11;
  double data[length];
  for(int i = 0; i < length; i++) {
    data[i] = 0;
  }
  int size;
  int runs;
  int verbose;
  //double data[11] = {days, infect_chance, alpha, gamma, asymp_chance, asymp_rate, symp_test, rand_test, test_success, pre_time, event_time};
  if(argc >= 2) {
    FILE* fp = fopen(argv[1], "r");
    if(fp==NULL) {
      printf("Failed to open file.\n");
      return(0);
    }

    //Base data - Line 1
    fscanf(fp, "%d", &size);
    int comma = fgetc(fp);
    fscanf(fp, "%d", &runs);
    comma = fgetc(fp);
    fscanf(fp, "%d", &verbose);

    //Other Data - Line 2
    comma = fgetc(fp);
    if(comma != '\n') {
      comma = fgetc(fp);
    }
    int i = 0;
    while((i < length)) {
      fscanf(fp, "%lf", &data[i]);
      comma = fgetc(fp);
      i++;
    }

    fclose(fp);
  }
  else{
    size = 20000;
    runs = 100;

    double r0 = 3.1;
    double disease_length = 14;
    double people_total = 12 * disease_length;
    int days = 30;
    double infect_chance = r0 / people_total;
    double alpha = (1 - pow(0.5,(1/4.3)));
    double gamma = (1 - pow(0.5,(1/5.8)));
    double asymp_chance = 0.20;
    double asymp_rate = 1;
    double symp_test = 0.1;
    double rand_test = 0;
    double test_success = 1.0;
    double pre_time = 2.0;
    double event_time = 7.0;

    data[0] = days;
    data[1] = infect_chance;
    data[2] = alpha;
    data[3] = gamma;
    data[4] = asymp_chance;
    data[5] = asymp_rate;
    data[6] = symp_test;
    data[7] = rand_test;
    data[8] = test_success;
    data[9] = pre_time;
    data[10] = event_time;
  }
  
  //int connections = 12;
  String fName = "Simulation_Results/0Net_Test.txt";

  clock_t start, end;
  double cpu_time_used;
  start = clock();
  pPerson* test = createNetwork(size);
  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

  printNetwork(test, size, fName);
  
  printf("Network Generation Time: %f\n",cpu_time_used);


  //Simulation
  start = clock();
  if(verbose == 1) {
    printf("VERBOSE ACTIVATED\nV - Daily Infection Format: day,infectious,infected,relationship,infected,relationship,...\nV - New line for new infectious person, 0 for yellow only, 1 for orange or yellow, 2 for all phases, 3 for event.\n");
  }
  printf("Starting Simulation.\n");
  simulate(test, size, runs, "Simulation_Results/0Sim_Test.txt", data, verbose);
  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

  printf("Simulation Time: %f seconds\n",cpu_time_used);
  printf("Average of %f seconds per Simulation\n", cpu_time_used/runs);
  printf("Results saved to 'Sim_Test.txt'\n");
  
  freeNetwork(test, size);
  return(0);
}

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include "NetworkGen.h"

pPerson mallocPerson() {
  pPerson pThis = (pPerson) malloc (sizeof(Person));

  if(pThis == (pPerson) NULL) {
    return pThis;
  }

  pThis->number = 0;
  pThis->age = 1;
  pThis->cCount = 0;
  pThis->status = 0;
  pThis->counter = 0;
  for(int i = 0; i < MAX_C; i++) {
    pThis->connections[i] = -1;
    pThis->con_type[i] = -1;
    pThis->times[i]=1;
  }

  return(pThis);
}

void copyPerson(pPerson dest, pPerson src) {
  dest->number = src->number;
  dest->age = src->age;
  dest->cCount = src->cCount;
  dest->status = src->status;
  dest->counter = src->counter;
  for(int i = 0; i < src->cCount; i++) {
    dest->connections[i] = src->connections[i];
    dest->con_type[i] = src->con_type[i];
  }
}


pPerson* mallocNetwork(int size) {
  pPerson *network = malloc(sizeof(pPerson)*size);
  for(int i = 0; i < size; i++) {
    network[i] = mallocPerson();
    network[i]->number = i;
  }

  return(network);
}

pPerson* createNetwork(int size) {
  //printf("-----Generating Networks-----\n");
  pPerson *baseNet = mallocNetwork(size);

  time_t t;
  srand((unsigned) time(&t));

  //Build 2 networks and join them for making care homes
  int* wNet = (int*) calloc(size,sizeof(int));
  for(int i = 0; i < size; i++) {
    wNet[i] = i;
  }
  shuffle(wNet, size);
  int* hNet = (int*) calloc(size,sizeof(int));
  memcpy(hNet, wNet, size*sizeof(int));
  
  int *wpos = (int*) calloc(1,sizeof(int));
  int *hpos = (int*) calloc(1,sizeof(int));

  //Generate the care homes
  genCareHomes(baseNet, size, wNet, wpos, hpos);

  //Disjoint the networks for future work on them
  int sizeH = size - *hpos;
  int* hNew = (int*) calloc(sizeH,sizeof(int));
  int i = 0;
  for(i = 0; i < (size - *hpos); i++) {
    hNew[i] = hNet[(i+*hpos)];
  }
  shuffle(hNew, sizeH);
  *hpos = 0;

  //Now we can build households. For this, we will temporarily make a list of ints for children:
  int childlist[size];
  int *cpos = (int*) calloc(1,sizeof(int));
  int sizeW = size - (*wpos);

  genHouseHolds(baseNet, sizeH, hNew, hpos, childlist, cpos);
  genSchools(baseNet, *cpos, childlist);
  genWork(baseNet, sizeW, wNet, wpos);
  genFriends(baseNet, size);

  //printf("-----Completed Network Generation-----\n");
  free(wNet);
  free(hNet);
  free(wpos);
  free(hpos);
  free(hNew);
  free(cpos);
  return(baseNet);
}
pPerson* importNetwork(char* file) {
  //printf("-----Importing Network-----\n");
  FILE* fp = fopen(file, "r");
  if(fp==NULL) {
    printf("Failed to open file.\n");
    return(0);
  }
  int size;
  char line[100];
  char* token;
  fgets(line, 100, fp);
  size = atoi(line);
  pPerson *baseNet = mallocNetwork(size);

  int start;
  int end;
  int time;
  char delim[] = " ,.";
  while(fgets(line, 100, fp) != NULL) {
    token = strtok(line, delim);
    start = atoi(token)-1;
    token = strtok(NULL, delim);
    end  = atoi(token)-1;
    token = strtok(NULL, delim);
    time = atoi(token);
    if(time != 0) {
      addConnection(baseNet, start, end, 0, time);
    }
  }
  //printf("-----Completed Network Importing-----\n");
  fclose(fp);
  return(baseNet);
}

void addConnection(pPerson* base, int i, int j, int type, int time) {
  base[i]->connections[base[i]->cCount] = j;
  base[i]->con_type[base[i]->cCount] = type;
  base[i]->times[base[i]->cCount] = time;
  base[i]->cCount += 1;
}

void copyNetwork(pPerson* dest, pPerson* source, int size) {
  for(int i = 0; i < size; i++) {
    copyPerson(dest[i], source[i]);
  }
}

pPerson* createCopy(pPerson* src, int size) {
  pPerson* dest = mallocNetwork(size);
  copyNetwork(dest, src, size);
  return dest;
}

void printNetwork(pPerson* network, int size, String file) {
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
  for(int i = 0; i < size; i++) {
    fprintf(fPtr, "Person %d: ", network[i]->number+1);
    fprintPerson(network[i], fPtr);
    fprintf(fPtr, "\n");
  }

  fclose(fPtr);
}

void fprintPerson(pPerson person, FILE *file) {
  fprintf(file, "Age - %d, Connection Count - %d\n Connections - ", person->age, person->cCount);
  for(int i = 0; i < person->cCount; i++) {
    fprintf(file, "%d (%d)\t", person->connections[i],person->con_type[i]);
  }
}

void genCareHomes(pPerson *base, int size, int* wNet, int* wP, int* hP) {
  //Care Home Source:https://www.nbanh.com/
  //68 homes, 4700 residents, ~69 people / home
  //68 homes / 776827 (NB Pop) = 0.00008753557
  //6000 employees, in Ontario 58% are personal workers
  //(https://www.ontario.ca/page/long-term-care-staffing-study)
  //Therefore, 6000 * 0.58 / 68 = 51 employees
  double hPerCap = 0.00008753557;
  int care_homes = ceil(hPerCap * size);
  //int start = *wP;
  
  if(size < 500) {
    care_homes = 0;
  }
  int count = 0;
  //printf("-----Generating Care Homes-----\n");
  while(count < care_homes) {
    int num = 69;
    int employees = 51;

    for(int i = *wP; i < *wP+num+employees; i++) {
      for(int j = i+1; j < *wP+num+employees; j++) {
	base[wNet[i]]->connections[base[wNet[i]]->cCount] = wNet[j];
	base[wNet[i]]->cCount += 1;
      }
    }

    for(int i = *wP; i < *wP+num; i++) {
      base[wNet[i]]->age = 2;
    }

    for(int i = *wP+num; i < *wP+num+employees; i++) {
      base[wNet[i]]->age = 1;
    }

    *hP = *hP + num;
    *wP = *wP + num + employees;
    count += 1;
  }
  //int end = *wP;
  //printf("-----Completed Care Homes-----\n");
}

void genHouseHolds(pPerson *base, int size, int* hNew, int* hp, int* childlist, int* cpos) {
  //printf("-----Generating Households-----\n");
  //For people remaining in the house list, give an age while assigning a house
  //Age Source: https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/Page.cfm?Lang=E&Geo1=PR&Code1=13&Geo2=&Code2=&Data=Count&SearchText=New%20Brunswick&SearchType=Begins&SearchPR=01&B1=All&GeoLevel=PR&GeoCode=13
  //14.79% Young,  65.29% Adult, 19.9% Old    
        
  //For household data
  //Source https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/Page.cfm?Lang=E&Geo1=PR&Code1=13&Geo2=&Code2=&Data=Count&SearchText=New%20Brunswick&SearchType=Begins&SearchPR=01&B1=All&GeoLevel=PR&GeoCode=13
  //Average Household Size = 2.3

  //Now we use up the WHOLE house list for ages and households
  while (*hp < size) {

    //Get a random size for the household, mean 2.3, sd 0.5
    int numG = round(normalRandom()*0.5+2.3);

    //Ensure the size fits and is reasonable
    if(*hp + numG > size) {
      numG = size - *hp;
    }
    else if (numG < 1) {
      numG = 1;
    }
    else if(numG > 8) {
      numG = 8;
    }

    //For each person, do connections
    for(int i = *hp; i < *hp+numG; i++) {

      //Create the connection, set connection type to Y/O/R
      for(int j = i; j < *hp+numG; j++) {
	base[hNew[i]]->connections[base[hNew[i]]->cCount] = hNew[j];
	base[hNew[i]]->con_type[base[hNew[i]]->cCount] = 0;
	base[hNew[i]]->cCount += 1;
      }

      //Now give a random age to the person
      double temp = rand() / ((double) RAND_MAX);

      //If they are young, they get added to the dreaded school list
      if(temp < 0.1479) {
	base[hNew[i]]->age = 0;
	*(childlist + (*cpos)) = hNew[i];
	*cpos += 1;
      }

      //If they are old, cool, we will deal with that later
      else if(temp > 0.8008) {
	base[hNew[i]]->age = 2;
      }

      //Otherwise they are an adult
      else {
	base[hNew[i]]->age = 1;
      }
    }

    //Make sure the pointer to the houselist is added up correctly
    *hp += numG;
  }
  //printf("-----Completed Households-----\n");
}

void genSchools(pPerson *base, int size, int* childlist) {
  //printf("-----Generating Schools-----\n");
  //We will use an average class size of 22

  //Get a random size for the school, mean 22, sd 5
  int numG = round(normalRandom()*5+22);
  int pos = 0;

  while(pos < size) {
    //Ensure the size fits and is reasonable
    if(pos + numG > size) {
      numG = size - pos;
    }
    else if (numG < 10) {
      numG = 10;
    }
    else if(numG > 50) {
      numG = 50;
    }    

    //For each student, make the usual connections
    for(int i = pos; i < pos+numG; i++) {

      //Make the connections for each student, set connection type to "Y/O"
      for(int j = i; j < pos+numG; j++) {
	base[childlist[i]]->connections[base[childlist[i]]->cCount] = childlist[j];
	base[childlist[i]]->con_type[base[childlist[i]]->cCount] = 1;
	base[childlist[i]]->cCount += 1;
      }
    }
    pos += numG;
  }
  //printf("-----Completed Schools-----\n");
}

void genWork(pPerson *base, int size, int* wNet, int* wp) {
  //printf("-----Generating Workplaces-----\n");
  //Work data, this is more common sense than anything, note, though, that 20% of seniors still work
  //Source: https://www12.statcan.gc.ca/census-recensement/2016/as-sa/98-200-x/2016027/98-200-x2016027-eng.cfm
  while (*wp < size) {
     
    //Get a random size for the household, mean 9.7, sd 2
    int numG = round(normalRandom()*2+9.7);

    //Ensure the size fits and is reasonable
    if(*wp + numG > size) {
      numG = size - *wp;
    }
    else if (numG < 2) {
      numG = 2;
    }
    else if(numG > 50) {
      numG = 50;
    }

    
    int arr[numG];
    int pos = 0;
    int num = (*wp)+numG;
    for(int i = *wp; i < num; i++) {
      if(i >= size) {
	break;
      }
      if(base[wNet[i]]->age != 3) {
	arr[pos] = wNet[i];
	pos += 1;
      }
    }

    //For each person, do connections
    for(int i = 0; i < pos; i++) {
      //Create the connection, set type to Y/O
      for(int j = i; j < pos; j++) {
	base[arr[i]]->connections[base[arr[i]]->cCount] = arr[j];
	base[arr[i]]->con_type[base[arr[i]]->cCount] = 1;
	base[arr[i]]->cCount += 1;
      }

      //Make sure the pointer to the worklist is added up correctly
    }
    *wp += numG;
  }
  //printf("-----Completed Workplaces-----\n");
}

void genFriends(pPerson *base, int size) {
  //printf("-----Generating Friends-----\n");
  for (int i = 0; i < size; i++) {
    int num1 = rand() % size;
    while(num1 == i) {
      num1 = rand() % size;
    }
    int num2 = rand() % size;
    while(num2 == i) {
      num2 = rand() % size;
    }

    //Create first connection, set type to R
    base[i]->connections[base[i]->cCount] = num1;
    base[i]->con_type[base[i]->cCount] = 2;
    base[i]->cCount += 1;
    //Create second connection, set type to R
    base[i]->connections[base[i]->cCount] = num2;
    base[i]->con_type[base[i]->cCount] = 2;
    base[i]->cCount += 1;
  }
  //printf("-----Completed Friends-----\n");
}
  
double normalRandom() {
  double v1 = ( (double)(rand()) + 1. )/( (double)(RAND_MAX) + 1. );
  double v2 = ( (double)(rand()) + 1. )/( (double)(RAND_MAX) + 1. );
  return(cos(2*3.14*v2)*sqrt(-2.*log(v1)));
}

void shuffle(int *arr, int size) { 
    for (int i = size-1; i > 0; i--) { 
        // Pick a random index from 0 to i 
        int j = rand() % (i+1);
        // Swap arr[i] with the element at random index 
        swap(&arr[i], &arr[j]); 
    } 
}

void swap(int* a, int* b) {
  int temp = *a;
  *a = *b;
  *b = temp;
}

void freePerson(pPerson input) {
  free(input);
}

void freeNetwork(pPerson* input, int size) {
  for(int i = 0; i < size; i++) {
    freePerson(input[i]);
  }
  free(input);
}

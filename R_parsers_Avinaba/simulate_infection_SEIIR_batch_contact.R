# Start the clock!
ptm <- proc.time()

library(dplyr) # for dataframe utitlities
library(iterators)


SIM_SCRIPT_NAME = 'simulate_infection_SEIIR_batch_contact'

# input files 

# contact matrix (pairlist)
CONTACT_MATRIX_DIR <- 'scenarioAvin_0_to_30_control'

LIST_OF_CONTACT_MATRIX_FILEPATH  <- 
  list.files(CONTACT_MATRIX_DIR, full.names = TRUE, pattern="contact\\_matrix\\-total\\-real\\.csv$", ignore.case = TRUE)



# population metadata 
# POPULATION_METADATA_CITISKETCH_JSON = 'scenarioBenchmark/demographicInfo-Campbellton.json'


# output directory
MAIN_DIR = paste(SIM_SCRIPT_NAME, "_output", sep = "")
dir.create(file.path(MAIN_DIR))

SUB_DIR = CONTACT_MATRIX_DIR
dir.create(file.path(MAIN_DIR, SUB_DIR))


OUTPUT_DIRECTORY <- paste(MAIN_DIR,  "/", 
                          SUB_DIR, "/", 
                          sep = "")



# Modelling modified SEIR by reducing states from Dr. Seahra's model
# Disease trajectory 
# 
# susceptible: susciptible
# exposed: latent_infections_not_isolated 
# infected1: pre_symptomatic_non_isolated  
# infected2: mild_non_isolated
# removed: *all*_isolated, hospitalized, hospitalized_ICU, recovered, dead


library(igraph)

{
# Infection states for 0 doses: 
  disease_model <- graph( edges = c("susceptible","exposed", 
                                    "susceptible","removed",
                                    "exposed", "infected1", 
                                    "infected1", "infected2", 
                                    "infected1", "removed", 
                                    "infected2", "removed"), directed = TRUE)

# For people who are not vaccinated: Infection transition/day parameters 
CONSTANT_REMOVAL_RATE <- 0.1 # gamma

# Parameters from Dr. Seahra's model
CONST_q <- 0.07          # fraction of all cases that isolate early
CONST_p <- 0.27          # fraction of non-hospitalized cases that isolate late

CONST_CHI <- 2/5         # inverse of mean length of infected but not infectious (latent) period
CONST_ita <- 1/5         # inverse of mean length of of asymptomatic/mild symptomatic period
CONST_KAPPA <- 2/5       # inverse of mean length of pre-symptomatic period

CONST_v_m <- 0.960       # fraction of cases never hospitalized (-> mild)
CONST_v_c <- 0.006       # fraction of cases hospitalized in ICU for part of stay 
CONST_v_h <- 0.034       # fraction of cases hospitalized but never in ICU



E(disease_model, path = c("exposed", "infected1") )$weight <- CONST_CHI # rate of change per day

E(disease_model, path = c("infected1", "infected2") )$weight <- CONST_v_m * (1 - CONST_p) * CONST_KAPPA
E(disease_model, path = c("infected1", "removed") )$weight <- CONST_v_m * CONST_p * CONST_KAPPA   +   CONST_v_h * CONST_KAPPA   +   CONST_v_c * CONST_KAPPA
                                                              
E(disease_model, path = c("infected2", "removed") )$weight <- CONST_ita

# Adding weight value to edge labels for visual inspection
E(disease_model)$label <- paste(E(disease_model)$weight, "/day")


# Adding special transitions 
E(disease_model, path = c("susceptible","exposed") )$weight <- "CALCULATE_FROM_CONTACT_MATRIX" # with a probability of  1 - q 
E(disease_model, path = c("susceptible","exposed") )$label <- paste0("if infected, then (1 - q) = ", (1 - CONST_q))

E(disease_model, path = c("susceptible","removed") )$weight <- "CALCULATE_FROM_CONTACT_MATRIX" # with a probability of q
E(disease_model, path = c("susceptible","removed") )$label <- paste0("if infected, then q = ", CONST_q)



}

plot(disease_model, layout=layout_as_tree)

tkplot(disease_model, 
       vertex.color="#77c98d", edge.arrow.size=0.75)

# Debug output graph 
E(disease_model)
edge_attr(disease_model)

# Generate transition matrix 
transition_matrix <- as_adjacency_matrix(disease_model, attr="weight", sparse = FALSE)

# generate list with indices 
# transition_list <- which(transition_matrix != "", arr.ind = TRUE, useNames = FALSE)



# Infection probability function <- contact time
getInfection_probability <- function(contact_time) {
  
  TIME_TILL_76p_CHANCE <- 20 * 60 # seconds (20 minutes)
  MAX_CHANCE <- 0.045 # About 3.4% chance by 20 minutes, to max 4.5% by Inf
  
  # TODO: Add a minimum cutoff time like Dr. Watmough suggested
  # Note: Since I am adding prob from each contact as of now, 
  #        must change the algorithm first @"Tag: Summation of probabilities"
  
  return (MAX_CHANCE * tanh(contact_time / TIME_TILL_76p_CHANCE))
  
}




# Memory Expensive approach to reduce total I/O
#
# Reading all the matrices in a list
# 

list_of_contact_matrix <- list()
df_index <- 1


for(CONTACT_MATRIX_AS_PAIRLIST_FILENAME in LIST_OF_CONTACT_MATRIX_FILEPATH){
  # Debug: 
  # cat(CONTACT_MATRIX_AS_PAIRLIST_FILENAME, "\n")
  
  # Read contact matrix 
  contact_matrix_as_pairlist <- read.table(CONTACT_MATRIX_AS_PAIRLIST_FILENAME, sep=",")
  
  # Adding readable column names 
  colnames(contact_matrix_as_pairlist) <- c("person_id_1", "person_id_2", "contact_in_seconds")
  
  # Converting to dataframe 
  contact_matrix_as_pairlist <- data.frame(contact_matrix_as_pairlist)
  
  contact_matrix_as_pairlist$infection_prob <- getInfection_probability(contact_matrix_as_pairlist$contact_in_seconds)
  
  list_of_contact_matrix[[df_index]] <- contact_matrix_as_pairlist
  df_index <- df_index + 1
}

COUNT_CONTACT_MATRIX <- length(list_of_contact_matrix)


# Create iterator that works till end of list
contact_matrix_iterator <- iter(list_of_contact_matrix)

# Get first contact matrix
contact_matrix_as_pairlist <- nextElem(contact_matrix_iterator)

# Simulation assumption: 
# Repeating the condition of last available contact matrix 
# if no. of days to simulate > available contact matrices
#
# This may be useful to simulate the simplest (mobility) scenarios with 
# two phases of normal movement with a restrictive phase in between 
#
#                                  .-> last contact info
#  |----COUNT_CONTACT_MATRIX------>|
#  +----TOTAL_SIMULATION_DAYS------------------- - - - >


# Initialize state 
TOTAL_STATES <- length(colnames(transition_matrix))
TOTAL_SIMULATION_DAYS <- 30
TOTAL_SIMULATED_PERSONS <- max(contact_matrix_as_pairlist[1:2])

STATE <- array(rep(NA, TOTAL_SIMULATED_PERSONS * TOTAL_SIMULATION_DAYS), 
               dim = c(TOTAL_SIMULATED_PERSONS, TOTAL_SIMULATION_DAYS) )


# Set up simulation 

# Set every person to susceptible for the first day
STATE[ , 1] <- which( colnames(transition_matrix) == "susceptible" )


STATE_NAMES = colnames(transition_matrix)

STATELIST_INFECTIOUS <- which(colnames(transition_matrix) == "infected1" | 
                                colnames(transition_matrix) == "infected2")

COUNT_FIRST_DAY_DISEASE_IMPORT <- 5 


# Randomly distribute the infection import
first_disease_import <- sample(seq_len(length(STATE[ , 1])), COUNT_FIRST_DAY_DISEASE_IMPORT)

cat("Setting first day disease import to: ", COUNT_FIRST_DAY_DISEASE_IMPORT, "\n")
for(person_id in first_disease_import){
  
  STATE[person_id , 1] <- sample(STATELIST_INFECTIOUS, 1)

  # Debug
  cat("person id - infected: ", person_id, "\n")
  
}

# To keep track of infection chain
# INFECTION_HISTORY <- data.frame(matrix(NA, ncol = 3, nrow = TOTAL_SIMULATED_PERSONS))

# Adding readable column names 
# colnames(INFECTION_HISTORY) <- c("target_person", "infector", "day_index")

# Initialize all target_person 
# INFECTION_HISTORY$target_person <- seq(1:TOTAL_SIMULATED_PERSONS)


# Simulate from day 2 to TOTAL_SIMULATION_DAYS
for(day_index in 2:TOTAL_SIMULATION_DAYS){
  
  # Slice yesterday's data
  previous_day_df <- data.frame(STATE[ , day_index - 1])
  colnames(previous_day_df) <- c("state_id")
  
  previous_day_df$person_id <- seq.int(nrow(previous_day_df))
  
  # Note: this only works as "infected" is a unique id
  previous_day_infectious_df <- filter(previous_day_df, state_id %in% STATELIST_INFECTIOUS)
  
  # previous_day_exposed_df <- filter(previous_day_df, state_id %in% STATELIST_EXPOSED)
  
  # previous_day_susciptible_df <- filter(previous_day_df, state_id %in% STATELIST_SUSCIPTIBLE)
  
  
  # colnames(previous_day_df) <- c("person_id_1", "state id")
  
  for(person_id in 1:TOTAL_SIMULATED_PERSONS){
    
    # Get state of this person from previous day
    PREV_STATE_INDEX <- STATE[person_id, day_index - 1]
    PREV_STATE_NAME <- STATE_NAMES[PREV_STATE_INDEX]
    # cat("Person no: ", person_id, ", state id: ", PREV_STATE_INDEX, ", state name: ", PREV_STATE_NAME, "\n")
    
    # Get possible transitions 
    POSSIBLE_TRANSITIONS <- which(transition_matrix[PREV_STATE_INDEX, ] != "")
    
    
    POSSIBLE_TRANSITIONS <- data.frame(POSSIBLE_TRANSITIONS)
    colnames(POSSIBLE_TRANSITIONS) <- c("state_id")
    POSSIBLE_TRANSITIONS$transition_weights <- transition_matrix[PREV_STATE_INDEX, which(transition_matrix[PREV_STATE_INDEX, ] != "")]
    
    # Get weights 
    # Check which of the transition weights are CALCULATE_FROM_CONTACT_MATRIX
    # Debug: 
    # print(POSSIBLE_TRANSITIONS)
    
    NO_OF_CONTACT_MATRIX_BASED_TRANSITION <- length(which(POSSIBLE_TRANSITIONS$transition_weights == "CALCULATE_FROM_CONTACT_MATRIX"))
    

    if(NO_OF_CONTACT_MATRIX_BASED_TRANSITION > 0){
      contacts_df <- filter(contact_matrix_as_pairlist, person_id_1 == person_id)
      # cat("Contacts of ", person_id)
      # print(contacts_df$person_id_2)
      
      # Check if person 2 was infectious on day_index - 1
      infectious_contacts_df <- filter(contacts_df, person_id_2 %in% previous_day_infectious_df$person_id)
      # cat("Infectious ontacts of ", person_id)
      # print(infectious_contacts_df$person_id_2)
      
      # Tag: Summation of probabilities 
      # TOTAL_PROBABILITY_OF_INFECTION <- sum(infectious_contacts_df$infection_prob)
      COUNT_OF_POSSIBLE_INFECTIOUS_CONTACTS <- nrow(infectious_contacts_df)
      next_state_id <- PREV_STATE_INDEX
      
      if(COUNT_OF_POSSIBLE_INFECTIOUS_CONTACTS > 0) {
        
        # Perform each Coin toss and check for possible infection till first occurs
        ALREADY_INFECTED <- FALSE
        for(contact_index in 1:COUNT_OF_POSSIBLE_INFECTIOUS_CONTACTS){
          
          if( ALREADY_INFECTED ) { break }
          
          # same as 
          # rbinom(n = 1, size = 1, prob = infectious_contacts_df[contact_index, ]$infection_prob) == TRUE
          if( runif(1) <= infectious_contacts_df[contact_index, ]$infection_prob ){
            
            # Infection event success
            next_state_id <- sample( c( which(STATE_NAMES == "exposed"), which(STATE_NAMES == "removed") ),
                                                             1,
                                                             prob = c((1 - CONST_q), CONST_q))
            ALREADY_INFECTED <- TRUE
            
            # Debug:
            # cat("Person ", infectious_contacts_df[contact_index, ]$person_id_2, " infected ", person_id, " on day no. ", day_index, "\n")
            
            # Book keeping 
            # INFECTION_HISTORY$infector[ which(INFECTION_HISTORY$target_person == person_id)  ] <- infectious_contacts_df[contact_index, ]$person_id_2
            # INFECTION_HISTORY$day_index[ which(INFECTION_HISTORY$target_person == person_id)  ] <- day_index
          }
        }
      }

      STATE[person_id, day_index] <- next_state_id
     
    }
    
    # TODO: Not looking out for transitions that may be a mix of both yet
    
    # Non-infectious, static parameter based transition
    else{
      
      # Check final state
      if(nrow(POSSIBLE_TRANSITIONS) == 0) {
        next_state_id <- PREV_STATE_INDEX
      }
      
      # Calculate possible transitions for non-final states
      else{
        
        # cat("Parametrised transitions for person: ", person_id, "in state: ", PREV_STATE_NAME, "\n")
        
        # There should not be any non numeric transition weights for this state
        # Coerce into double type 
        POSSIBLE_TRANSITIONS$transition_weights <- as.double(POSSIBLE_TRANSITIONS$transition_weights)
        
        STAY_IN_STATE_WEIGHT <- min(1.0, abs(1.0 - sum(POSSIBLE_TRANSITIONS$transition_weights)))
        
        # print(POSSIBLE_TRANSITIONS)
        
        # Create temp loop-back state 
        complete_state_ids <- append(POSSIBLE_TRANSITIONS$state_id, PREV_STATE_INDEX)
        complete_state_weights <- append(POSSIBLE_TRANSITIONS$transition_weights, STAY_IN_STATE_WEIGHT)
        
        # Choose a state w.r.t weight
        next_state_id <- sample(complete_state_ids, 1, prob = complete_state_weights)
      }
      
      # Set state 
      STATE[person_id, day_index] <- next_state_id
      
      # Debug: 
      # if(PREV_STATE_INDEX != next_state_id){
        # cat("Changed Person id: ", person_id, ":  ", STATE_NAMES[PREV_STATE_INDEX], "to ", STATE_NAMES[next_state_id], "\n")
      # }
      
      
    }
    
  }
  
  # if there's another contact matrix available, then iterate
  # Generates "Error: StopIteration" when reaches end
  contact_matrix_as_pairlist <- nextElem(contact_matrix_iterator)
}

# setting Time as unique run identifier
UUID <- Sys.time()

# set output path
OUTPUT_STATE_FILEPATH_CSV <- paste(OUTPUT_DIRECTORY, UUID, "-STATE.csv",  sep = "")

# write all state transitions as CSV 
write.table(STATE, sep=",", OUTPUT_STATE_FILEPATH_CSV, row.names = FALSE, col.names = FALSE)


# Human readable names 
# set output path
OUTPUT_STATENAMES_FILEPATH_CSV <- paste(OUTPUT_DIRECTORY, UUID, "-STATENAME.csv",  sep = "")

STATE_NAMES <- data.frame(STATE_NAMES)
STATE_NAMES$state_id <- seq.int(nrow(STATE_NAMES))

# write all state_id -> state name mapping as CSV 
write.table(STATE_NAMES, sep=",", OUTPUT_STATENAMES_FILEPATH_CSV, row.names = FALSE, col.names = FALSE)


# set output path
# OUTPUT_INFECTORS_FILEPATH_CSV <- paste(OUTPUT_DIRECTORY, UUID, "-INFECTORS.csv",  sep = "")

# write all state transitions as CSV 
# write.table(INFECTION_HISTORY, sep=",", OUTPUT_INFECTORS_FILEPATH_CSV, row.names = FALSE, col.names = FALSE)


# Stop the clock
cat(proc.time() - ptm)
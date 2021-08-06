# Start the clock!
ptm <- proc.time()

library(dplyr) # for dataframe utitlities
library(iterators)


SIM_SCRIPT_NAME = 'simulate_infection_SIR_batch_contact'

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



# Modelling SIR
# Disease trajectory 
# 
# susceptible 
# infected
# removed


library(igraph)

{
# Infection states for 0 doses: 
  disease_model <- graph( edges = c("susceptible","infected",
                                    "infected", "removed"), directed = TRUE)

# For people who are not vaccinated: Infection transition/day parameters 
CONSTANT_REMOVAL_RATE <- 0.1 # gamma

E(disease_model, path = c("susceptible","infected") )$weight <- "CALCULATE_FROM_CONTACT_MATRIX"
E(disease_model, path = c("infected", "removed") )$weight <- CONSTANT_REMOVAL_RATE

# Adding weight value to edge labels for visual inspection
E(disease_model)$label <- paste(E(disease_model)$weight, "/day")

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
  
  TIME_TILL_76p_CHANCE <- 1200 # seconds (20 minutes)
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

COUNT_FIRST_DAY_DISEASE_IMPORT <- 5 

# Randomly distribute the infection import
first_disease_import <- sample(seq_len(length(STATE[ , 1])), COUNT_FIRST_DAY_DISEASE_IMPORT)

cat("Setting first day disease import to: ", COUNT_FIRST_DAY_DISEASE_IMPORT, "\n")
for(person_id in first_disease_import){
  
  # Note: this only works as "infected" is a unique id
  STATE[person_id , 1] <- which(STATE_NAMES == "infected")

  # Debug
  cat("person id - infected: ", person_id, "\n")
  
}



# Simulate from day 2 to TOTAL_SIMULATION_DAYS
for(day_index in 2:TOTAL_SIMULATION_DAYS){
  
  # Slice yesterday's data
  previous_day_df <- data.frame(STATE[ , day_index - 1])
  colnames(previous_day_df) <- c("state_id")
  
  previous_day_df$person_id <- seq.int(nrow(previous_day_df))
  
  # Note: this only works as "infected" is a unique id
  previous_day_infectious_df <- filter(previous_day_df, state_id == which(STATE_NAMES == "infected"))
  
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
    
    # Get cumulative probability of person_id to be infected by 
    # any of it's neighbour (another person with time,space overlap)
    
    TOTAL_PROBABILITY_OF_INFECTION <- 0
    if(NO_OF_CONTACT_MATRIX_BASED_TRANSITION > 0){
      contacts_df <- filter(contact_matrix_as_pairlist, person_id_1 == person_id)
      # cat("Contacts of ", person_id)
      # print(contacts_df$person_id_2)
      
      # Check if person 2 was infectious on day_index - 1
      infectious_contacts_df <- filter(contacts_df, person_id_2 %in% previous_day_infectious_df$person_id)
      # cat("Infectious ontacts of ", person_id)
      # print(infectious_contacts_df$person_id_2)
      
      # Tag: Summation of probabilities 
      TOTAL_PROBABILITY_OF_INFECTION <- sum(infectious_contacts_df$infection_prob)
      next_state_id <- PREV_STATE_INDEX
      
      # (Sum of) Coin tosses
      if(TOTAL_PROBABILITY_OF_INFECTION > 0){
        # cat("Chance of infection with: ", TOTAL_PROBABILITY_OF_INFECTION)
        if(runif(1) <= TOTAL_PROBABILITY_OF_INFECTION){
          
          next_state_id <- sample(POSSIBLE_TRANSITIONS$state_id, 1) # Assign a random state from all possible starting infection state
          # cat(" Infected person: ",  person_id, ", to ", STATE_NAMES[next_state_id], "\n")
        } 
        # else{
        #   cat("\n")
        # }
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

# Stop the clock
cat(proc.time() - ptm)
# Start the clock!
ptm <- proc.time()

library(dplyr) # for dataframe utitlities
library(iterators)


SIM_SCRIPT_NAME = 'simulate_infection_01_batch_contact_for_60_days'

#SIM_SCRIPT_NAME = 'simulate_infection_01_batch_contact'

# input files 

# contact matrix (pairlist)
CONTACT_MATRIX_DIR <- 'scenarioAvin_0_to_30_control'

LIST_OF_CONTACT_MATRIX_FILEPATH  <- 
  list.files(CONTACT_MATRIX_DIR, full.names = TRUE, pattern="contact\\_matrix\\-total\\-real\\.csv$", ignore.case = TRUE)



# population metadata 
# POPULATION_METADATA_CITISKETCH_JSON = 'scenarioBenchmark/demographicInfo-Campbellton.json'


# output directory
MAIN_DIR = paste(SIM_SCRIPT_NAME, "_output", sep = "")
dir.create(file.path(MAIN_DIR) , showWarnings = FALSE)

SUB_DIR = CONTACT_MATRIX_DIR
dir.create(file.path(MAIN_DIR, SUB_DIR) , showWarnings = FALSE)


OUTPUT_DIRECTORY <- paste(MAIN_DIR,  "/", 
                          SUB_DIR, "/", 
                          sep = "")



# Modelling Dr. Seahra's disease trajectory model
# 
# per-day probability to be input as parameters 



# Disease trajectory 
# 
# susceptible, received_dose_0, received_dose_1: may become infected (w.r.t contact_second with pre-symptomatic/ day)
# latent_infections: infected but not infectious (Exposed)
# pre_symptomatic: infected and infectious and showing symptoms (Infectious)
# recovered: (Removed)
# dead: (Removed)


# Vaccination trajectory 
#
# susceptible: (could interpret as received dose 0) : non infected, technically "latent_infected" people might opt for 1st dose
# |- received_dose_0: 
# |- received_dose_1: 
# '- received_dose_2: 


# Hospitalization trajectory 
# 
# pre_symptomatic: people having visible symptoms who might seek hospitalization and/or ICU
# |- mild: not seeking hospitalization 
# |- hospitalized: regular hospitalization 
# '- hospitalized_ICU: 


# Isolation state (living condition)
# - Relevant post infection to decide on transition probability
# 
# isolated:
# non-isolated: 

library(igraph)

# Vaccinations
disease_model <- graph( edges = c("susceptible","received_dose1",  
                                  "received_dose1","received_dose2"),directed = TRUE)

# TODO: Vaccination/day parameters 
# E(disease_model, path = c("susceptible", "received_dose1") )$weight <- 0.1
# E(disease_model, path = c("received_dose1", "received_dose2") )$weight <- 0.1

# Adding weight value to edge labels for visual inspection
E(disease_model)$label <- paste(E(disease_model)$weight, "/day")

plot(disease_model)


{
# Infection states for 0 doses: 
  disease_model <- disease_model+  
                   graph( edges = c("latent_infections_isolated", "pre_symptomatic_isolated", 
                                    # "latent_infections_isolated", "pre_symptomatic_non_isolated",
                                    
                                    # "latent_infections_not_isolated", "pre_symptomatic_isolated", 
                                    "latent_infections_not_isolated", "pre_symptomatic_non_isolated",
                                    
                                    "pre_symptomatic_isolated", "mild_isolated",
                                    # "pre_symptomatic_isolated", "mild_non_isolated",
                                    "pre_symptomatic_isolated", "hospitalized", 
                                    "pre_symptomatic_isolated", "hospitalized_ICU", 
                                    
                                    "pre_symptomatic_non_isolated", "mild_isolated",
                                    "pre_symptomatic_non_isolated", "mild_non_isolated",
                                    "pre_symptomatic_non_isolated", "hospitalized", 
                                    "pre_symptomatic_non_isolated", "hospitalized_ICU", 
                                    
                                    "mild_isolated", "recovered", 
                                    "mild_non_isolated", "recovered", 
                                    "hospitalized", "recovered", 
                                    "hospitalized_ICU", "recovered", 
                                    
                                    "hospitalized", "dead", 
                                    "hospitalized_ICU", "dead"), directed = TRUE)

# For people who are not vaccinated: Infection transition/day parameters 
# CONSTANT_NOT_VACCINATED <- 0.1 

# For people who are not vaccinated: Infection transition/day parameters 
  
CONST_CHI <- 2/5         # inverse of mean length of infected but not infectious (latent) period
CONST_KAPPA <- 2/5       # inverse of mean length of pre-symptomatic period
CONST_rho <- 1/10        # inverse of mean length of non-ICU based hospitalization 
CONST_sigma <- 1/10      # inverse of mean length of ICU based hospitalization 
CONST_ita <- 1/5         # inverse of mean length of of asymptomatic/mild symptomatic period

CONST_v_m <- 0.960       # fraction of cases never hospitalized (-> mild)
CONST_v_c <- 0.006       # fraction of cases hospitalized in ICU for part of stay 
CONST_v_h <- 0.034       # fraction of cases hospitalized but never in ICU

# CONST_v_m + CONST_v_c + CONST_v_h must be 1

CONST_f_c <- 1/2        # fraction of ICU hospitalization resulting in death 
CONST_f_h <- 1/10       # fraction of non-ICU hospitalization resulting in death

CONST_p <- 0.27         # fraction of non-hospitalized cases that isolate late
CONST_q <- 0.07         # fraction of all cases that isolate early 



# From Latent
E(disease_model, path = c("latent_infections_isolated", "pre_symptomatic_isolated") )$weight <- CONST_CHI
E(disease_model, path = c("latent_infections_isolated", "pre_symptomatic_isolated") )$label <- paste0("Chi = ", CONST_CHI)

# E(disease_model, path = c("latent_infections_isolated", "pre_symptomatic_non_isolated") )$weight <- 0
# E(disease_model, path = c("latent_infections_isolated", "pre_symptomatic_non_isolated") )$label <- "0"


# E(disease_model, path = c("latent_infections_not_isolated", "pre_symptomatic_isolated") )$weight <-  0
# E(disease_model, path = c("latent_infections_not_isolated", "pre_symptomatic_isolated") )$label <-  "0"

E(disease_model, path = c("latent_infections_not_isolated", "pre_symptomatic_non_isolated") )$weight <- CONST_CHI
E(disease_model, path = c("latent_infections_not_isolated", "pre_symptomatic_non_isolated") )$label <- paste0("Chi = ", CONST_CHI)




# From Pre-symptomatic
E(disease_model, path = c("pre_symptomatic_isolated", "mild_isolated") )$weight <- CONST_v_m * CONST_KAPPA
E(disease_model, path = c("pre_symptomatic_isolated", "mild_isolated") )$label <- paste0("v[m] * Kappa = ", CONST_v_m * CONST_KAPPA)

# E(disease_model, path = c("pre_symptomatic_isolated", "mild_non_isolated") )$weight <- 0
# E(disease_model, path = c("pre_symptomatic_isolated", "mild_non_isolated") )$label <- "0"

E(disease_model, path = c("pre_symptomatic_isolated", "hospitalized") )$weight <- CONST_v_h * CONST_KAPPA
E(disease_model, path = c("pre_symptomatic_isolated", "hospitalized") )$label <- paste0("v[h] * Kappa = ", CONST_v_h  * CONST_KAPPA)

E(disease_model, path = c("pre_symptomatic_isolated", "hospitalized_ICU") )$weight <- CONST_v_c * CONST_KAPPA
E(disease_model, path = c("pre_symptomatic_isolated", "hospitalized") )$label <- paste0("v[c] * Kappa = ", CONST_v_c  * CONST_KAPPA)


E(disease_model, path = c("pre_symptomatic_non_isolated", "mild_isolated") )$weight <- CONST_v_m * CONST_p * CONST_KAPPA
E(disease_model, path = c("pre_symptomatic_non_isolated", "mild_isolated") )$label <- paste0("v[m] * p * Kappa = ", CONST_v_m * CONST_p * CONST_KAPPA)

E(disease_model, path = c("pre_symptomatic_non_isolated", "mild_non_isolated") )$weight <- CONST_v_m * (1 - CONST_p) * CONST_KAPPA
E(disease_model, path = c("pre_symptomatic_non_isolated", "mild_non_isolated") )$label  <- paste0("v[m] * (1 - p) * Kappa = ", CONST_v_m * (1 - CONST_p) * CONST_KAPPA)

E(disease_model, path = c("pre_symptomatic_non_isolated", "hospitalized") )$weight <- CONST_v_h * CONST_KAPPA
E(disease_model, path = c("pre_symptomatic_non_isolated", "hospitalized") )$label <- paste0("v[h] * Kappa = ", CONST_v_h  * CONST_KAPPA)

E(disease_model, path = c("pre_symptomatic_non_isolated", "hospitalized_ICU") )$weight <- CONST_v_c * CONST_KAPPA
E(disease_model, path = c("pre_symptomatic_non_isolated", "hospitalized_ICU") )$label <- paste0("v[c] * Kappa = ", CONST_v_c  * CONST_KAPPA)




# From mild
E(disease_model, path = c("mild_isolated", "recovered") )$weight <- CONST_ita
E(disease_model, path = c("mild_isolated", "recovered") )$label <- paste0("ita = ", CONST_ita)

E(disease_model, path = c("mild_non_isolated", "recovered") )$weight <- CONST_ita
E(disease_model, path = c("mild_non_isolated", "recovered") )$label <- paste0("ita = ", CONST_ita)



# From hospitalized 
E(disease_model, path = c("hospitalized", "recovered") )$weight <- CONST_rho * (1 - CONST_f_h)
E(disease_model, path = c("hospitalized", "recovered") )$label <- paste0("rho * (1 - f[h]) = ", CONST_rho * (1 - CONST_f_h))

E(disease_model, path = c("hospitalized", "dead") )$weight <- CONST_rho * CONST_f_h
E(disease_model, path = c("hospitalized", "dead") )$label <- paste0("rho * f[h] = ", CONST_rho * CONST_f_h)


# From hospitalized with ICU usage
E(disease_model, path = c("hospitalized_ICU", "recovered") )$weight <- CONST_sigma * (1 - CONST_f_c)
E(disease_model, path = c("hospitalized_ICU", "recovered") )$label <- paste0("sigma * (1 - f[c]) = ", CONST_sigma * (1 - CONST_f_c))

E(disease_model, path = c("hospitalized_ICU", "dead") )$weight <- CONST_sigma * CONST_f_c
E(disease_model, path = c("hospitalized_ICU", "dead") )$label <- paste0("sigma * f[c] = ", CONST_sigma * CONST_f_c)

# Adding weight value to edge labels for visual inspection
# E(disease_model)$label <- paste(E(disease_model)$weight, "/day")

}

# Infection parameters for people with 1st dose of vaccination
# TODO: Change transition parameters



# Infection parameters for people with 2nd dose of vaccination
# TODO: Change transition parameters



# Attach susceptible with latent_infections (Latent) type states (transition implemented via special weight value) @"### INELEGANT SOLUTION ###" 
disease_model <- disease_model + edge("susceptible", "latent_infections_isolated", 
                                      weight = "CALCULATE_FROM_CONTACT_MATRIX", 
                                      label = paste0("if infected, then q = ", CONST_q))

disease_model <- disease_model + edge("susceptible", "latent_infections_not_isolated", 
                                       weight = "CALCULATE_FROM_CONTACT_MATRIX", 
                                       label = paste0("if infected, then (1 - q) = ", (1 - CONST_q)))



plot(disease_model, layout=layout_as_tree)

# tkplot(disease_model, layout=layout_as_tree, 
#        canvas.height = 800, canvas.width = 1000,
#        vertex.color="#77c98d", edge.arrow.size=0.75)

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

  # MAX_CHANCE <- 0.035 
  # MAX_CHANCE <- 0.025
  # MAX_CHANCE <- 0.015
  # MAX_CHANCE <- 0.005
  # MAX_CHANCE <- 0.002
  # MAX_CHANCE <- 0.001
  
  # TODO: Add a minimum cutoff time like Dr. Watmough suggested
  # Note: Since I am sequentially drawing from each contact as of now, 
  #        must change the algorithm first @"Tag: Sequence of probabilities"
  
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
#TOTAL_SIMULATION_DAYS <- 30
TOTAL_SIMULATION_DAYS <- 60 # fails, Probably needs try catch for nextElem()
TOTAL_SIMULATED_PERSONS <- max(contact_matrix_as_pairlist[1:2])

STATE <- array(rep(NA, TOTAL_SIMULATED_PERSONS * TOTAL_SIMULATION_DAYS), 
               dim = c(TOTAL_SIMULATED_PERSONS, TOTAL_SIMULATION_DAYS) )



# Set up simulation 

# Set every person to susceptible for the first day
STATE[ , 1] <- which( colnames(transition_matrix) == "susceptible" )





STATE_NAMES = colnames(transition_matrix)

# Mapping to SEIR 
STATELIST_SUSCEPTIBLE <- which(colnames(transition_matrix) == "susceptible" | 
                                 colnames(transition_matrix) == "received_dose1" |
                                 colnames(transition_matrix) == "received_dose2")

STATELIST_LATENT <- which(colnames(transition_matrix) == "latent_infections_isolated" | 
                             colnames(transition_matrix) == "latent_infections_not_isolated")

STATELIST_INFECTIOUS <- which(colnames(transition_matrix) == "pre_symptomatic_non_isolated" | 
                                colnames(transition_matrix) == "mild_non_isolated")


STATELIST_REMOVED <- which(colnames(transition_matrix) == "recovered" | 
                               colnames(transition_matrix) == "dead" )

STATELIST_NEW_CASES <- which(colnames(transition_matrix) == "latent_infections_isolated" | 
                                colnames(transition_matrix) == "mild_isolated" )



COUNT_FIRST_DAY_DISEASE_IMPORT <- 5 

DAILY_NEW_CASES <- array( rep(0, TOTAL_SIMULATION_DAYS), dim = TOTAL_SIMULATION_DAYS )

# Set first day new cases to COUNT_FIRST_DAY_DISEASE_IMPORT for bookkeeping
DAILY_NEW_CASES[1] <- COUNT_FIRST_DAY_DISEASE_IMPORT

# Randomly distribute the infection import
first_disease_import <- sample(seq_len(length(STATE[ , 1])), COUNT_FIRST_DAY_DISEASE_IMPORT)

cat("Setting first day disease import to: ", COUNT_FIRST_DAY_DISEASE_IMPORT, "\n")
for(person_id in first_disease_import){
  
  # Starting with infectious: may be changed to exposed
  STATE[person_id , 1] <- sample(STATELIST_INFECTIOUS, 1)

  # Debug
  cat("person id - infected: ", person_id, "\n")
  
}


# Simulate from day 2 to TOTAL_SIMULATION_DAYS
for(day_index in 2:TOTAL_SIMULATION_DAYS){
  
  # Slice yesterday's data
  previous_day_df <- data.frame(STATE[ , day_index - 1])
  colnames(previous_day_df) <- c("state_id")
  
  previous_day_df$person_id <- seq.int(nrow(previous_day_df))
  
  previous_day_infectious_df <- filter(previous_day_df, state_id %in% STATELIST_INFECTIOUS)
  
  # previous_day_latent_df <- filter(previous_day_df, state_id %in% STATELIST_LATENT)
  
  # previous_day_susceptible_df <- filter(previous_day_df, state_id %in% STATELIST_SUSCEPTIBLE)
  
  
  # colnames(previous_day_df) <- c("person_id_1", "state id")
  
  COUNT_NEW_INFECTIONS_TODAY <- 0 # flush the counter
  
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
    
    # TOTAL_PROBABILITY_OF_INFECTION <- 0
    
    if(NO_OF_CONTACT_MATRIX_BASED_TRANSITION > 0){
      contacts_df <- filter(contact_matrix_as_pairlist, person_id_1 == person_id)
      # cat("Contacts of ", person_id)
      # print(contacts_df$person_id_2)
      
      # Check if person 2 was infectious on day_index - 1
      infectious_contacts_df <- filter(contacts_df, person_id_2 %in% previous_day_infectious_df$person_id)
      # cat("Infectious ontacts of ", person_id)
      # print(infectious_contacts_df$person_id_2)
      
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
            next_state_id <- sample( c( which(STATE_NAMES == "latent_infections_not_isolated"), which(STATE_NAMES == "latent_infections_isolated") ),
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
    
    # For each person check if transition occurred 
    if(PREV_STATE_INDEX != next_state_id){
      # cat("Changed Person id: ", person_id, ":  ", STATE_NAMES[PREV_STATE_INDEX], "to ", STATE_NAMES[next_state_id], "\n")
      if( next_state_id %in% STATELIST_NEW_CASES) {
        COUNT_NEW_INFECTIONS_TODAY <- COUNT_NEW_INFECTIONS_TODAY + 1
      }
    }
    
  }
  
  
  
  
  # Tabulate new infections 
  DAILY_NEW_CASES[day_index] <- COUNT_NEW_INFECTIONS_TODAY
  
  # if there's another contact matrix available, then iterate
  # Generates "Error: StopIteration" when reaches end
  contact_matrix_as_pairlist <- tryCatch(
    {
      nextElem(contact_matrix_iterator)
    },
    error = function(e){
      # cat("Setting up day ", day_index, " with last known Contact matrix index: ", COUNT_CONTACT_MATRIX)
      contact_matrix_as_pairlist
    }
  )
    
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



# Daily new cases
OUTPUT_NEWCASES_FILEPATH_CSV <- paste(OUTPUT_DIRECTORY, UUID, "-DAILY-NEW-CASES.csv",  sep = "")

# write all Daily new cases
write.table(DAILY_NEW_CASES, sep=",", OUTPUT_NEWCASES_FILEPATH_CSV, row.names = FALSE, col.names = FALSE)

# Stop the clock
cat(proc.time() - ptm)
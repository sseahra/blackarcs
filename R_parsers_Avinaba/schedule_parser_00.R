# Get the data directory from readtext
# JSON_FILENAME <-'scenarioDane/schedules-1627398411898.json'
# JSON_FILENAME <-'scenarioAvin/schedules-1627418843229.json'

JSON_FILENAME <-'scenarioBenchmark/Schedule Benchmark A - Cambellton.json'

# JSON_FILENAME <-'scenarioBenchmark/benchmark B.json' # 5% scenario




OUTPUT_FILENAME <- substr(JSON_FILENAME,1, nchar(JSON_FILENAME)-5)
OUTPUT_FILENAME_TOTAL <- paste(OUTPUT_FILENAME, "-contact_matrix-total.json", sep = "")



library(jsonlite) # for read_json()
library(dplyr) # for distinct()
# library(bit) # for bit() vectors
# library(gplots) # for his2d()

# read all schedules as list of lists
schedules <- read_json(JSON_FILENAME) 

# TODO: check / cleanup departureTime and arrivedAtSource issues if needed

# no of agents 
no_of_agents <- length(schedules)

# trip length for each agent
trip_length <- lengths(schedules)

# trip length distribution  
trip_length_dist <- table(trip_length)

# trip length histogram
hist(trip_length, col = "#3d66a8")


# iterate and build combined location_ids for Residential and Non-Residential point of interests
# location_id_counter <- 0
all_locations <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(all_locations) <- c("citisketch_location_type", "citisketch_location_id")

for(i in 1:length(schedules)){
  #  list_of_single_schedule <- schedules[i]
  # Debug note: should be singleton list of a schedule
  #if(i == 1){
  #  print(schedules[i])
  #}
  
  for(j in 1:length(schedules[i])){
    # list_of_event <- schedules[i][[j]]
    # Debug note: should be list of schedule event
    #if(j == 1){
    #  print(schedules[i][[j]])
    #}
    for(k in 1:length(schedules[i][[j]])){
      event <- schedules[i][[j]][[k]]
      # Debug note: should be a single schedule event
      # event := [source, sourceIndex, arrivedAtSource, departureTime, destination, destinationIndex]
      #if(k == 1){
      #  print(schedules[i][[j]][[k]])
      #}
      
      # Add source 
      new_source_loc <- data.frame(
        citisketch_location_type = event$source,
        citisketch_location_id = event$sourceIndex
      )
      all_locations <- rbind(all_locations, new_source_loc)
      
      # Add destination
      new_destination_loc <- data.frame(
        citisketch_location_type = event$source,
        citisketch_location_id = event$sourceIndex
      )
      all_locations <- rbind(all_locations, new_destination_loc)
      
      }
    }
}

# Remove duplicates from visited locations 
distinct_locations <- distinct(all_locations) 

# total no. of locations visited 
distinct_location_count <- count(distinct_locations)[1, "n"]

# For the Campbellton Scenario 
# No. of agents : 4986
# No. of visited/stayed in locations: 3185
#
# If we consider a 3D bit vector for agents X locations X time_slice
# for the overarching proto-data structure which may be flattened with 
# different projections/strategies to construct the contact matrix 
#
# with time_slice of 1 second -> we need at least 1.39 * (10^12) bits ~ 176 GB
# blurring the resolution to 1 minute -> 2.32 * (10^8) bits ~ 3 GB
# -> as of now bits are getting coerced into integers for list or array or matrix types
#
#
# further blurring resolution to 10 minute to quickly test the idea with integer arrays ~ 10 GB

time_slice <- (24 * 60) / 10
time_slice_in_seconds <- 60 * 10
agent_x_loc_x_time <- array(rep(FALSE, no_of_agents * distinct_location_count * time_slice), 
                            dim = c(no_of_agents, distinct_location_count, time_slice) )
contact_matrix <- array(rep(0, no_of_agents * no_of_agents), dim = c(no_of_agents, no_of_agents))


count_stationary_agents <- 0
count_non_looping_agents <- 0
count_agents_departing_post_24h <- 0

# Assign occupancy for each 144 time_slices for all agent X location 
for(i in 1:length(schedules)){
  # agent_id <- i - 1
  agent_id <- i
  
  for(j in 1:length(schedules[i])){
    
    startloc_row_id <- which(distinct_locations$citisketch_location_type == schedules[i][[j]][[1]]$source 
                             & distinct_locations$citisketch_location_id == schedules[i][[j]][[1]]$sourceIndex )
    # set escape cases
    stays_in_flag <- FALSE
    
    for(k in 1:length(schedules[i][[j]])){
      event <- schedules[i][[j]][[k]]
      # event := [source, sourceIndex, arrivedAtSource, departureTime, destination, destinationIndex]
      
      if(stays_in_flag == TRUE){
        break
      }
      # Case: starting location
      if(k == 1){ 
        # Debug
        # print(i)
        # print(startloc_row_id)
        # print(distinct_locations[startloc_row_id, ])
        # print("--")
        
        # check for all 9s (forever stays)
        STAYS_IN <- 99999999999999
        if(event$departureTime == STAYS_IN){
          
          stays_in_flag <- TRUE
          
          # set all time_slices to TRUE for this agent (i - 1)
          # print(i - 1)
          
          
          agent_x_loc_x_time[agent_id, startloc_row_id, ] <- TRUE
          
          
          count_stationary_agents <- count_stationary_agents + 1
        }
        else{
          start_timesilce <- floor(event$arrivedAtSource / time_slice_in_seconds)
          end_timeslice <- floor(event$departureTime / time_slice_in_seconds)
          
          # print(agent_id)
          
          agent_x_loc_x_time[agent_id, 
                             startloc_row_id, 
                             start_timesilce:end_timeslice ] <- TRUE
        }
      }
      
      # Case: ending event location
      else if(k == length(schedules[i][[j]])) {
        eventloc_row_id <- which(distinct_locations$citisketch_location_type == event$source 
                                 & distinct_locations$citisketch_location_id == event$sourceIndex )
        
        # Check for departure time > 2400 hours (i.e > 86400 seconds)
        if(event$departureTime > 86400){
          count_agents_departing_post_24h  <- count_agents_departing_post_24h + 1
          
          # Forcing departure time to be one time slice less than 24000
          # Making sure the agent reaches the final destination with 
          # an assumed commute time of 10 min < 8 min (expected)
          event$departureTime <- 86400 - time_slice_in_seconds
        }
        
        
        start_timesilce <- floor(event$arrivedAtSource / time_slice_in_seconds)
        end_timeslice <- floor(event$departureTime / time_slice_in_seconds)
        
        # Debug: 
        # print(agent_id)
        # print(eventloc_row_id)
        # print(start_timesilce)
        # print(end_timeslice)
        
        # Mark time slices at penultimate location
        agent_x_loc_x_time[agent_id, 
                           eventloc_row_id, 
                           start_timesilce:end_timeslice ] <- TRUE
        
        
        endloc_row_id <- which(distinct_locations$citisketch_location_type == event$destination 
                                 & distinct_locations$citisketch_location_id == event$destinationIndex )
        
        # Check for incomplete loops
        if(endloc_row_id != startloc_row_id){
          count_non_looping_agents <- count_non_looping_agents + 1
          
          # Debug: 
          # print("Non looping schedule")
          # print(agent_id)
        }
        
        # Debug: 
        # print(endloc_row_id)
        # print((end_timeslice + 1))
        # print("--")
        
        
        # Assuming agent reaches final location within 1 time_slice ~ 10 min > 8 min
        # Mark time slices at last destination from end_timeslice + 1 till the end time_slice (144)
        if((end_timeslice + 1) <= time_slice){
          agent_x_loc_x_time[agent_id, 
                             endloc_row_id, 
                             (end_timeslice + 1):time_slice ] <- TRUE
        }
        
        
      }
      # Case: In between locations
      else {
        eventloc_row_id <- which(distinct_locations$citisketch_location_type == event$source 
                                 & distinct_locations$citisketch_location_id == event$sourceIndex )
        
        start_timesilce <- floor(event$arrivedAtSource / time_slice_in_seconds)
        end_timeslice <- floor(event$departureTime / time_slice_in_seconds)
        
        # Debug: 
        # print(agent_id)
        # print(eventloc_row_id)
        # print(start_timesilce)
        # print(end_timeslice)
        
        # Mark time slices at penultimate location
        agent_x_loc_x_time[agent_id, 
                           eventloc_row_id, 
                           start_timesilce:end_timeslice ] <- TRUE
      }
      
    }
  }
}

# Get list of location visited by each agent
# list_of_loc_visited_by <- list()
# for(i in 1:no_of_agents){
  
  # flush the result
  # single_agent_x_loc <- array(rep(FALSE, distinct_location_count), 
  #                           dim = distinct_location_count)
  # for(k in 1:time_slice){
  #   single_agent_x_loc <- single_agent_x_loc | agent_x_loc_x_time[i, , k]
  # }
  
  
  # Debug:
  # print(i) # agent id
  # print(which(single_agent_x_loc == TRUE))
  # print("--")
  
  # list_of_loc_visited_by[[i]] <- which(single_agent_x_loc == TRUE)
# }


# Get list of agents for each location
list_of_agents_at_loc <- list()
for(j in 1:distinct_location_count){
  
  #flush the result
  single_loc_x_agent <- array(rep(FALSE, no_of_agents),
                              dim = no_of_agents)
  for(k in 1:time_slice){
    single_loc_x_agent <- single_loc_x_agent | agent_x_loc_x_time[ , j, k]
  }
  
  # Debug:
  # print(j) # location id
  # print(which(single_loc_x_agent == TRUE))
  # print("--")
  
  list_of_agents_at_loc[[j]] <- which(single_loc_x_agent == TRUE)
}

# Trace contact matrix per time-slice
for(k in 1:time_slice){
  for(j in 1:distinct_location_count){
    agents_in_contact <- which(agent_x_loc_x_time[ , j, k] == TRUE)
    count_agents_in_contact <- length(agents_in_contact)
    
    # for any (location, time_slice) pair 
    # check for possible 
    if(count_agents_in_contact > 1){
      # Debug: 
      # print(k) # at k-th time slice
      # print(j) # on/in j-th location
      # print(distinct_locations[j, ]$citisketch_location_type) # location type
      # print(agents_in_contact) # list agents
      # print(count_agents_in_contact) # count
      # print("--")
      
      # Add relevant weight to contact matrix
      # TODO: How might we consider different weights for different locations?
      # since different location offer different modalities of shared activities
      
      contact_weight <- 1
      for(outer_agent_id in agents_in_contact){
        for(inner_agent_id in agents_in_contact){
          
          if(outer_agent_id == inner_agent_id){
            break
          }
          
          contact_matrix[outer_agent_id, inner_agent_id] <- 
            contact_matrix[outer_agent_id, inner_agent_id] + contact_weight
          
          contact_matrix[inner_agent_id, outer_agent_id] <- contact_matrix[outer_agent_id, inner_agent_id] # contact is bi-directional
        }
      }

    }
  }
}

# Get agents with max time overlap
which(contact_matrix == max(contact_matrix), arr.ind = T) - 1 # to correlate with citisketch ids

# Generating bitmap for visual inspection 
image(contact_matrix)

# Write full contact matrices 
write_json(contact_matrix, OUTPUT_FILENAME_TOTAL)



# Write pairlists of contact as csv
# [agent_id_1, agent_id_2, total_contact_seconds]

# get all the contact pairs from 2D matrix
pairwise_list <- which(contact_matrix > 0, arr.ind = TRUE, useNames = FALSE)

# generate a dataframe  
pairwise_list <- data.frame(pairwise_list)

# add blank column
pairwise_list <- data.frame(pairwise_list, contact_seconds = 0)

# fill with contact_weight * 10 * 60 ~> (overestimated) contact seconds 
for(i in 1:nrow(pairwise_list)) {
  pairwise_list[i, 3] <- contact_matrix[ pairwise_list[i, 1], pairwise_list[i, 2]  ]  * time_slice_in_seconds
}

# sort by first agent for easier comparison
pairwise_list <- pairwise_list[order(pairwise_list$X1),, drop=FALSE]

# set output path
OUTPUT_PAIRLIST_FILENAME_TOTAL <- paste(OUTPUT_FILENAME, "-contact_matrix-total.csv", sep = "")

# write as CSV 
# write.csv(pairwise_list, OUTPUT_PAIRLIST_FILENAME_TOTAL, row.names = FALSE, col.names = FALSE)
write.table(pairwise_list, sep=",", OUTPUT_PAIRLIST_FILENAME_TOTAL, row.names = FALSE, col.names = FALSE)

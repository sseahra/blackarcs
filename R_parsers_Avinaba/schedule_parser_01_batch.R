# Start the clock!
ptm <- proc.time()

library(jsonlite) # for read_json()
library(dplyr) # for distinct() and filter()

SCHEDULE_INPUT_DIR <- 'scenarioAvin_0_to_30_control'

# Pattern matching files with names "schedules- ..... .json", as output from Citisketch
LIST_OF_SCHEDULE_JSON_FILEPATH <- 
  list.files(SCHEDULE_INPUT_DIR, full.names = TRUE, pattern="^schedules\\-.*\\.json$", ignore.case = TRUE)

for(JSON_FILENAME in LIST_OF_SCHEDULE_JSON_FILEPATH) {
  
  # Debug: 
  # cat("Processing: ", JSON_FILENAME )
  
  # output name stub
  OUTPUT_FILENAME <- substr(JSON_FILENAME,1, nchar(JSON_FILENAME)-5)
  
  # read all schedules as list of lists
  schedules <- read_json(JSON_FILENAME) 
  
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
    
    for(j in 1:length(schedules[i])){
  
      for(k in 1:length(schedules[i][[j]])){
        event <- schedules[i][[j]][[k]]
        
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
  
  
  # create blank dataframe structure as described by Dr. Seahra (except for "class")
  
  schedules_as_df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(schedules_as_df) <- c("person", "location_id", "arrival", "departure")
  
  
  # convert list of list of lists into flattened dataframe of "stops"
  # * Preprocess departureTime == 9s and arrivedAtSource == -1 as stationary agents 
  
  # error code never departing all 9s 
  FAIL_DEPARTURE_TIME <- 99999999999999
  # error code for agent had plans but routing failed 
  FAIL_ROUTING <- -1
  
  # * add extra entry for loopbacks into the final destination 
  
  # extra bookkeeping  
  count_stationary_agents <- 0
  count_non_looping_agents <- 0
  count_agents_departing_post_24h <- 0
  
  for(i in 1:length(schedules)){
    person_id <- i
    
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
          
          # Subcase: stationary agent
          if(event$departureTime == FAIL_DEPARTURE_TIME || event$arrivedAtSource == FAIL_ROUTING){
            
            stays_in_flag <- TRUE
            
            # create record
            new_stop <- data.frame(
              person = person_id,
              location_id = startloc_row_id, 
              arrival = 0,
              departure = 24 * 60 * 60 
            )
            # add to dataframe 
            schedules_as_df <- rbind(schedules_as_df, new_stop)
            
            # bookkeeping
            count_stationary_agents <- count_stationary_agents + 1
          }
          
          else {
            
            # create record
            new_stop <- data.frame(
              person = person_id,
              location_id = startloc_row_id, 
              arrival = event$arrivedAtSource,
              departure = event$departureTime
            )
            # add to dataframe 
            schedules_as_df <- rbind(schedules_as_df, new_stop)
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
            # an assumed commute time of 8 min (expected)
            event$departureTime <- 86400 - 8 * 60 
          }
          
          # Create record for penultimate stop 
          new_stop <- data.frame(
            person = person_id,
            location_id = eventloc_row_id, 
            arrival = event$arrivedAtSource,
            departure = event$departureTime
          )
          # add to dataframe 
          schedules_as_df <- rbind(schedules_as_df, new_stop)
          
          
          
          endloc_row_id <- which(distinct_locations$citisketch_location_type == event$destination 
                                 & distinct_locations$citisketch_location_id == event$destinationIndex )
          
          # Check for incomplete loops
          if(endloc_row_id != startloc_row_id){
            count_non_looping_agents <- count_non_looping_agents + 1
          }
          
          
          # arrival time at final location 
          endloc_arrival <- event$departureTime + 8
          
          # check for overflow 
          if( endloc_arrival > 24 * 60 * 60 ) {
            endloc_arrival <- 24 * 60 * 60
          }
          
          # Create record for final stop 
          new_stop <- data.frame(
            person = person_id,
            location_id = endloc_row_id, 
            arrival = endloc_arrival, # assumed 8 minute commute
            departure = 24 * 60 * 60
          )
          # add to dataframe 
          schedules_as_df <- rbind(schedules_as_df, new_stop)
        }
        
        # Case: In between events
        else {
          eventloc_row_id <- which(distinct_locations$citisketch_location_type == event$source 
                                   & distinct_locations$citisketch_location_id == event$sourceIndex )
          
          
          # create record
          new_stop <- data.frame(
            person = person_id,
            location_id = eventloc_row_id, 
            arrival = event$arrivedAtSource,
            departure = event$departureTime
          )
          # add to dataframe 
          schedules_as_df <- rbind(schedules_as_df, new_stop)
        }
        
      }
    }
  }
  
  
  # Calculate contact 
  
  # creating blank 2D matrix of total contact time 
  contact_matrix <- array(rep(0, no_of_agents * no_of_agents), dim = c(no_of_agents, no_of_agents))
  
  
  # contact pair list + location
  
  # structure:= person_id_1, person_id_2, contact_time, contact_location_id
  # 
  #    location_id: eventually assign different probability for different (location, contact_time) based on expert opinion from Public Health Officials 
  #                 also useful to quickly remove contact overlap by location_id (simulate closure of non-essential location types for extreme countermeasures)
  # contact_pairlist_x_location <- data.frame(matrix(ncol = 4, nrow = 0))
  # colnames(contact_pairlist_x_location) <- c("person_1", "person_2", "contact_time", "location_id")
  
  
  for(person1_id in 1:no_of_agents){
    single_schedule_df <- filter(schedules_as_df, person == person1_id)
    count_locations_visited <- nrow(single_schedule_df)
    
    # print(paste0("For person id: ", person1_id))
    for(j in 1:count_locations_visited){
      
      this_location_id <- single_schedule_df[j, 2]
      person1_arrival <- single_schedule_df[j, 3]
      person1_departure <- single_schedule_df[j, 4]
      
      all_overlap_df <- filter(schedules_as_df, 
                               (person != person1_id) & 
                               (location_id == this_location_id) & 
                               (arrival <= person1_departure | departure >= person1_arrival))
      # print(all_overlap_df)
      
      count_overlaps <- nrow(all_overlap_df)
      
      if(count_overlaps > 0){
        for(k in 1:count_overlaps){
          
          total_contact_time <- 0
          
          person2_id <- all_overlap_df[k, 1]
          person2_arrival <- all_overlap_df[k, 3]
          person2_departure <- all_overlap_df[k, 4]
          
          contact_start <- max(person1_arrival, person2_arrival)
          contact_end <- min(person1_departure, person2_departure)
          
          # TODO: Investigate cause of negative outputs
          total_contact_time <- abs(contact_end - contact_start)
          
          # Debug: 
          # print(paste0("contact no: ", k))
          # print(paste0("contact size: ", total_contact_time))
          contact_matrix[person1_id, person2_id] <- contact_matrix[person1_id, person2_id] + total_contact_time
          
  
          # create new contact pair 
          # new_contact_pair <- data.frame(
          #   person_1 = person1_id,
          #   person_2 = person2_id, 
          #   contact_time = total_contact_time,
          #   location_id = this_location_id
          # )
          # add to dataframe 
          # contact_pairlist_x_location <- rbind(contact_pairlist_x_location, new_contact_pair)
          
        }
      }
      
    }
    # print("--")
  }
  
  # Get agents with max time overlap
  # which(contact_matrix == max(contact_matrix), arr.ind = T) - 1 # to correlate with citisketch ids
  
  # OUTPUT_FILENAME_TOTAL <- paste(OUTPUT_FILENAME, "-contact_matrix-total-real.json", sep = "")
  # Write full contact matrices 
  # write_json(contact_matrix, OUTPUT_FILENAME_TOTAL)
  
  
  # Write pairlists of contact as csv
  # [agent_id_1, agent_id_2, total_contact_seconds]
  
  # get all the contact pairs from 2D matrix
  pairwise_list <- which(contact_matrix > 0, arr.ind = TRUE, useNames = FALSE)
  
  # generate a dataframe  
  pairwise_list <- data.frame(pairwise_list)
  
  # fill with contact_weight * 10 * 60 ~> (overestimated) contact seconds 
  pairwise_list$contact_seconds <- contact_matrix[contact_matrix > 0]
  
  # sort by first agent for easier comparison
  pairwise_list <- pairwise_list[order(pairwise_list$X1),, drop=FALSE]
  
  # set output path
  OUTPUT_PAIRLIST_FILENAME_TOTAL <- paste(OUTPUT_FILENAME, "-contact_matrix-total-real.csv", sep = "")
  
  # write as CSV 
  # write.csv(pairwise_list, OUTPUT_PAIRLIST_FILENAME_TOTAL, row.names = FALSE, col.names = FALSE)
  write.table(pairwise_list, sep=",", OUTPUT_PAIRLIST_FILENAME_TOTAL, row.names = FALSE, col.names = FALSE)
  
  # write location list as CSV 
  OUTPUT_LOCATION_ID_FILENAME <- paste(OUTPUT_FILENAME, "-location_id.csv", sep = "")
  distinct_locations$ID <- seq.int(nrow(distinct_locations))
  write.table(distinct_locations, sep=",", OUTPUT_LOCATION_ID_FILENAME, row.names = FALSE, col.names = FALSE)

}  
  
# Stop the clock
cat(proc.time() - ptm)

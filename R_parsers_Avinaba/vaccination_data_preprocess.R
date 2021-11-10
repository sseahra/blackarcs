# Script to pre-process vaccination data 

CONST_NB_POP = 779993

# Vaccination data directory 
VACCINATION_DATA_DIR <- 'vaccination_data/New_Brunswick/'

# CSV dictionary "Days since 4th Jan 2021", "Total no. of doses disbursed" 
LIST_OF_VACCINATION_DATA_FILEPATH  <- 
  list.files(VACCINATION_DATA_DIR, full.names = TRUE, pattern=".csv$", ignore.case = TRUE)


# Empty list 
vaccination_data_table_list <- list()

for(VACCINATION_DATA_FILENAME in LIST_OF_VACCINATION_DATA_FILEPATH){
  
  # Debug:
  # cat(VACCINATION_DATA_FILENAME, "\n")
  
  # Read vaccination data 
  vaccination_data_table <- read.table(VACCINATION_DATA_FILENAME, sep=",")
  
  # Adding readable column names 
  colnames(vaccination_data_table) <- c("day_since_jan4th2021", basename(VACCINATION_DATA_FILENAME))
  
  # Append to list
  vaccination_data_table_list <- append(vaccination_data_table_list, vaccination_data_table)
  
}

TOTAL_DAYS_OF_OBSERVATION <- max(lengths(vaccination_data_table_list))

# Setup vaccination data as "Days since Jan 4th 2021", "no. new 1st doses", "no. of new 2nd doses"
vaccination_perday_mat <- matrix (ncol = 3, nrow = TOTAL_DAYS_OF_OBSERVATION)
vaccination_perday_mat_colnames <- c("day_since_jan42021", "dose1", "dose2")
colnames(vaccination_perday_mat) <- vaccination_perday_mat_colnames


# TODO: Inelegent joining of two tables, refactor
# Check if date indices match
if(sum(vaccination_data_table_list[[1]] == vaccination_data_table_list[[3]]) == TOTAL_DAYS_OF_OBSERVATION){
  
  # Debug
  # cat("All date indices match\n")
  
  # Copy the first day data 
  vaccination_perday_mat[1, "day_since_jan42021"] <- vaccination_data_table_list[[1]][1]
  vaccination_perday_mat[1, "dose1"] <- vaccination_data_table_list[[2]][1]
  vaccination_perday_mat[1, "dose2"] <- vaccination_data_table_list[[4]][1]
  
  # Start from 2nd index 
  for(i in 2:TOTAL_DAYS_OF_OBSERVATION){
    
    # Debug
    # cat("For day: ", i, "\n")
    vaccination_perday_mat[i, "day_since_jan42021"] <- vaccination_data_table_list[[1]][i]
    
    # change cumulative to new vaccinations 
    # change cumulative to new vaccinations 
    vaccination_perday_mat[i, "dose2"] <- vaccination_data_table_list[[4]][i] - vaccination_data_table_list[[4]][i -1]
    
    vaccination_perday_mat[i, "dose1"] <- vaccination_data_table_list[[2]][i] - vaccination_data_table_list[[2]][i -1] +  vaccination_perday_mat[i, "dose2"] 
  }
}
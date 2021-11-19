# Script to plot daily new cases and cumulative active cases with and without school closures 

# Start the clock!
ptm <- proc.time()

# High level algo: 
#
# 1. Read infection history to find outbreaks with more than 2 total infections 
# 2. Read daily active cases and remove non-outbreaks
# 3. Repeat 1 to 2 for both sets of simulation runs, with and without school closures 

# Sim output directory for cumulative active case based school closures 
SCHOOL_CLOSURE_INPUT_DIR <- 'complex_model_v01_w_school_closure_010_for_365d_output/scenarioAvin_0_to_30_control'

# Pattern matching files with names "*INFECTION-HISTORY.csv", as output from Citisketch
LIST_OF_INFECTION_HISTORY_W_SCHOOL_CLOSURE_FILEPATH <- 
  list.files(SCHOOL_CLOSURE_INPUT_DIR, full.names = TRUE, pattern="-INFECTION\\-HISTORY\\.csv$", ignore.case = TRUE)

# Empty list 
# infection_history_w_school_closure_table_list <- list()
outbreak_run_index_w_school_closure_list <- c()
non_outbreak_run_index_w_school_closure_list <- c()

for(index in 1:length(LIST_OF_INFECTION_HISTORY_W_SCHOOL_CLOSURE_FILEPATH)) {
  
  # Read as CSV 
  temp_infection_history_w_school_closure_table <- read.table(LIST_OF_INFECTION_HISTORY_W_SCHOOL_CLOSURE_FILEPATH[index], sep = ",")
  temp_total_infections <- length(which(!is.na(temp_infection_history_w_school_closure_table$variant)))
  
  # Debug: 
  # cat("Run ", index, ", has total infections: ", temp_total_infections, "\n", sep = "")
  
  # Append to list if total no. of infections are greater than 2
  if(temp_total_infections > 2){
    outbreak_run_index_w_school_closure_list <- c(outbreak_run_index_w_school_closure_list, index)
  }
  else{
    non_outbreak_run_index_w_school_closure_list <- c(non_outbreak_run_index_w_school_closure_list, index)
  }
}

cat("\nFor runs with threshold based school closure: ")
cat("\n  No. of runs: ", length(LIST_OF_INFECTION_HISTORY_W_SCHOOL_CLOSURE_FILEPATH), sep = "")
cat("\n  Percent outbreak: ", (length(outbreak_run_index_w_school_closure_list) / length(LIST_OF_INFECTION_HISTORY_W_SCHOOL_CLOSURE_FILEPATH)) * 100, sep = "")
cat("\n  Non-outbreak runs to remove from plot: ", non_outbreak_run_index_w_school_closure_list)



# Pattern matching files with names "*DAILY-ACTIVE-CASES.csv", as output from Citisketch
LIST_OF_DAILY_ACTIVE_CASES_W_SCHOOL_CLOSURE_FILEPATH <-
  list.files(SCHOOL_CLOSURE_INPUT_DIR, full.names = TRUE, pattern="DAILY\\-ACTIVE\\-CASES\\.csv$", ignore.case = TRUE)


# Empty list
daily_cases_w_school_closure_table_list <- list()

max_no_of_days <- 0

for(DAILY_ACTIVE_CASES_W_SCHOOL_CLOSURE_FILENAME in LIST_OF_DAILY_ACTIVE_CASES_W_SCHOOL_CLOSURE_FILEPATH){

  # Debug:
  # cat(DAILY_ACTIVE_CASES_W_SCHOOL_CLOSURE_FILENAME, "\n")

  # Read vaccination data
  temp_daily_cases_w_school_closure_table <- read.table(DAILY_ACTIVE_CASES_W_SCHOOL_CLOSURE_FILENAME, sep=",")


  # Append to list
  daily_cases_w_school_closure_table_list <- append(daily_cases_w_school_closure_table_list, temp_daily_cases_w_school_closure_table)

  temp_max_no_of_days <- max(lengths(temp_daily_cases_w_school_closure_table))

  # Get max no of days
  if(max_no_of_days < temp_max_no_of_days){
    max_no_of_days <- temp_max_no_of_days
  }
}

no_of_sims <- length(daily_cases_w_school_closure_table_list)



# Debug value
CONST_NOT_KNOWN = -1

# Translate to matrix 
active_cases_w_school_closure_2D_mat <- array(rep(CONST_NOT_KNOWN, max_no_of_days * no_of_sims),
                          dim = c(max_no_of_days, no_of_sims) )


for( i in 1:no_of_sims ){
  for( j in 1:length(daily_cases_w_school_closure_table_list[[i]])){
    active_cases_w_school_closure_2D_mat[j, i] <- daily_cases_w_school_closure_table_list[[i]][[j]]
  }

}

mean_cases <- c()
median_cases <- c()
min_cases <- c()
max_cases <- c()
first_qurtile_cases <- c()
third_quartile_cases <- c()



# Remove non outbreak cases 
# 
# Cases where total number of infections doesn't go beyond 1 
# 
# Cases where total number of infections doesn't go beyond 2
active_cases_w_school_closure_2D_mat <- active_cases_w_school_closure_2D_mat[ , - c(8, 11, 23, 25, 28, 47) ] 
# 
for(i in 1:max_no_of_days){
  temp_summary <- summary(active_cases_w_school_closure_2D_mat[i , ])
  
  mean_cases[i] <- temp_summary["Mean"]
  median_cases[i] <- temp_summary["Median"]
  min_cases[i] <- temp_summary["Min."]
  max_cases[i] <- temp_summary["Max."]
  first_qurtile_cases[i] <- temp_summary["1st Qu."]
  third_quartile_cases[i] <- temp_summary["3rd Qu."]

}

active_cases_w_school_closure_df <- as.data.frame(active_cases_w_school_closure_2D_mat)

#Summary stats
active_cases_w_school_closure_df <- cbind(active_cases_w_school_closure_df, mean_cases)
active_cases_w_school_closure_df <- cbind(active_cases_w_school_closure_df, median_cases)
active_cases_w_school_closure_df <- cbind(active_cases_w_school_closure_df, min_cases)
active_cases_w_school_closure_df <- cbind(active_cases_w_school_closure_df, max_cases)
active_cases_w_school_closure_df <- cbind(active_cases_w_school_closure_df, first_qurtile_cases)
active_cases_w_school_closure_df <- cbind(active_cases_w_school_closure_df, third_quartile_cases)

# End of processing cumulative active cases of runs with school closure 

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Sim output directory for runs without closures 
CONTROL_INPUT_DIR <- 'complex_model_v01_010_for_365d_output/scenarioAvin_0_to_30_control'

# Pattern matching files with names "*INFECTION-HISTORY.csv", as output from Citisketch
LIST_OF_INFECTION_HISTORY_FILEPATH <- 
  list.files(CONTROL_INPUT_DIR, full.names = TRUE, pattern="-INFECTION\\-HISTORY\\.csv$", ignore.case = TRUE)

# Empty list 
outbreak_run_index_list <- c()
non_outbreak_run_index_list <- c()

for(index in 1:length(LIST_OF_INFECTION_HISTORY_FILEPATH)) {
  
  # Read as CSV 
  temp_infection_history_table <- read.table(LIST_OF_INFECTION_HISTORY_FILEPATH[index], sep = ",")
  temp_total_infections <- length(which(!is.na(temp_infection_history_table$variant)))
  
  # Debug: 
  # cat("Run ", index, ", has total infections: ", temp_total_infections, "\n", sep = "")
  
  # Append to list if total no. of infections are greater than 2
  if(temp_total_infections > 2){
    outbreak_run_index_list <- c(outbreak_run_index_list, index)
  }
  else{
    non_outbreak_run_index_list <- c(non_outbreak_run_index_list, index)
  }
}

cat("\nFor runs without intervention measures: ")
cat("\n  No. of runs: ", length(LIST_OF_INFECTION_HISTORY_FILEPATH), sep = "")
cat("\n  Percent outbreak: ", (length(outbreak_run_index_list) / length(LIST_OF_INFECTION_HISTORY_FILEPATH)) * 100, sep = "")
cat("\n  Non-outbreak runs to remove from plot: ", non_outbreak_run_index_list)



# Pattern matching files with names "*DAILY-ACTIVE-CASES.csv", as output from Citisketch
LIST_OF_DAILY_ACTIVE_CASES_FILEPATH <-
  list.files(CONTROL_INPUT_DIR, full.names = TRUE, pattern="DAILY\\-ACTIVE\\-CASES\\.csv$", ignore.case = TRUE)


# Empty list
daily_cases_table_list <- list()

max_no_of_days <- 0

for(DAILY_ACTIVE_CASES_FILENAME in LIST_OF_DAILY_ACTIVE_CASES_FILEPATH){
  
  # Debug:
  # cat(DAILY_ACTIVE_CASES_FILENAME, "\n")
  
  # Read vaccination data
  temp_daily_cases_table <- read.table(DAILY_ACTIVE_CASES_FILENAME, sep=",")
  
  
  # Append to list
  daily_cases_table_list <- append(daily_cases_table_list, temp_daily_cases_table)
  
  temp_max_no_of_days <- max(lengths(temp_daily_cases_table))
  
  # Get max no of days
  if(max_no_of_days < temp_max_no_of_days){
    max_no_of_days <- temp_max_no_of_days
  }
}

no_of_sims <- length(daily_cases_table_list)



# Debug value
CONST_NOT_KNOWN = -1

# Translate to matrix 
active_cases_2D_mat <- array(rep(CONST_NOT_KNOWN, max_no_of_days * no_of_sims),
                                              dim = c(max_no_of_days, no_of_sims) )


for( i in 1:no_of_sims ){
  for( j in 1:length(daily_cases_table_list[[i]])){
    active_cases_2D_mat[j, i] <- daily_cases_table_list[[i]][[j]]
  }
  
}

mean_cases <- c()
median_cases <- c()
min_cases <- c()
max_cases <- c()
first_qurtile_cases <- c()
third_quartile_cases <- c()



# Remove non outbreak cases 
# 
# Cases where total number of infections doesn't go beyond 1 
# 
# Cases where total number of infections doesn't go beyond 2
active_cases_2D_mat <- active_cases_2D_mat[ , - c(2, 4, 8, 9, 17, 19, 23, 26, 27, 30, 33, 37, 43, 47) ] 
# 
for(i in 1:max_no_of_days){
  temp_summary <- summary(active_cases_2D_mat[i , ])
  
  mean_cases[i] <- temp_summary["Mean"]
  median_cases[i] <- temp_summary["Median"]
  min_cases[i] <- temp_summary["Min."]
  max_cases[i] <- temp_summary["Max."]
  first_qurtile_cases[i] <- temp_summary["1st Qu."]
  third_quartile_cases[i] <- temp_summary["3rd Qu."]
  
}

active_cases_df <- as.data.frame(active_cases_2D_mat)

#Summary stats
active_cases_df <- cbind(active_cases_df, mean_cases)
active_cases_df <- cbind(active_cases_df, median_cases)
active_cases_df <- cbind(active_cases_df, min_cases)
active_cases_df <- cbind(active_cases_df, max_cases)
active_cases_df <- cbind(active_cases_df, first_qurtile_cases)
active_cases_df <- cbind(active_cases_df, third_quartile_cases)





# For viz 
library(tidyverse)

# Aesthetic
CONST_PLOT_START_DAY = 1
CONST_PLOT_END_DAY = 100

CONST_PLOT_INTERVENTION_LINE_COLOUR = "#42f57e34"
CONST_PLOT_INTERVENTION_MEDIAN_LINE_COLOUR = "#248043"
CONST_PLOT_INTERVENTION_FIRST_QUARTILE_LINE_COLOUR = "#417853"
CONST_PLOT_INTERVENTION_THIRD_QUARTILE_LINE_COLOUR = "#417853"

CONST_PLOT_LINE_COLOUR = "#F6871734"
CONST_PLOT_MEDIAN_LINE_COLOUR = "#F73B0C"
CONST_PLOT_FIRST_QUARTILE_LINE_COLOUR = "#9c2c10"
CONST_PLOT_THIRD_QUARTILE_LINE_COLOUR = "#9c2c10"


ggplot() +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V1), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V2), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V3), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V4), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V5), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V6), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V7), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V8), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V9), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V10), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V11), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V12), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V13), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V14), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V15), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V16), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V17), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V18), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V19), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V20), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V21), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V22), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V23), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V24), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V25), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V26), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V27), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V28), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V29), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V30), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V31), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V32), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V33), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V34), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V35), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V36), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V37), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V38), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V39), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V40), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V41), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V42), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V43), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V44), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) + 
  # ---- Non outbreak cutoff
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V45), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V46), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V47), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V48), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V49), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = V50), color = CONST_PLOT_INTERVENTION_LINE_COLOUR) +

  geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = median_cases), color = CONST_PLOT_INTERVENTION_MEDIAN_LINE_COLOUR) + 
  
  geom_ribbon(data = active_cases_w_school_closure_df, mapping = aes(x = seq(1:max_no_of_days), ymax = third_quartile_cases, ymin = first_qurtile_cases), fill = CONST_PLOT_INTERVENTION_MEDIAN_LINE_COLOUR, alpha = 0.4) + 
  
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = first_qurtile_cases), color = CONST_PLOT_INTERVENTION_FIRST_QUARTILE_LINE_COLOUR) + 
  # geom_line(data = active_cases_w_school_closure_df, mapping = aes( x = seq(1:max_no_of_days), y = third_quartile_cases), color = CONST_PLOT_INTERVENTION_THIRD_QUARTILE_LINE_COLOUR) +
  
  
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V1), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V2), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V3), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V4), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V5), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V6), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V7), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V8), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V9), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V10), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V11), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V12), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V13), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V14), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V15), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V16), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V17), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V18), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V19), color = CONST_PLOT_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V20), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V21), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V22), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V23), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V24), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V25), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V26), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V27), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V28), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V29), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V30), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V31), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V32), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V33), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V34), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V35), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V36), color = CONST_PLOT_LINE_COLOUR) +
  # ---- Non outbreak cutoff
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V37), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V38), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V39), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V40), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V41), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V42), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V43), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V44), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V45), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V46), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V47), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V48), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V49), color = CONST_PLOT_LINE_COLOUR) +
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V50), color = CONST_PLOT_LINE_COLOUR) +
  
  geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = median_cases), color = CONST_PLOT_MEDIAN_LINE_COLOUR) + 
  
  geom_ribbon(data = active_cases_df, mapping = aes(x = seq(1:max_no_of_days), ymax = third_quartile_cases, ymin = first_qurtile_cases), fill = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.4) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = first_qurtile_cases), color = CONST_PLOT_FIRST_QUARTILE_LINE_COLOUR) + 
  # geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = third_quartile_cases), color = CONST_PLOT_THIRD_QUARTILE_LINE_COLOUR) +
  
  xlab("Simulation day") + 
  # ylab("Active cases") +
  ylab("Total active cases (for runs with total infection > 2)") +
  # ylim(0, 250) + 
  xlim(CONST_PLOT_START_DAY, CONST_PLOT_END_DAY) + 
  theme(legend.position = "right")


# Stop the clock
cat("\n")
cat(proc.time() - ptm)

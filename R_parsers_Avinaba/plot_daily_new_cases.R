# Script to plot daily new cases 

# Start the clock!
ptm <- proc.time()

# Test case
# DAILY_NEW_CASES_INPUT_DIR <- 'complex_model_v00_025_for_365d_output/scenarioAvin_0_to_30_control'

# Sim without interventions 
# DAILY_NEW_CASES_INPUT_DIR <- 'complex_model_v00_025_for_365d_1case_output/scenarioAvin_0_to_30_control'

# Sim with school closures when total active cases exceeds threshold
DAILY_NEW_CASES_INPUT_DIR <- 'complex_model_v00_w_school_closure_025_for_365d_1case_output/scenarioAvin_0_to_30_control'

# Sim without interventions 
# DAILY_NEW_CASES_INPUT_DIR <- 'complex_model_v00_025_for_365d_1case_01_output/scenarioAvin_0_to_30_control'

# Pattern matching files with names "schedules- ..... .json", as output from Citisketch
LIST_OF_DAILY_NEW_CASES_FILEPATH <- 
  list.files(DAILY_NEW_CASES_INPUT_DIR, full.names = TRUE, pattern="DAILY\\-NEW\\-CASES\\.csv$", ignore.case = TRUE)


# Empty list 
daily_cases_table_list <- list()

max_no_of_days <- 0

for(DAILY_NEW_CASES_FILENAME in LIST_OF_DAILY_NEW_CASES_FILEPATH){
  
  # Debug:
  cat(DAILY_NEW_CASES_FILENAME, "\n")
  
  # Read vaccination data 
  temp_daily_cases_table <- read.table(DAILY_NEW_CASES_FILENAME, sep=",")
  
  
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
new_cases_2D_mat <- array(rep(CONST_NOT_KNOWN, max_no_of_days * no_of_sims), 
                          dim = c(max_no_of_days, no_of_sims) )


for( i in 1:no_of_sims ){
  for( j in 1:length(daily_cases_table_list[[i]])){
    new_cases_2D_mat[j, i] <- daily_cases_table_list[[i]][[j]]
  }
  
}

mean_cases <- c()
median_cases <- c()
min_cases <- c()
max_cases <- c()
first_qurtile_cases <- c()
third_quartile_cases <- c()
# for( j in 1:no_of_sims){
#   temp_median <- median(new_cases_2D_mat[j, ])
#   median_cases[j] <- temp_median
# }

for(i in 1:max_no_of_days){
  # temp_median <- median(new_cases_2D_mat[i , ])
  temp_summary <- summary(new_cases_2D_mat[i , ])
  mean_cases[i] <- temp_summary["Mean"]
  median_cases[i] <- temp_summary["Median"]
  min_cases[i] <- temp_summary["Min."]
  max_cases[i] <- temp_summary["Max."]
  first_qurtile_cases[i] <- temp_summary["1st Qu."]
  third_quartile_cases[i] <- temp_summary["3rd Qu."]
  
}

# For viz 
library(tidyverse)

# Aesthetic
CONST_PLOT_START_DAY = 1
CONST_PLOT_END_DAY = 100
CONST_PLOT_LINE_COLOUR = "#F6871734"
CONST_PLOT_MEAN_LINE_COLOUR = "#F73B0C"

CONST_PLOT_FIRST_QUARTILE_LINE_COLOUR = "#230500"
CONST_PLOT_THIRD_QUARTILE_LINE_COLOUR = "#230500"

new_cases_df <- as.data.frame(new_cases_2D_mat)

#Summary stats
new_cases_df <- cbind(new_cases_df, mean_cases)
new_cases_df <- cbind(new_cases_df, median_cases)
new_cases_df <- cbind(new_cases_df, min_cases)
new_cases_df <- cbind(new_cases_df, max_cases)
new_cases_df <- cbind(new_cases_df, first_qurtile_cases)
new_cases_df <- cbind(new_cases_df, third_quartile_cases)

# Debug
# ggplot(data = new_cases_df) # Unsure what this plots

# ggplot(data = new_cases_df) + 
#   geom_point()


# plot_data <- ggplot(data = new_cases_df, mapping = aes(x = seq(1:max_no_of_days), y = V1))



# plot_data + 
#   geom_line()


ggplot() +
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V1), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V2), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V3), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V4), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V5), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V6), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V7), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V8), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V9), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V10), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V11), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V12), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V13), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V14), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V15), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V16), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V17), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V18), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V19), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V20), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V30), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V31), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V32), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V33), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V34), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V35), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V36), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V37), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V38), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V39), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V40), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V41), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V42), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V43), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V44), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V45), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V46), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V47), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V48), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V49), color = CONST_PLOT_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V50), color = CONST_PLOT_LINE_COLOUR) + 
  
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = mean_cases), color = CONST_PLOT_MEAN_LINE_COLOUR) + 
  
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = first_qurtile_cases), color = CONST_PLOT_FIRST_QUARTILE_LINE_COLOUR) + 
  geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = third_quartile_cases), color = CONST_PLOT_THIRD_QUARTILE_LINE_COLOUR) +
  
  xlab("Simulation day") + 
  ylab("New cases") + 
  xlim(CONST_PLOT_START_DAY, CONST_PLOT_END_DAY)


# geom_line(data = new_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = V5), color = CONST_PLOT_LINE_COLOUR) + # Stop the clock
cat(proc.time() - ptm)



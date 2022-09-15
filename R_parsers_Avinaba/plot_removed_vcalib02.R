# Script to plot daily new cases for calibration

# Start the clock!
ptm <- proc.time()

library(tidyr) # For drop_na
library(dplyr) # For summarise

# High level algo:
#
# 1. Read infection history to find outbreaks with more than 2 total infections
# 2. Read daily active cases and remove non-outbreaks
# 3. Repeat 1 to 2 for both sets of simulation runs, with and without school closures

# WARNING: The STATENAME column index of integer id and String name has been swapped from vcalib04 and above

# CSV Output dir
# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_26_05_22/' # Stat-writing disabled at tag: @Stat write
# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_26_05_22_Complex/'
# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_03_06_22_SIR/'

# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_03_06_22_Complex/'
# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_09_06_22_Complex/'

# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_07_07_22_SIR_30mat_Control/'
# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_07_07_22_SIR_30mat_vary_period/'
# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_07_07_22_SIR_30mat_vary_attendance/'
# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_07_07_22_SIR_30mat_vary_start_date/'
# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_07_07_22_SIR_30mat_IPDF'

# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_11_08_22_vcalib11_3day_tracing/'
# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_11_08_22_vcalib11_sameday_tracing/'

# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_12_08_22_vcalib11_compare_tracing/'
# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_13_08_22_vcalib11_compare_tracing/'
# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_13_08_22_vcalib11_compare_tracing_2/'
# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_14_08_22_vcalib13_compare_tracing/'
# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_18_08_22_vcalib13_compare_tracing/'

# OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toTeam_25_08_22_vcalib13_compare_voluntary_testing/'

OUTPUT_CSV_DIR = '/home/avin/Documents/Meta Design/Drafts/toJarod_14_09_22_vcalib13_compare_voluntary_testing/'

# Settings / Parameters (Note: the code exits without warning if no outbreak is found)
# CONST_MIN_TOTAL_INFECTION_FOR_OUTBREAK = 15
CONST_MIN_TOTAL_INFECTION_FOR_OUTBREAK = 0 # For Jarod 

# State names to look for as defined in simulation model
CONST_CHOSEN_DISEASE_MODEL <- "SIR"
# CONST_CHOSEN_DISEASE_MODEL <- "SIR + Testing + Isolation"
# CONST_CHOSEN_DISEASE_MODEL <- "SIR + Testing + Isolation + Contact Tracing"

# Note: this is loosely implemented with manual string matching
DISEASE_MODEL_NAMES <- c ("SIR",
                          "Complex",
                          "SIR + Testing + Isolation",
                          "SIR + Testing + Isolation + Contact Tracing")

# Disease model configuration check
# -----
if (CONST_CHOSEN_DISEASE_MODEL %in% DISEASE_MODEL_NAMES == FALSE){
  cat("\n Required disease model: ", CONST_CHOSEN_DISEASE_MODEL, " is not implemented yet", sep ="")
  cat("\nTerminating simulation\n", sep = "")
  cat("\n", proc.time() - start_time, "\n")
  stop()
}

if (CONST_CHOSEN_DISEASE_MODEL == "Complex"){
  # Complex model
  # ----------------
  # Regex pattern: \"([0-9][0-9])\_.+\"
  # xx_<recovered/dead>, where xx -> no. of doses of vaccine
  # Note: Triple dosed or single boosted states for non-omicron variants are not implemented as of 4th March 2022

  # Aggregate removed
  STATENAMES_REMOVED = c("00_recovered",
                        "00_dead",
                        "01_recovered",
                        "01_dead",
                        "02_recovered",
                        "02_dead",

                        "00_recovered_omicron",
                        "00_dead_omicron",
                        "01_recovered_omicron",
                        "01_dead_omicron",
                        "02_recovered_omicron",
                        "02_dead_omicron",
                        "03_recovered_omicron",
                        "03_dead_omicron")

  STATENAMES_INFECTIOUS = c("00_pre_symptomatic_non_isolated",
                        "00_mild_non_isolated",
                        "01_pre_symptomatic_non_isolated",
                        "01_mild_non_isolated",
                        "02_pre_symptomatic_non_isolated",
                        "02_mild_non_isolated",

                        "00_pre_symptomatic_non_isolated_omicron",
                        "00_mild_non_isolated_omicron",
                        "01_pre_symptomatic_non_isolated_omicron",
                        "01_mild_non_isolated_omicron",
                        "02_pre_symptomatic_non_isolated_omicron",
                        "02_mild_non_isolated_omicron",
                        "03_pre_symptomatic_non_isolated_omicron",
                        "03_mild_non_isolated_omicron")

  STATENAMES_ALL_INFECTED = c("00_latent_infections_not_isolated",
                              "00_latent_infections_isolated",
                              
                              "00_pre_symptomatic_non_isolated",
                              "00_pre_symptomatic_isolated",
                              
                              "00_mild_non_isolated",
                              "00_mild_isolated",
                              
                              "00_hospitalized",
                              "00_hospitalized_ICU",
                              
                              
                              
                              "01_latent_infections_not_isolated",
                              "00_latent_infections_isolated",
                              
                              "01_pre_symptomatic_non_isolated",
                              "01_pre_symptomatic_isolated",
                              
                              "01_mild_non_isolated",
                              "01_mild_isolated",
                              
                              "01_hospitalized",
                              "01_hospitalized_ICU",
                              
                              
                              
                              "02_latent_infections_not_isolated",
                              "02_latent_infections_isolated",
                              
                              "02_pre_symptomatic_non_isolated",
                              "02_pre_symptomatic_isolated",
                              
                              "02_mild_non_isolated",
                              "02_mild_isolated",
                              
                              "02_hospitalized",
                              "02_hospitalized_ICU",
                              
                              
                              
                              "00_latent_infections_not_isolated_omicron",
                              "00_latent_infections_isolated_omicron",
                              
                              "00_pre_symptomatic_non_isolated_omicron",
                              "00_pre_symptomatic_isolated_omicron",
                              
                              "00_mild_non_isolated_omicron",
                              "00_mild_isolated_omicron",
                              
                              "00_hospitalized_omicron",
                              "00_hospitalized_ICU_omicron",
                              
                              
                              
                              "01_latent_infections_not_isolated_omicron",
                              "00_latent_infections_isolated_omicron",
                              
                              "01_pre_symptomatic_non_isolated_omicron",
                              "01_pre_symptomatic_isolated_omicron",
                              
                              "01_mild_non_isolated_omicron",
                              "01_mild_isolated_omicron",
                              
                              "01_hospitalized_omicron",
                              "01_hospitalized_ICU_omicron",
                              
                              
                              
                              "02_latent_infections_not_isolated_omicron",
                              "02_latent_infections_isolated_omicron",
                              
                              "02_pre_symptomatic_non_isolated_omicron",
                              "02_pre_symptomatic_isolated_omicron",
                              
                              "02_mild_non_isolated_omicron",
                              "02_mild_isolated_omicron",
                              
                              "02_hospitalized_omicron",
                              "02_hospitalized_ICU_omicron",
                              
                              
                              
                              "03_latent_infections_not_isolated_omicron",
                              "03_latent_infections_isolated_omicron",
                              
                              "03_pre_symptomatic_non_isolated_omicron",
                              "03_pre_symptomatic_isolated_omicron",
                              
                              "03_mild_non_isolated_omicron",
                              "03_mild_isolated_omicron",
                              
                              "03_hospitalized_omicron",
                              "03_hospitalized_ICU_omicron")


} else if(CONST_CHOSEN_DISEASE_MODEL == "SIR"){

  # SIR (without vaccines)
  # ----------------
  STATENAMES_REMOVED = c("removed")
  STATENAMES_INFECTIOUS = c("infected")
  STATENAMES_ALL_INFECTED = c("infected")

} else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){

  # SIR + Testing + Isolation
  # ----------------
  STATENAMES_REMOVED = c("removed")
  STATENAMES_INFECTIOUS = c("(unconfirmed) infected")
  STATENAMES_ALL_INFECTED = c("(unconfirmed) infected", "(isolated + unconfirmed) infected", "(isolated + confirmed) infected")

} else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){

  # SIR + Testing + Isolation + Contact Tracing
  # ----------------
  STATENAMES_REMOVED = c("removed")
  STATENAMES_INFECTIOUS = c("(unconfirmed) infected")
  STATENAMES_ALL_INFECTED = c("(unconfirmed) infected", "(isolated + unconfirmed) infected", "(isolated + confirmed) infected")
}



# Sim output directory for runs without closures

# CONTROL_INPUT_DIR <- 'LocalDebug_Campbellton_SIR_with_voluntary_testing_random_import_vcalib13_00300_1mat_for_210d_aggregated_output/SIR_calib_contact_matrix'
CONTROL_INPUT_DIR <- 'LocalDebug_Campbellton_SIR_classic_random_import_vcalib13_00300_1mat_for_210d_aggregated_output/SIR_calib_contact_matrix'

# CONTROL_INPUT_DIR <- 'Benchmark_SIR_frozen_import_refactor_vcalib06_00200_for_210d_output/SIR_calib_contact_matrix'

#--- Riverview - SIR runs - Location wise - No damping
# CONTROL_INPUT_DIR = 'Riverview_SIR_frozen_import_refactor_vcalib07_00300_for_210d_location_wise_output/Riverview_Schedule_02Jun22'
# CONTROL_INPUT_DIR = 'Riverview_SIR_frozen_import_refactor_vcalib07_00400_for_210d_location_wise_output/Riverview_Schedule_02Jun22'
# CONTROL_INPUT_DIR = 'Riverview_SIR_frozen_import_refactor_vcalib07_00500_for_210d_location_wise_output/Riverview_Schedule_02Jun22'
# CONTROL_INPUT_DIR = 'Riverview_SIR_frozen_import_refactor_vcalib07_00600_for_210d_location_wise_output/Riverview_Schedule_02Jun22'
# CONTROL_INPUT_DIR = 'Riverview_SIR_frozen_import_refactor_vcalib07_00700_for_210d_location_wise_output/Riverview_Schedule_02Jun22'
# CONTROL_INPUT_DIR = 'Riverview_SIR_frozen_import_refactor_vcalib07_00800_for_210d_location_wise_output/Riverview_Schedule_02Jun22'

# CONTROL_INPUT_DIR = 'Riverview_SIR_random_import_refactor_vcalib07_00300_for_210d_location_wise_output/Riverview_Schedule_02Jun22'

# CONTROL_INPUT_DIR = 'Riverview_Complex_random_import_refactor_vcalib07_00300_for_210d_location_wise_output/Riverview_Schedule_02Jun22'

# CONTROL_INPUT_DIR = 'Riverview_SIR_random_import_refactor_vcalib07_00300_for_210d_location_wise_output/Riverview_Schedule_02Jun22'

# CONTROL_INPUT_DIR = 'Riverview_09Jun22_SIR_random_import_refactor_vcalib07_00300_for_210d_location_wise_output/Riverview_Schedule_09Jun22'



#--- End of Riverview runs ----

#--- Campbellton  ----

# - - Location wise -- Control 1day -- for Contact tracing
# - - - Same day tracing --
# CONTROL_INPUT_DIR = 'Campbellton_SIRplusplusplus_sameday_tracing_random_import_vcalib11_00300_1mat_for_210d_location_based_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Campbellton_SIRplusplusplus_sameday_tracing_random_import_vcalib11_00200_1mat_for_210d_location_based_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Campbellton_SIRplusplusplus_sameday_tracing_random_import_vcalib11_00100_1mat_for_210d_location_based_output/SIR_calib_contact_matrix'
# - - - End of Same day tracing

# - - - Delayed tracing
# CONTROL_INPUT_DIR = 'Campbellton_SIRplusplusplus_delay03_tracing_random_import_vcalib11_00300_1mat_for_210d_location_based_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Campbellton_SIRplusplusplus_delay03_tracing_random_import_vcalib11_00200_1mat_for_210d_location_based_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Campbellton_SIRplusplusplus_delay03_tracing_random_import_vcalib11_00100_1mat_for_210d_location_based_output/SIR_calib_contact_matrix'
# - - - End of Delayed tracing
# - - End of Location wise - - Control 1day -- for Contact tracing

# - - Location wise -- Control 30 day -- only voluntary testing - NO Contact Tracing

# CONTROL_INPUT_DIR = 'Campbellton_SIR_only_voluntary_testing_random_import_vcalib13_00300_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_only_voluntary_testing_random_import_vcalib13_00400_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_only_voluntary_testing_random_import_vcalib13_00500_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_only_voluntary_testing_random_import_vcalib13_00600_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_only_voluntary_testing_random_import_vcalib13_00700_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_only_voluntary_testing_random_import_vcalib13_00800_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'

# - - End of Location wise -- Control 30 day -- only voluntary testing - NO Contact Tracing


# - - Location wise -- Control 30 day -- for Contact Tracing
# - - - Same day tracing --
# CONTROL_INPUT_DIR = 'Campbellton_SIR_sameday_tracing_random_import_vcalib13_00300_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_sameday_tracing_random_import_vcalib13_00400_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_sameday_tracing_random_import_vcalib13_00500_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_sameday_tracing_random_import_vcalib13_00600_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_sameday_tracing_random_import_vcalib13_00700_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_sameday_tracing_random_import_vcalib13_00800_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'

# - - - Delayed tracing
# CONTROL_INPUT_DIR = 'Campbellton_SIR_delay03_tracing_random_import_vcalib13_00300_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_delay03_tracing_random_import_vcalib13_00400_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_delay03_tracing_random_import_vcalib13_00500_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_delay03_tracing_random_import_vcalib13_00600_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_delay03_tracing_random_import_vcalib13_00700_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_delay03_tracing_random_import_vcalib13_00800_30mat_for_210d_location_based_output/schedules_Campbellton_Control_Jan_12_2022'

# - - End of Location wise - - Control 30 day -- for Contact tracing


# - - Location wise -- Control 30days -- for Event
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00300_30mat_for_210d_location_wise_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00400_30mat_for_210d_location_wise_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00500_30mat_for_210d_location_wise_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00600_30mat_for_210d_location_wise_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00700_30mat_for_210d_location_wise_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00800_30mat_for_210d_location_wise_output/schedules_Campbellton_Control_Jan_12_2022'

# - - - - Location wise -- 30days -- varying event period
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00300_30mat_for_210d_location_wise_w_simulated_event_200agent_20_to_20_output/schedules_Campbellton_Control_Jan_12_2022' # 1 day event
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00300_30mat_for_210d_location_wise_w_simulated_event_200agent_20_to_21_output/schedules_Campbellton_Control_Jan_12_2022' # 2 day event
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00300_30mat_for_210d_location_wise_w_simulated_event_200agent_20_to_22_output/schedules_Campbellton_Control_Jan_12_2022' # 3 day event
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00300_30mat_for_210d_location_wise_w_simulated_event_200agent_20_to_23_output/schedules_Campbellton_Control_Jan_12_2022' # 4 day event

# - - - - Location wise -- 30days -- varying event attendance
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00300_30mat_for_210d_location_wise_w_simulated_event_25agent_20_to_23_output/schedules_Campbellton_Control_Jan_12_2022' # 25 attendees
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00300_30mat_for_210d_location_wise_w_simulated_event_50agent_20_to_23_output/schedules_Campbellton_Control_Jan_12_2022' # 50 attendees
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00300_30mat_for_210d_location_wise_w_simulated_event_100agent_20_to_23_output/schedules_Campbellton_Control_Jan_12_2022' # 100 attendees
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00300_30mat_for_210d_location_wise_w_simulated_event_400agent_20_to_23_output/schedules_Campbellton_Control_Jan_12_2022' # 400 attendees

# - - - - Location wise -- 30days -- varying event start date
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00300_30mat_for_210d_location_wise_w_simulated_event_200agent_1_to_4_output/schedules_Campbellton_Control_Jan_12_2022' # starts on day 1
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00300_30mat_for_210d_location_wise_w_simulated_event_200agent_9_to_12_output/schedules_Campbellton_Control_Jan_12_2022' # starts on day 9
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00300_30mat_for_210d_location_wise_w_simulated_event_200agent_17_to_20_output/schedules_Campbellton_Control_Jan_12_2022' # starts on day 17
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00300_30mat_for_210d_location_wise_w_simulated_event_200agent_28_to_31_output/schedules_Campbellton_Control_Jan_12_2022' # starts on day 28
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00300_30mat_for_210d_location_wise_w_simulated_event_200agent_36_to_39_output/schedules_Campbellton_Control_Jan_12_2022' # starts on day 36
# CONTROL_INPUT_DIR = 'Campbellton_SIR_random_import_vcalib08_00300_30mat_for_210d_location_wise_w_simulated_event_200agent_41_to_44_output/schedules_Campbellton_Control_Jan_12_2022' # starts on day 41

# - - Location wise -- Control 30days -- IPDF
# CONTROL_INPUT_DIR = 'Campbellton_SIR_w_IPDF_random_import_vcalib09_00300_30mat_for_210d_location_wise_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_w_IPDF_random_import_vcalib09_00400_30mat_for_210d_location_wise_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_w_IPDF_random_import_vcalib09_00500_30mat_for_210d_location_wise_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_w_IPDF_random_import_vcalib09_00600_30mat_for_210d_location_wise_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_w_IPDF_random_import_vcalib09_00700_30mat_for_210d_location_wise_output/schedules_Campbellton_Control_Jan_12_2022'
# CONTROL_INPUT_DIR = 'Campbellton_SIR_w_IPDF_random_import_vcalib09_00800_30mat_for_210d_location_wise_output/schedules_Campbellton_Control_Jan_12_2022'
# - - End of Location wise -- Control 30days -- IPDF




#--- SIR runs
# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib05_00300_for_210d_location_wise_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib05_00400_for_210d_location_wise_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib05_00500_for_210d_location_wise_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib05_00600_for_210d_location_wise_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib05_00700_for_210d_location_wise_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib05_00800_for_210d_location_wise_output/SIR_calib_contact_matrix'

# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib05_00300_for_210d_location_wise_halfSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib05_00400_for_210d_location_wise_halfSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib05_00500_for_210d_location_wise_halfSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib05_00600_for_210d_location_wise_halfSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib05_00700_for_210d_location_wise_halfSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib05_00800_for_210d_location_wise_halfSchool_output/SIR_calib_contact_matrix'

# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib06_00300_for_210d_location_wise_zeroSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib06_00400_for_210d_location_wise_zeroSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib06_00500_for_210d_location_wise_zeroSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib06_00600_for_210d_location_wise_zeroSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib06_00700_for_210d_location_wise_zeroSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'SIR_frozen_import_refactor_vcalib06_00800_for_210d_location_wise_zeroSchool_output/SIR_calib_contact_matrix'


#--- Complex runs - Damping Schools
# CONTROL_INPUT_DIR = 'Campbellton_Complex_random_import_refactor_vcalib07_00300_for_210d_location_wise_output/SIR_calib_contact_matrix'

# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00300_for_210d_location_wise_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00400_for_210d_location_wise_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00500_for_210d_location_wise_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00600_for_210d_location_wise_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00700_for_210d_location_wise_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00800_for_210d_location_wise_output/SIR_calib_contact_matrix'

# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00300_for_210d_location_wise_halfSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00400_for_210d_location_wise_halfSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00500_for_210d_location_wise_halfSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00600_for_210d_location_wise_halfSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00700_for_210d_location_wise_halfSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00800_for_210d_location_wise_halfSchool_output/SIR_calib_contact_matrix'

# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00300_for_210d_location_wise_zeroSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00400_for_210d_location_wise_zeroSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00500_for_210d_location_wise_zeroSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00600_for_210d_location_wise_zeroSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00700_for_210d_location_wise_zeroSchool_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib06_00800_for_210d_location_wise_zeroSchool_output/SIR_calib_contact_matrix'

#--- Complex runs - Damping Commercial
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00300_for_210d_location_wise_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00400_for_210d_location_wise_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00500_for_210d_location_wise_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00600_for_210d_location_wise_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00700_for_210d_location_wise_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00800_for_210d_location_wise_output/SIR_calib_contact_matrix'

# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00300_for_210d_location_wise_halfCommercial_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00400_for_210d_location_wise_halfCommercial_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00500_for_210d_location_wise_halfCommercial_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00600_for_210d_location_wise_halfCommercial_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00700_for_210d_location_wise_halfCommercial_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00800_for_210d_location_wise_halfCommercial_output/SIR_calib_contact_matrix'

# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00300_for_210d_location_wise_zeroCommercial_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00400_for_210d_location_wise_zeroCommercial_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00500_for_210d_location_wise_zeroCommercial_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00600_for_210d_location_wise_zeroCommercial_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00700_for_210d_location_wise_zeroCommercial_output/SIR_calib_contact_matrix'
# CONTROL_INPUT_DIR = 'Complex_frozen_import_refactor_vcalib07_00800_for_210d_location_wise_zeroCommercial_output/SIR_calib_contact_matrix'

# Helper functions
# -------------------  

# Gets a vector of double differentiated all infected agent per day 
# 
getLongestSequenceOfTrue <- function(vector_of_true_n_false){
  list_of_index_pairs <- list()
  temp_true_sequence_index_pair <- c()
  
  temp_beg_index = 0
  temp_end_index = 0
  temp_sequence_length = 0
  
  vector_of_sequence_length <- c()
  
  max_length_index <- c(temp_beg_index, temp_end_index)
  
  # TODO: Guard clause to reject any other kind of vector
  
  length_of_input_vector = length(vector_of_true_n_false)
  
  if(length_of_input_vector <= 0){
    cat("\n[WARNING] Empty input list, Returning (0,0) ")
  } else{
    
    FLAG_within_sequence = FALSE
    
    for(scan_index in 1:length_of_input_vector){
      
      if(vector_of_true_n_false[scan_index] == TRUE){
        # A TRUE is found at scan_index
        
        if(FLAG_within_sequence == FALSE){
          # A new sequence is found 
          FLAG_within_sequence = TRUE
          
          # set the sequence beginning and ending index 
          temp_beg_index = scan_index
          temp_end_index = scan_index
          
          # Reset the sequence length
          temp_sequence_length = 1

          
        } else if(FLAG_within_sequence == TRUE){
          # A continuing sequence
          
          # Shift sequence ending index
          temp_end_index = scan_index
          
          # Add to sequence length
          temp_sequence_length = temp_sequence_length + 1
        }
      } else{
        # Anything else than a TRUE is found at scan index
        if(FLAG_within_sequence == TRUE){
          # This marks an end to active sequence was being counted 
          FLAG_within_sequence = FALSE
          
          # Create a new true sequence index pair 
          temp_true_sequence_index_pair <- c(temp_beg_index, temp_end_index)
          # Add to list of indices 
          list_of_index_pairs[[ length(list_of_index_pairs) + 1]] <- temp_true_sequence_index_pair
          # Add to list of sequence length
          vector_of_sequence_length[ length(vector_of_sequence_length) + 1] <- temp_sequence_length
        } else if(FLAG_within_sequence == FALSE){
          # Do nothing
        }
      }
      
    }
  }
  
  # return(max_length_index)
  # return(list_of_index_pairs)
  # return(vector_of_sequence_length)
  
  # Get the first longest sequence of TRUE
  longest_sequence_index = min(which(vector_of_sequence_length == max(vector_of_sequence_length)))
  
  # Debug: 
  # cat("\n Longest sequence index: ", longest_sequence_index)
  # cat("\n Sequence lengths: ", vector_of_sequence_length)
  # cat("\n Index pairs: ")
  # print(list_of_index_pairs)
  
  return(list_of_index_pairs[[longest_sequence_index]])
}

# Overall statistics book-keeping
# ------------------------------
run_stat_mat <- matrix (ncol = 18, nrow = 1)
run_stat_mat_colnames <- c("amplitude",
                           "observed peak mean",
                           "observed peak std",
                           "actual peak mean",
                           "actual peak std",
                           "removed mean",
                           "removed std",
                           "r0 estimated mean",
                           "r0 growth mean",
                           "r0 growth std",
                           "n of runs",
                           "n of outbreaks",
                           "% of outbreaks",
                           "sim setup time mean",
                           "sim setup time std",
                           "sim run time mean",
                           "sim run time std",
                           "sim dir"
)
colnames(run_stat_mat) <- run_stat_mat_colnames

runs_amplitude <- NULL

runs_peak_vector <- c()
runs_peak_mean <- NULL
runs_peak_std <- NULL

runs_actual_peak_vector <- c()
runs_actual_peak_mean <- NULL
runs_actual_peak_std <- NULL

runs_removed_vector <- c()
runs_removed_mean <- NULL
runs_removed_std <- NULL

# Note: Right now, assumes all runs in a folder has the same no. of initial population
runs_total_sim_pop <- NULL

runs_r0 <- NULL

runs_r0_actual_vector <- c()
runs_r0_actual_mean <- NULL
runs_r0_actual_std <- NULL

runs_n_of_runs <- NULL
runs_n_of_outbreaks <- NULL
runs_percent_of_outbreaks <- NULL

runs_dir <- CONTROL_INPUT_DIR


# Pattern matching files with names "*DAILY-ACTIVE-CASES.csv", as output from Citisketch
LIST_OF_DAILY_ACTIVE_CASES_FILEPATH <-
  list.files(CONTROL_INPUT_DIR, full.names = TRUE, pattern="DAILY\\-ACTIVE\\-CASES\\.csv$", ignore.case = TRUE)

LIST_OF_STATE_MATRIX_FILEPATH <-
  list.files(CONTROL_INPUT_DIR, full.names = TRUE, pattern="STATE\\.csv$", ignore.case = TRUE)

LIST_OF_STATENAME_FILEPATH <-
  list.files(CONTROL_INPUT_DIR, full.names = TRUE, pattern="STATENAME\\.csv$", ignore.case = TRUE)

LIST_OF_CONFIG_FILEPATH <-
  list.files(CONTROL_INPUT_DIR, full.names = TRUE, pattern="CONFIG\\.csv$", ignore.case = TRUE)

LIST_OF_CONFIG_FILEPATH <-
  list.files(CONTROL_INPUT_DIR, full.names = TRUE, pattern="CONFIG\\.csv$", ignore.case = TRUE)

LIST_OF_INFECTION_HISTORY_FILEPATH <-
  list.files(CONTROL_INPUT_DIR, full.names = TRUE, pattern="-INFECTION\\-HISTORY\\.csv$", ignore.case = TRUE)


# Empty list
daily_cases_table_list <- list()
outbreak_run_index_list <- c()
non_outbreak_run_index_list <- c()
infectious_period <- c()
sim_runtime_list <- c()
sim_setuptime_list <- c()
infection_history_table_list <- list()

total_removed_vector_list <- list() # Add vectors of individuals in "removed" state
# total_removed_vector_list_index <- 1 #
total_actual_infectious_vector_list <- list() # Add vectors of individuals in infectious state(s)

total_infected_vector_list <- list() 

max_no_of_days <- 0

# Note: Assumes all of outputs for a run "X" (X-DAILY-ACTIVE-CASES, X-STATE, X-STATENAME, X-CONFIG) is available
#       and are indexed numerically the same upon reading as list above
for(index in 1:length(LIST_OF_DAILY_ACTIVE_CASES_FILEPATH)){

  # Read run metadata
  temp_config_table <- read.table(LIST_OF_CONFIG_FILEPATH[index], sep = ",", header = TRUE)
  if(
    ("CONST_MAX_CHANCE" %in% colnames(temp_config_table)) & # Has the key to be extracted
    (nrow(temp_config_table) > 1) # At least has a row
    ){
    runs_amplitude <- levels(temp_config_table["CONST_MAX_CHANCE"][[1]])[1]
  }

  if(
    ("sim_setup_time" %in% colnames(temp_config_table)) & # Has the key to be extracted
    (nrow(temp_config_table) > 1) # At least has a row
  ){
    sim_setuptime_list <- c(sim_setuptime_list, as.numeric(levels(temp_config_table["sim_setup_time"][[1]])[1]) )
  }

  if(
    ("sim_run_time" %in% colnames(temp_config_table)) & # Has the key to be extracted
    (nrow(temp_config_table) > 1) # At least has a row
  ){
    sim_runtime_list <- c(sim_runtime_list, as.numeric(levels(temp_config_table["sim_run_time"][[1]])[1]) )
  }

  # Read infection history
  # temp_infection_history_table <- read.table(LIST_OF_INFECTION_HISTORY_FILEPATH[index], sep = ",", header = TRUE)

  # Debug:
  # cat(LIST_OF_INFECTION_HISTORY_FILEPATH[index], "\n")

  # Read daily cases data
  temp_daily_cases_table <- read.table(LIST_OF_DAILY_ACTIVE_CASES_FILEPATH[index], sep=",")


  # Append to list
  daily_cases_table_list <- append(daily_cases_table_list, temp_daily_cases_table)

  temp_max_no_of_days <- max(lengths(temp_daily_cases_table))

  # Get max no of days
  if(max_no_of_days < temp_max_no_of_days){
    max_no_of_days <- temp_max_no_of_days
  }

  # Check for max of active cases
  temp_max_active_cases <- max(temp_daily_cases_table)

  # Debug:
  # cat("Run ", index, ", has peak infections: ", temp_max_active_cases, "\n", sep = "")

  # Append to list if total no. of infections are greater than 15
  if(temp_max_active_cases > CONST_MIN_TOTAL_INFECTION_FOR_OUTBREAK){

    # Debug:
    # cat("\nOutbreak found for run index: ",  index, ", filename: ", LIST_OF_STATE_MATRIX_FILEPATH[index], "\n", sep ="")

    outbreak_run_index_list <- c(outbreak_run_index_list, index)

    # Build the removed till date table
    # -----
    # 0. Read the STATNAME data and find the numeric indices for REMOVED states
    # 1. Read the full state per day matrix
    # 2. Aggregate at removed state
    temp_state_names_table <- read.table(LIST_OF_STATENAME_FILEPATH[index], sep=",")

    # Debug:
    # print(temp_state_names_table)

    temp_state_matrix_table <- read.table(LIST_OF_STATE_MATRIX_FILEPATH[index], sep=",")


    # Get infectious period of all simulated agents (including non-infected)
    temp_STATELIST_INFECTIOUS <- sapply(temp_state_names_table, function(r) which(r %in% STATENAMES_INFECTIOUS))$V2 # WARNING: This switches to $V2 from vcalib04+
    temp_infectious_period <- apply(temp_state_matrix_table, 1, function(r) length(which(r %in% temp_STATELIST_INFECTIOUS)))

    
    # Get the integer states for all infected agents (both known and unknown)
    temp_STATELIST_ALL_INFECTED <- sapply(temp_state_names_table, function(r) which(r %in% STATENAMES_ALL_INFECTED))$V2
    
    # Remove non-infected agents
    temp_indices_not_infected <- which(temp_infectious_period == 0)
    temp_infectious_period <- temp_infectious_period[ -temp_indices_not_infected]

    # Add to book-keeping across all runs
    infectious_period <- c(infectious_period, temp_infectious_period)



    temp_STATELIST_REMOVED <- sapply(temp_state_names_table, function(r) which(r %in% STATENAMES_REMOVED))$V2 # WARNING: This switches to $V2 from vcalib04+







    temp_removed_per_day_single_run <- c()
    temp_actual_infectious_per_day_single_run <- c()
    temp_total_infected_per_day_single_run <- c()
    # iterate over columns
    for(day_index in 1:length(names(temp_state_matrix_table))){
      # print(day_index)
      # print(temp_state_matrix_table[[names(temp_state_matrix_table)[day_index]]]) # Read as vector

      # Aggregate removed
      temp_removed_on_this_day <- length(which(temp_state_matrix_table[[names(temp_state_matrix_table)[day_index]]] %in% temp_STATELIST_REMOVED))

      # Append to vector
      temp_removed_per_day_single_run <- c(temp_removed_per_day_single_run, temp_removed_on_this_day)


      # Aggregate unobserved infectious
      temp_actual_infectious_on_this_day <- length(which(temp_state_matrix_table[[names(temp_state_matrix_table)[day_index]]] %in% temp_STATELIST_INFECTIOUS))

      # Append to vector
      temp_actual_infectious_per_day_single_run <- c(temp_actual_infectious_per_day_single_run, temp_actual_infectious_on_this_day)
      
      
      # Aggregate unobserved + observed infectious
      temp_infected_on_this_day <- length(which(temp_state_matrix_table[[names(temp_state_matrix_table)[day_index]]] %in% temp_STATELIST_ALL_INFECTED))
      
      # Append to vector 
      temp_total_infected_per_day_single_run <- c(temp_total_infected_per_day_single_run, temp_infected_on_this_day)

    }

    # Append to list of vectors
    # total_removed_vector_list <- append(total_removed_vector_list, temp_removed_per_day_single_run) # Garbles the dimensions
    total_removed_vector_list[[length(total_removed_vector_list) + 1]] <- temp_removed_per_day_single_run
    total_actual_infectious_vector_list[[length(total_actual_infectious_vector_list) + 1]] <- temp_actual_infectious_per_day_single_run
    total_infected_vector_list[[length(total_infected_vector_list) + 1]] <- temp_total_infected_per_day_single_run
    
    # print("\nBreakpoint here")
    
    # Find the exponential growth time window for this run
    time_window_vector <- getLongestSequenceOfTrue( diff( diff( temp_total_infected_per_day_single_run ) ) > 0)
    # Note: hacky implementation 
    time_window_beg <- time_window_vector[1]
    time_window_end <- time_window_vector[2] + 2 # Extra 2 days to account for double derivative
    time_window_sim_days <- time_window_beg:time_window_end

    # Read infection history
    temp_infection_history_table <- read.table(LIST_OF_INFECTION_HISTORY_FILEPATH[index], sep = ",", header = TRUE)
    infection_history_table_list[[length(infection_history_table_list) + 1]] <- temp_infection_history_table
    
    # Filter by exponential growth time window
    temp_growth_phase_infection_history_table <- subset(temp_infection_history_table, `infected.on` %in% time_window_sim_days)
    
    # Group by infectors
    # transmission_count_summary_df <- temp_infection_history_table["infected.by"] %>% drop_na(`infected.by`) %>% group_by(`infected.by`) %>% summarise(count_transmissions = n())
    transmission_count_summary_df <- temp_growth_phase_infection_history_table["infected.by"] %>% drop_na(`infected.by`) %>% group_by(`infected.by`) %>% summarise(count_transmissions = n())
    
    # Calcuate R0s
    temp_r0 <- mean(transmission_count_summary_df$count_transmissions)

    # Append to vector
    runs_r0_actual_vector <- c(runs_r0_actual_vector, temp_r0)

  }
  else{
    non_outbreak_run_index_list <- c(non_outbreak_run_index_list, index)
  }
}

no_of_sims <- length(daily_cases_table_list)
no_of_outbreak_sims <- length(outbreak_run_index_list)

# Book-keeping semantics
runs_n_of_runs <- no_of_sims
runs_n_of_outbreaks <- no_of_outbreak_sims
runs_percent_of_outbreaks <- runs_n_of_outbreaks / runs_n_of_runs

runs_total_sim_pop <- dim(temp_state_matrix_table)[[1]]

runs_r0_actual_mean <- mean(runs_r0_actual_vector)
runs_r0_actual_std <- sd(runs_r0_actual_vector)

cat("\nRuns found in directory: ", CONTROL_INPUT_DIR, sep = "")
cat("\n  Disease model: ", CONST_CHOSEN_DISEASE_MODEL, sep = "")
cat("\n  Amplitude: ", runs_amplitude, sep = "")
cat("\n  No. of runs: ", length(LIST_OF_DAILY_ACTIVE_CASES_FILEPATH), sep = "")
cat("\n  No. of outbreak runs (for peak infected >", CONST_MIN_TOTAL_INFECTION_FOR_OUTBREAK, "): ", length(outbreak_run_index_list), sep = "")
cat("\n  Percent outbreak (for peak infected >", CONST_MIN_TOTAL_INFECTION_FOR_OUTBREAK, "): ", (length(outbreak_run_index_list) / length(LIST_OF_DAILY_ACTIVE_CASES_FILEPATH)) * 100, sep = "")
cat("\n  Non-outbreak runs to remove from plot: ")
cat("\n", non_outbreak_run_index_list, sep = ", ")
cat("\n  No. of non outbreak runs: ", length(non_outbreak_run_index_list), sep = "")
cat("\n")
cat("\n---------")
cat("\n Output directory: ", OUTPUT_CSV_DIR)
cat("\n---------")


# Debug value
CONST_NOT_KNOWN = -1

# Translate active cases to matrix
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

std_dev <- c()

# Dr.Seahra: Visualise with 20% band + 10% band to cover 80% of outcomes in a contour like viz
thirty_eth_quantile_cases <- c()
forty_eth_quantile_cases <- c()
sixty_eth_quantile_cases <- c()
seventy_eth_quantile_cases <- c()


# TODO: Change to ODS tab
OUTPUT_CSV_FILEPATH <- paste(OUTPUT_CSV_DIR,"DAILY-ACTIVE-CASES-", CONST_CHOSEN_DISEASE_MODEL, runs_amplitude, "-ALL-RUNS.csv",  sep = "")

# write all Daily new cases
write.table(active_cases_2D_mat, sep=",", OUTPUT_CSV_FILEPATH, row.names = FALSE, col.names = TRUE)


# Remove non outbreak cases
#
# Cases where peak infection doesn't go beyond 15
# active_cases_2D_mat <- active_cases_2D_mat[ , - c(7, 8, 9, 11, 16, 19, 21, 25, 27, 31, 35, 39, 43, 46, 49, 51, 56, 59, 60, 62, 65, 73, 79, 80, 81, 83, 85, 86, 91, 92, 94, 97, 104, 105, 106, 107, 108, 111, 112, 114, 115, 116, 121, 124, 128, 136, 141, 147, 156, 157, 165, 184, 185, 186, 187, 189, 191, 192, 195, 197, 200) ]

if (length(non_outbreak_run_index_list) > 0){
  active_cases_2D_mat <- active_cases_2D_mat[ , - non_outbreak_run_index_list]
}

# Get peaks and related stats for all outbreaks
runs_peak_vector <- apply(active_cases_2D_mat, 2, max)
runs_peak_mean <- mean(runs_peak_vector)
runs_peak_std <- sd(runs_peak_vector)

#
for(i in 1:max_no_of_days){
  temp_summary <- summary(active_cases_2D_mat[i , ])

  mean_cases[i] <- temp_summary["Mean"]
  median_cases[i] <- temp_summary["Median"]
  min_cases[i] <- temp_summary["Min."]
  max_cases[i] <- temp_summary["Max."]
  first_qurtile_cases[i] <- temp_summary["1st Qu."]
  third_quartile_cases[i] <- temp_summary["3rd Qu."]

  std_dev[i] <- sd(active_cases_2D_mat[i , ])


  temp_quantile_vector <- quantile(active_cases_2D_mat[i, ], prob = c(0.3, 0.4, 0.6, 0.7))
  thirty_eth_quantile_cases[i] <- temp_quantile_vector[["30%"]]
  forty_eth_quantile_cases[i] <- temp_quantile_vector[["40%"]]
  sixty_eth_quantile_cases[i] <- temp_quantile_vector[["60%"]]
  seventy_eth_quantile_cases[i] <- temp_quantile_vector[["70%"]]

}

active_cases_df <- as.data.frame(active_cases_2D_mat)

#Summary stats
active_cases_df <- cbind(active_cases_df, mean_cases)
active_cases_df <- cbind(active_cases_df, median_cases)
active_cases_df <- cbind(active_cases_df, min_cases)
active_cases_df <- cbind(active_cases_df, max_cases)
active_cases_df <- cbind(active_cases_df, first_qurtile_cases)
active_cases_df <- cbind(active_cases_df, third_quartile_cases)

active_cases_df <- cbind(active_cases_df, std_dev)

active_cases_df <- cbind(active_cases_df, thirty_eth_quantile_cases)
active_cases_df <- cbind(active_cases_df, forty_eth_quantile_cases)
active_cases_df <- cbind(active_cases_df, sixty_eth_quantile_cases)
active_cases_df <- cbind(active_cases_df, seventy_eth_quantile_cases)

# TODO: Change to ODS tab
OUTPUT_CSV_FILEPATH <- paste(OUTPUT_CSV_DIR,"DAILY-ACTIVE-CASES-", CONST_CHOSEN_DISEASE_MODEL, runs_amplitude, "-ONLY-OUTBREAKS.csv",  sep = "")

# write all Daily new cases
write.table(active_cases_df, sep=",", OUTPUT_CSV_FILEPATH, row.names = FALSE, col.names = TRUE)


# Process cumulative removed cases per-day
# -------------------------------------------
# Translate total removed cases to matrix (for only outbreaks)
total_removed_cases_2D_mat <- array(rep(CONST_NOT_KNOWN, max_no_of_days * no_of_outbreak_sims),
                                    dim = c(max_no_of_days, no_of_outbreak_sims) )


# Transposing to row index as day-index
for(i in 1:no_of_outbreak_sims){
  for(j in 1:max_no_of_days){
    total_removed_cases_2D_mat[j, i] <- total_removed_vector_list[[i]][[j]]
  }
}

# Get asymptotic removed and related stats for all outbreaks
runs_removed_vector <- total_removed_cases_2D_mat[dim(total_removed_cases_2D_mat)[[1]], ] # Take the removed case for last day
runs_removed_mean <- mean(runs_removed_vector)
runs_removed_std <- sd(runs_removed_vector)

# Compute approximate R0
runs_r0 <- 1.0/(1.0 - (runs_removed_mean/runs_total_sim_pop))

# Visualise

# # Compare R0s obtained from asymptotic removed (in blue) vs actual R0 from infection history (in red)
# plot( 1.0/(1.0 - (runs_removed_vector/4602) ), col="blue", ylim =c(0, max(1.0/(1.0 - (runs_removed_vector/4602) ))))
# points( runs_r0_actual_vector, col="red")

# # Frequency of transmission per person
# hist(transmission_count_summary_df$count_transmissions, breaks = 200)
# abline( v = mean(transmission_count_summary_df$count_transmissions), lty = 2)

mean_cases <- c()
median_cases <- c()
min_cases <- c()
max_cases <- c()
first_qurtile_cases <- c()
third_quartile_cases <- c()

# Dr.Seahra: Visualise with 20% band + 10% band to cover 80% of outcomes in a contour like viz
thirty_eth_quantile_cases <- c()
forty_eth_quantile_cases <- c()
sixty_eth_quantile_cases <- c()
seventy_eth_quantile_cases <- c()

for(i in 1:max_no_of_days){
  temp_summary <- summary(total_removed_cases_2D_mat[i , ])

  mean_cases[i] <- temp_summary["Mean"]
  median_cases[i] <- temp_summary["Median"]
  min_cases[i] <- temp_summary["Min."]
  max_cases[i] <- temp_summary["Max."]
  first_qurtile_cases[i] <- temp_summary["1st Qu."]
  third_quartile_cases[i] <- temp_summary["3rd Qu."]


  temp_quantile_vector <- quantile(total_removed_cases_2D_mat[i, ], prob = c(0.3, 0.4, 0.6, 0.7))
  thirty_eth_quantile_cases[i] <- temp_quantile_vector[["30%"]]
  forty_eth_quantile_cases[i] <- temp_quantile_vector[["40%"]]
  sixty_eth_quantile_cases[i] <- temp_quantile_vector[["60%"]]
  seventy_eth_quantile_cases[i] <- temp_quantile_vector[["70%"]]

}

total_removed_cases_df <- as.data.frame(total_removed_cases_2D_mat)

#Summary stats
total_removed_cases_df <- cbind(total_removed_cases_df, mean_cases)
total_removed_cases_df <- cbind(total_removed_cases_df, median_cases)
total_removed_cases_df <- cbind(total_removed_cases_df, min_cases)
total_removed_cases_df <- cbind(total_removed_cases_df, max_cases)
total_removed_cases_df <- cbind(total_removed_cases_df, first_qurtile_cases)
total_removed_cases_df <- cbind(total_removed_cases_df, third_quartile_cases)

total_removed_cases_df <- cbind(total_removed_cases_df, thirty_eth_quantile_cases)
total_removed_cases_df <- cbind(total_removed_cases_df, forty_eth_quantile_cases)
total_removed_cases_df <- cbind(total_removed_cases_df, sixty_eth_quantile_cases)
total_removed_cases_df <- cbind(total_removed_cases_df, seventy_eth_quantile_cases)

# TODO: Change to ODS tab
OUTPUT_CSV_FILEPATH <- paste(OUTPUT_CSV_DIR,"DAILY-RECOVERED-TILL-DATE-CASES-", CONST_CHOSEN_DISEASE_MODEL, runs_amplitude,"-ONLY-OUTBREAKS.csv",  sep = "")

# write all Daily new cases
write.table(total_removed_cases_df, sep=",", OUTPUT_CSV_FILEPATH, row.names = FALSE, col.names = TRUE)







# Process cumulative actual infectious cases per-day
# -------------------------------------------
# Translate total actual infectious cases to matrix (for only outbreaks)
total_actual_infectious_cases_2D_mat <- array(rep(CONST_NOT_KNOWN, max_no_of_days * no_of_outbreak_sims),
                                              dim = c(max_no_of_days, no_of_outbreak_sims) )


# Transposing to row index as day-index
for(i in 1:no_of_outbreak_sims){
  for(j in 1:max_no_of_days){
    total_actual_infectious_cases_2D_mat[j, i] <- total_actual_infectious_vector_list[[i]][[j]]
  }
}

# Get peaks and related stats for all outbreaks
runs_actual_peak_vector <- apply(total_actual_infectious_cases_2D_mat, 2, max)
runs_actual_peak_mean <- mean(runs_actual_peak_vector)
runs_actual_peak_std <- sd(runs_actual_peak_vector)

mean_cases <- c()
median_cases <- c()
min_cases <- c()
max_cases <- c()
first_qurtile_cases <- c()
third_quartile_cases <- c()

# Dr.Seahra: Visualise with 20% band + 10% band to cover 80% of outcomes in a contour like viz
thirty_eth_quantile_cases <- c()
forty_eth_quantile_cases <- c()
sixty_eth_quantile_cases <- c()
seventy_eth_quantile_cases <- c()

for(i in 1:max_no_of_days){
  temp_summary <- summary(total_actual_infectious_cases_2D_mat[i , ])

  mean_cases[i] <- temp_summary["Mean"]
  median_cases[i] <- temp_summary["Median"]
  min_cases[i] <- temp_summary["Min."]
  max_cases[i] <- temp_summary["Max."]
  first_qurtile_cases[i] <- temp_summary["1st Qu."]
  third_quartile_cases[i] <- temp_summary["3rd Qu."]


  temp_quantile_vector <- quantile(total_actual_infectious_cases_2D_mat[i, ], prob = c(0.3, 0.4, 0.6, 0.7))
  thirty_eth_quantile_cases[i] <- temp_quantile_vector[["30%"]]
  forty_eth_quantile_cases[i] <- temp_quantile_vector[["40%"]]
  sixty_eth_quantile_cases[i] <- temp_quantile_vector[["60%"]]
  seventy_eth_quantile_cases[i] <- temp_quantile_vector[["70%"]]

}

total_actual_infectious_cases_df <- as.data.frame(total_actual_infectious_cases_2D_mat)

#Summary stats
total_actual_infectious_cases_df <- cbind(total_actual_infectious_cases_df, mean_cases)
total_actual_infectious_cases_df <- cbind(total_actual_infectious_cases_df, median_cases)
total_actual_infectious_cases_df <- cbind(total_actual_infectious_cases_df, min_cases)
total_actual_infectious_cases_df <- cbind(total_actual_infectious_cases_df, max_cases)
total_actual_infectious_cases_df <- cbind(total_actual_infectious_cases_df, first_qurtile_cases)
total_actual_infectious_cases_df <- cbind(total_actual_infectious_cases_df, third_quartile_cases)

total_actual_infectious_cases_df <- cbind(total_actual_infectious_cases_df, thirty_eth_quantile_cases)
total_actual_infectious_cases_df <- cbind(total_actual_infectious_cases_df, forty_eth_quantile_cases)
total_actual_infectious_cases_df <- cbind(total_actual_infectious_cases_df, sixty_eth_quantile_cases)
total_actual_infectious_cases_df <- cbind(total_actual_infectious_cases_df, seventy_eth_quantile_cases)

# TODO: Change to ODS tab
OUTPUT_CSV_FILEPATH <- paste(OUTPUT_CSV_DIR,"DAILY-ACTUAL-INFECTIOUS-CASES-", CONST_CHOSEN_DISEASE_MODEL, runs_amplitude, "-ONLY-OUTBREAKS.csv",  sep = "")

# write all Daily new cases
write.table(total_actual_infectious_cases_df, sep=",", OUTPUT_CSV_FILEPATH, row.names = FALSE, col.names = TRUE)







# Aggregate book-keeping
run_stat_mat[1, "amplitude"] <- runs_amplitude
run_stat_mat[1, "observed peak mean"] <- runs_peak_mean
run_stat_mat[1, "observed peak std"] <- runs_peak_std
run_stat_mat[1, "actual peak mean"] <- runs_actual_peak_mean
run_stat_mat[1, "actual peak std"] <- runs_actual_peak_std
run_stat_mat[1, "removed mean"] <- runs_removed_mean
run_stat_mat[1, "removed std"] <- runs_removed_std
run_stat_mat[1, "r0 estimated mean"] <- runs_r0
run_stat_mat[1, "r0 growth mean"] <- runs_r0_actual_mean
run_stat_mat[1, "r0 growth std"] <- runs_r0_actual_std
run_stat_mat[1, "n of runs"] <- runs_n_of_runs
run_stat_mat[1, "n of outbreaks"] <- runs_n_of_outbreaks
run_stat_mat[1, "% of outbreaks"] <- runs_percent_of_outbreaks
run_stat_mat[1, "sim setup time mean"] <- mean(sim_setuptime_list)
run_stat_mat[1, "sim setup time std"] <- sd(sim_setuptime_list)
run_stat_mat[1, "sim run time mean"] <- mean(sim_runtime_list)
run_stat_mat[1, "sim run time std"] <- sd(sim_runtime_list)
run_stat_mat[1, "sim dir"] <- runs_dir

# UUID <- Sys.time()
OUTPUT_CSV_FILEPATH <- paste(OUTPUT_CSV_DIR, CONST_CHOSEN_DISEASE_MODEL, runs_amplitude, "-STATS.csv",  sep = "")

# write all stats, tag: @Stat write
write.table(run_stat_mat, sep=",", OUTPUT_CSV_FILEPATH, row.names = FALSE, col.names = TRUE)


# Process cumulative infections per venue type
# -------------------------------------------
if(TRUE){
  # Bad code:
  aggregated_location_type_summary_table <- summary(infection_history_table_list[[1]]$citisketch_location_type)
  aggregated_location_type_summary_table <- 0
  for (temp_table in infection_history_table_list){
    aggregated_location_type_summary_table <- aggregated_location_type_summary_table + summary(temp_table$citisketch_location_type)
  }

  # UUID <- Sys.time()
  OUTPUT_CSV_FILEPATH <- paste(OUTPUT_CSV_DIR, CONST_CHOSEN_DISEASE_MODEL, runs_amplitude, "-INFECTION-BY-VENUE.csv",  sep = "")

  # write all stats, tag: @Stat write
  write.table(aggregated_location_type_summary_table, sep=",", OUTPUT_CSV_FILEPATH, row.names = TRUE, col.names = TRUE)
}
# End of processing cumulative active cases of runs with no interventions
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------




# For viz
library(tidyverse)
# For reshaping
library(reshape)

# Aesthetic
CONST_PLOT_START_DAY = 1
CONST_PLOT_END_DAY = 210
CONST_YLIM = max(runs_peak_vector) # force y-axis for reference
CONST_YLIM = 120

CONST_PLOT_INTERVENTION_LINE_COLOUR = "#42f57e22"
CONST_PLOT_INTERVENTION_MEDIAN_LINE_COLOUR = "#248043"
CONST_PLOT_INTERVENTION_FIRST_QUARTILE_LINE_COLOUR = "#417853"
CONST_PLOT_INTERVENTION_THIRD_QUARTILE_LINE_COLOUR = "#417853"

CONST_PLOT_LINE_COLOUR = "#F6871722"
CONST_PLOT_MEDIAN_LINE_COLOUR = "#F73B0C"
CONST_PLOT_FIRST_QUARTILE_LINE_COLOUR = "#9c2c10"
CONST_PLOT_THIRD_QUARTILE_LINE_COLOUR = "#9c2c10"


CONST_PLOT_INTERVENTION_LVL2_LINE_COLOUR = "#1d74cc22"
CONST_PLOT_INTERVENTION_LVL2_MEDIAN_LINE_COLOUR = "#125496"
CONST_PLOT_INTERVENTION_LVL2_FIRST_QUARTILE_LINE_COLOUR = "#153e66"
CONST_PLOT_INTERVENTION_LVL2_THIRD_QUARTILE_LINE_COLOUR = "#153e66"

CONST_PLOT_INTERVENTION_LVL2_ALT_LINE_COLOUR = "#b81dcc34"
CONST_PLOT_INTERVENTION_LVL2_ALT_MEDIAN_LINE_COLOUR = "#871296"
CONST_PLOT_INTERVENTION_LVL2_ALT_FIRST_QUARTILE_LINE_COLOUR = "#5d1566"
CONST_PLOT_INTERVENTION_LVL2_ALT_THIRD_QUARTILE_LINE_COLOUR = "#5d1566"

# CONST_PLOT_INTERVENTION_LVL2_COMB_LINE_COLOUR = "#1db8cc34"
# CONST_PLOT_INTERVENTION_LVL2_COMB_MEDIAN_LINE_COLOUR = "#128796"
# CONST_PLOT_INTERVENTION_LVL2_COMB_FIRST_QUARTILE_LINE_COLOUR = "#155d66"
# CONST_PLOT_INTERVENTION_LVL2_COMB_THIRD_QUARTILE_LINE_COLOUR = "#155d66"

CONST_PLOT_INTERVENTION_LVL2_COMB_LINE_COLOUR = "#8fcc1d34"
CONST_PLOT_INTERVENTION_LVL2_COMB_MEDIAN_LINE_COLOUR = "#689712"
CONST_PLOT_INTERVENTION_LVL2_COMB_FIRST_QUARTILE_LINE_COLOUR = "#496615"
CONST_PLOT_INTERVENTION_LVL2_COMB_THIRD_QUARTILE_LINE_COLOUR = "#496615"


CONST_PLOT_INTERVENTION_LVL3_LINE_COLOUR = "#deab2a34"
CONST_PLOT_INTERVENTION_LVL3_MEDIAN_LINE_COLOUR = "#deab2a"
CONST_PLOT_INTERVENTION_LVL3_FIRST_QUARTILE_LINE_COLOUR = "#deab2a"
CONST_PLOT_INTERVENTION_LVL3_THIRD_QUARTILE_LINE_COLOUR = "#deab2a"

CONST_PLOT_INTERVENTION_LVL4_LINE_COLOUR = "#25c29122"
CONST_PLOT_INTERVENTION_LVL4_MEDIAN_LINE_COLOUR = "#326958"

# Medians - Daily Active Cases
ggplot() +

  # Control - Daily Active Cases
  geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = median_cases), color = CONST_PLOT_MEDIAN_LINE_COLOUR) +
  geom_ribbon(data = active_cases_df, mapping = aes(x = seq(1:max_no_of_days), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.1) +
  geom_ribbon(data = active_cases_df, mapping = aes(x = seq(1:max_no_of_days), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.2) +

  # No-School - Daily Active Cases
  # geom_line(data = NO_SCHOOL_active_cases_df, mapping = aes( x = seq(1:NO_SCHOOL_max_no_of_days), y = median_cases), color = CONST_PLOT_INTERVENTION_MEDIAN_LINE_COLOUR) +
  # geom_ribbon(data = NO_SCHOOL_active_cases_df, mapping = aes(x = seq(1:NO_SCHOOL_max_no_of_days), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_MEDIAN_LINE_COLOUR, alpha = 0.1) +
  # geom_ribbon(data = NO_SCHOOL_active_cases_df, mapping = aes(x = seq(1:NO_SCHOOL_max_no_of_days), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_MEDIAN_LINE_COLOUR, alpha = 0.2) +

  # ggtitle("Business as usual (red) vs. School closure (green)") +
  ggtitle(paste("Business as usual (red) with ", length(outbreak_run_index_list), " realisations, amplitude = ", runs_amplitude, sep ="")) +
  xlab("Simulation day") +
  # ylab("Active cases") +
  ylab("Total active cases (for runs with active cases > 15)") +
  ylim(0, CONST_YLIM) +
  xlim(CONST_PLOT_START_DAY, CONST_PLOT_END_DAY) +
  theme(legend.position = "right")

# Debug
# cat("Debug Here \n");

# Medians - Removed till date
ggplot() +
  # Control - Removed till date
  geom_line(data = total_removed_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = median_cases), color = CONST_PLOT_MEDIAN_LINE_COLOUR, linetype = 1) +
  geom_ribbon(data = total_removed_cases_df, mapping = aes(x = seq(1:max_no_of_days), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.1, linetype = 1) +
  geom_ribbon(data = total_removed_cases_df, mapping = aes(x = seq(1:max_no_of_days), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.2, linetype = 1) +

  # No-School - Removed till date
  # geom_line(data = NO_SCHOOL_total_removed_cases_df, mapping = aes( x = seq(1:NO_SCHOOL_max_no_of_days), y = median_cases), color = CONST_PLOT_INTERVENTION_MEDIAN_LINE_COLOUR, linetype = 1) +
  # geom_ribbon(data = NO_SCHOOL_total_removed_cases_df, mapping = aes(x = seq(1:NO_SCHOOL_max_no_of_days), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_MEDIAN_LINE_COLOUR, alpha = 0.1, linetype = 1) +
  # geom_ribbon(data = NO_SCHOOL_total_removed_cases_df, mapping = aes(x = seq(1:NO_SCHOOL_max_no_of_days), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_MEDIAN_LINE_COLOUR, alpha = 0.2, linetype = 1) +

  # Kinda sorta asymptotes
  geom_hline(aes(yintercept = total_removed_cases_df$median_cases[max_no_of_days]), color = CONST_PLOT_MEDIAN_LINE_COLOUR, linetype = 2) +
  # geom_hline(aes(yintercept = NO_SCHOOL_total_removed_cases_df$median_cases[NO_SCHOOL_max_no_of_days]), color = CONST_PLOT_INTERVENTION_MEDIAN_LINE_COLOUR, linetype = 2) +

  ggtitle(paste("Business as usual (red): ", total_removed_cases_df$median_cases[max_no_of_days], " removed \n",
                # "vs. School closure (green): ", NO_SCHOOL_total_removed_cases_df$median_cases[NO_SCHOOL_max_no_of_days], " removed \n",
                "Total no. of simulated agents: ", runs_total_sim_pop, " with ", length(outbreak_run_index_list), " realisations and default amplitude: ", runs_amplitude,
                sep="")) +
  xlab("Simulation day") +
  # ylab("Active cases") +
  ylab("Removed cases till date (for runs with active cases > 15)") +
  ylim(0, runs_total_sim_pop) +
  xlim(CONST_PLOT_START_DAY, CONST_PLOT_END_DAY) +
  theme(legend.position = "right")



#---- Control: Individual run plots ---#
# Reshaping DF - adding a sim day index column as x
# Note: recasting as data frame to convert dyplr tibbles into vanilla data frames, Ref: https://stackoverflow.com/a/35500964/15175554
active_cases_df_reshaped <-  as.data.frame(data_frame(x = seq(1:max_no_of_days), active_cases_df[ , 1:no_of_outbreak_sims ]))

# Transforming to row wise time-series i.e. long form
active_cases_df_reshaped <- melt(active_cases_df_reshaped, id.vars = "x")
colour_cols <- rep(CONST_PLOT_LINE_COLOUR, no_of_outbreak_sims)

ggplot() +
  geom_line(data = active_cases_df_reshaped, aes(x = x, y = value, color = variable)) +
  scale_color_manual(values = colour_cols) +
  theme(legend.position = "none") +

  # Per-day median ribbons
  geom_line(data = active_cases_df, mapping = aes( x = seq(1:max_no_of_days), y = median_cases), color = CONST_PLOT_MEDIAN_LINE_COLOUR) +
  geom_ribbon(data = active_cases_df, mapping = aes(x = seq(1:max_no_of_days), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.1) +
  geom_ribbon(data = active_cases_df, mapping = aes(x = seq(1:max_no_of_days), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.2) +

  ggtitle(paste("Business as usual (red) with ", length(outbreak_run_index_list), " realisations, amplitude = ", runs_amplitude, sep ="")) +
  xlab("Simulation day") +
  ylab("Total active cases (for runs with active cases > 15)") +
  ylim(0, CONST_YLIM) +
  xlim(CONST_PLOT_START_DAY, CONST_PLOT_END_DAY)

#----- End of Control individual run plots ---#

# Note: Iterative code fragment for comparison  ribbon plots assumes the following data frames exist:
#       active_cases_df_NoDamp, active_cases_df_HalfDamp, active_cases_df_FullDamp
if(FALSE)
{
  # Note: 11 is just the number of (day-wise) summary columns added during analysis,
  #       namely: (mean, median, min, max, first quartile, third quartile, standard deviation, {thirty-th, forty-th, sixty-th, seventy-th} quantile )

  # Setup simulation group-wise colour vector
  colour_cols_NoDamp <- rep(CONST_PLOT_LINE_COLOUR, ncol(active_cases_df_NoDamp) - 11)
  # colour_cols_HalfDamp <- rep(CONST_PLOT_INTERVENTION_LINE_COLOUR, ncol(active_cases_df_HalfDamp) - 11)
  colour_cols_FullDamp <- rep(CONST_PLOT_INTERVENTION_LVL2_LINE_COLOUR, ncol(active_cases_df_FullDamp) - 11)
  # combined_colour_cols <- c(colour_cols_NoDamp, colour_cols_HalfDamp, colour_cols_FullDamp)
  combined_colour_cols <- c(colour_cols_NoDamp, colour_cols_FullDamp)

  # Combine all run data into a single data frame
  all_runs_df <- cbind(active_cases_df_NoDamp[ , 1:(ncol(active_cases_df_NoDamp) - 11)],
                       # active_cases_df_HalfDamp[ , 1:(ncol(active_cases_df_HalfDamp) - 11)],
                       active_cases_df_FullDamp[ , 1:(ncol(active_cases_df_FullDamp) - 11)])

  all_runs_df_colnames <- sprintf("run-%s",seq(1:ncol(all_runs_df)))
  colnames(all_runs_df) <- all_runs_df_colnames

  # Add day-index as 'x'
  all_runs_df_reshaped <-  as.data.frame( data_frame(x = seq_along(all_runs_df[, 1]), all_runs_df) )
  # Melt into Long form
  all_runs_df_reshaped <- melt(all_runs_df_reshaped, id.vars = "x")

  ggplot() +

    # All Runs
    geom_line(data = all_runs_df_reshaped, aes(x = x, y = value, color = variable)) +
    scale_color_manual(values = c(combined_colour_cols) ) +

    # No-Damp - median ribbons - Red
    geom_line(data = active_cases_df_NoDamp, mapping = aes( x = seq(1:nrow(active_cases_df_NoDamp)), y = median_cases), color = CONST_PLOT_MEDIAN_LINE_COLOUR) +
    geom_ribbon(data = active_cases_df_NoDamp, mapping = aes(x = seq(1:nrow(active_cases_df_NoDamp)), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.1) +
    geom_ribbon(data = active_cases_df_NoDamp, mapping = aes(x = seq(1:nrow(active_cases_df_NoDamp)), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.2) +

    # Half-Damp - median ribbons - Green
    # geom_line(data = active_cases_df_HalfDamp, mapping = aes( x = seq(1:nrow(active_cases_df_HalfDamp)), y = median_cases), color = CONST_PLOT_INTERVENTION_MEDIAN_LINE_COLOUR) +
    # geom_ribbon(data = active_cases_df_HalfDamp, mapping = aes(x = seq(1:nrow(active_cases_df_HalfDamp)), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_MEDIAN_LINE_COLOUR, alpha = 0.1) +
    # geom_ribbon(data = active_cases_df_HalfDamp, mapping = aes(x = seq(1:nrow(active_cases_df_HalfDamp)), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_MEDIAN_LINE_COLOUR, alpha = 0.2) +

    # Full-Damp - median ribbons - Blue
    geom_line(data = active_cases_df_FullDamp, mapping = aes( x = seq(1:nrow(active_cases_df_FullDamp)), y = median_cases), color = CONST_PLOT_INTERVENTION_LVL2_MEDIAN_LINE_COLOUR) +
    geom_ribbon(data = active_cases_df_FullDamp, mapping = aes(x = seq(1:nrow(active_cases_df_FullDamp)), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_LVL2_MEDIAN_LINE_COLOUR, alpha = 0.1) +
    geom_ribbon(data = active_cases_df_FullDamp, mapping = aes(x = seq(1:nrow(active_cases_df_FullDamp)), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_LVL2_MEDIAN_LINE_COLOUR, alpha = 0.2) +


    ggtitle(paste("Amplitude: ", runs_amplitude,  ", Disease Model: ", CONST_CHOSEN_DISEASE_MODEL,
                  "\nTracing delay: None",
                  "\nRed - Unobserved Infectious ", # ncol(active_cases_df_NoDamp) - 11, " outbreaks",
                  # "\nGreen - Delayed Tracing - SIR ", ncol(active_cases_df_HalfDamp) - 11, " outbreaks",
                  "\nBlue - Isolating Positive Results ", # ncol(active_cases_df_FullDamp) - 11, " outbreaks",
                  sep ="")
            ) +
    theme(legend.position = "none") +
    xlab("Simulation day") +
    ylab("Total active cases (for runs with active cases > 15)") +
    # ylim(0, 750) + #003
    # ylim(0, 1250) + #004
    # ylim(0, 1350) + #005
    # ylim(0, 1550) + #006
    ylim(0, 1650) + #007
    # ylim(0, 1750) +
    # xlim(CONST_PLOT_START_DAY, CONST_PLOT_END_DAY)
    xlim(CONST_PLOT_START_DAY, 100)

}


# Comparing three different strategies
if(FALSE)
{
  # Note: 11 is just the number of (day-wise) summary columns added during analysis,
  #       namely: (mean, median, min, max, first quartile, third quartile, standard deviation, {thirty-th, forty-th, sixty-th, seventy-th} quantile )
  
  
  # Setup simulation group-wise colour vector
  colour_cols_NoDamp <- rep(CONST_PLOT_LINE_COLOUR, ncol(active_cases_df_NoDamp) - 11)
  colour_cols_HalfDamp <- rep(CONST_PLOT_INTERVENTION_LVL4_LINE_COLOUR, ncol(active_cases_df_HalfDamp) - 11)
  colour_cols_FullDamp <- rep(CONST_PLOT_INTERVENTION_LVL2_LINE_COLOUR, ncol(active_cases_df_FullDamp) - 11)
  combined_colour_cols <- c(colour_cols_NoDamp, colour_cols_HalfDamp, colour_cols_FullDamp)
  
  # Combine all run data into a single data frame
  all_runs_df <- cbind(active_cases_df_NoDamp[ , 1:(ncol(active_cases_df_NoDamp) - 11)],
                       active_cases_df_HalfDamp[ , 1:(ncol(active_cases_df_HalfDamp) - 11)],
                       active_cases_df_FullDamp[ , 1:(ncol(active_cases_df_FullDamp) - 11)])
  
  all_runs_df_colnames <- sprintf("run-%s",seq(1:ncol(all_runs_df)))
  colnames(all_runs_df) <- all_runs_df_colnames
  
  # Add day-index as 'x'
  all_runs_df_reshaped <-  as.data.frame( data_frame(x = seq_along(all_runs_df[, 1]), all_runs_df) )
  # Melt into Long form
  all_runs_df_reshaped <- melt(all_runs_df_reshaped, id.vars = "x")
  
  ggplot() +
    
    # All Runs
    geom_line(data = all_runs_df_reshaped, aes(x = x, y = value, color = variable)) +
    scale_color_manual(values = c(combined_colour_cols) ) +
    
    # No-Damp - median ribbons - Red
    geom_line(data = active_cases_df_NoDamp, mapping = aes( x = seq(1:nrow(active_cases_df_NoDamp)), y = median_cases), color = CONST_PLOT_MEDIAN_LINE_COLOUR) +
    geom_ribbon(data = active_cases_df_NoDamp, mapping = aes(x = seq(1:nrow(active_cases_df_NoDamp)), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.1) +
    geom_ribbon(data = active_cases_df_NoDamp, mapping = aes(x = seq(1:nrow(active_cases_df_NoDamp)), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.2) +
    
    # Half-Damp - median ribbons - Green
    geom_line(data = active_cases_df_HalfDamp, mapping = aes( x = seq(1:nrow(active_cases_df_HalfDamp)), y = median_cases), color = CONST_PLOT_INTERVENTION_LVL4_MEDIAN_LINE_COLOUR) +
    geom_ribbon(data = active_cases_df_HalfDamp, mapping = aes(x = seq(1:nrow(active_cases_df_HalfDamp)), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_LVL4_MEDIAN_LINE_COLOUR, alpha = 0.1) +
    geom_ribbon(data = active_cases_df_HalfDamp, mapping = aes(x = seq(1:nrow(active_cases_df_HalfDamp)), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_LVL4_MEDIAN_LINE_COLOUR, alpha = 0.2) +
    
    # Full-Damp - median ribbons - Blue
    geom_line(data = active_cases_df_FullDamp, mapping = aes( x = seq(1:nrow(active_cases_df_FullDamp)), y = median_cases), color = CONST_PLOT_INTERVENTION_LVL2_MEDIAN_LINE_COLOUR) +
    geom_ribbon(data = active_cases_df_FullDamp, mapping = aes(x = seq(1:nrow(active_cases_df_FullDamp)), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_LVL2_MEDIAN_LINE_COLOUR, alpha = 0.1) +
    geom_ribbon(data = active_cases_df_FullDamp, mapping = aes(x = seq(1:nrow(active_cases_df_FullDamp)), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_LVL2_MEDIAN_LINE_COLOUR, alpha = 0.2) +
    
    
    ggtitle(paste("Tested positive and isolating cases, Amplitude: ", "0.008",  ", Disease Model: SIR varying Tracing Strategy",
    # ggtitle(paste("Unobserved infectious cases, Amplitude: ", "0.008",  ", Disease Model: SIR varying Tracing Strategy",             
                  "\nRed - No Tracing  ", # ncol(active_cases_df_NoDamp) - 11, " outbreaks",
                  "\nGreen - Delayed Tracing  ", # ncol(active_cases_df_HalfDamp) - 11, " outbreaks",
                  "\nBlue - Same day tracing ", # ncol(active_cases_df_FullDamp) - 11, " outbreaks",
                  sep ="")
    ) +
    theme(legend.position = "none") +
    xlab("Simulation day") +
    ylab("Total no. of cases (for runs with active cases > 15)") +
    # ylim(0, 1100) + #003
    # ylim(0, 1350) + #004
    # ylim(0, 1450) + #005
    # ylim(0, 1700) + #006
    # ylim(0, 1800) + #007
    ylim(0, 2050) + #008
    # xlim(CONST_PLOT_START_DAY, CONST_PLOT_END_DAY)
    xlim(CONST_PLOT_START_DAY, 100)
  
}

# Comparing medians of observed positives vs. unobserved infected 
if(FALSE)
{
  ggplot() +
    
    # Control - observed positive - median ribbons - Red muted
    geom_line(data = active_cases_df_008_noTracing, mapping = aes( x = seq(1:nrow(active_cases_df_NoDamp)), y = median_cases), color = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.3) +
    geom_ribbon(data = active_cases_df_008_noTracing, mapping = aes(x = seq(1:nrow(active_cases_df_NoDamp)), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.025) +
    geom_ribbon(data = active_cases_df_008_noTracing, mapping = aes(x = seq(1:nrow(active_cases_df_NoDamp)), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.05) +
    
    # Control - unobserved infectious - median ribbons - Red
    geom_line(data = total_actual_infectious_cases_df_008_noTracing, mapping = aes( x = seq(1:nrow(active_cases_df_NoDamp)), y = median_cases), color = CONST_PLOT_MEDIAN_LINE_COLOUR) +
    geom_ribbon(data = total_actual_infectious_cases_df_008_noTracing, mapping = aes(x = seq(1:nrow(active_cases_df_NoDamp)), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.1) +
    geom_ribbon(data = total_actual_infectious_cases_df_008_noTracing, mapping = aes(x = seq(1:nrow(active_cases_df_NoDamp)), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.2) +
    
    
    
    
    # Strategy 1 - observed positive - median ribbons - Green muted
    geom_line(data = active_cases_df_008_delay03Tracing, mapping = aes( x = seq(1:nrow(active_cases_df_NoDamp)), y = median_cases), color = CONST_PLOT_INTERVENTION_LVL4_MEDIAN_LINE_COLOUR, alpha = 0.3) +
    geom_ribbon(data = active_cases_df_008_delay03Tracing, mapping = aes(x = seq(1:nrow(active_cases_df_NoDamp)), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_LVL4_MEDIAN_LINE_COLOUR, alpha = 0.025) +
    geom_ribbon(data = active_cases_df_008_delay03Tracing, mapping = aes(x = seq(1:nrow(active_cases_df_NoDamp)), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_LVL4_MEDIAN_LINE_COLOUR, alpha = 0.05) +
    
    # Strategy 1 - unobserved infectious - median ribbons - Green
    geom_line(data = total_actual_infectious_cases_df_008_delay03Tracing, mapping = aes( x = seq(1:nrow(active_cases_df_NoDamp)), y = median_cases), color = CONST_PLOT_INTERVENTION_LVL4_MEDIAN_LINE_COLOUR) +
    geom_ribbon(data = total_actual_infectious_cases_df_008_delay03Tracing, mapping = aes(x = seq(1:nrow(active_cases_df_NoDamp)), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_LVL4_MEDIAN_LINE_COLOUR, alpha = 0.1) +
    geom_ribbon(data = total_actual_infectious_cases_df_008_delay03Tracing, mapping = aes(x = seq(1:nrow(active_cases_df_NoDamp)), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_LVL4_MEDIAN_LINE_COLOUR, alpha = 0.2) +
    
    
    
    
    # Strategy 2 - observed positive - median ribbons - Blue muted
    geom_line(data = active_cases_df_008_sameDayTracing, mapping = aes( x = seq(1:nrow(active_cases_df_NoDamp)), y = median_cases), color = CONST_PLOT_INTERVENTION_LVL2_MEDIAN_LINE_COLOUR, alpha = 0.3) +
    geom_ribbon(data = active_cases_df_008_sameDayTracing, mapping = aes(x = seq(1:nrow(active_cases_df_NoDamp)), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_LVL2_MEDIAN_LINE_COLOUR, alpha = 0.025) +
    geom_ribbon(data = active_cases_df_008_sameDayTracing, mapping = aes(x = seq(1:nrow(active_cases_df_NoDamp)), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_LVL2_MEDIAN_LINE_COLOUR, alpha = 0.05) +
    
    # Strategy 2 - unobserved infectious - median ribbons - Blue
    geom_line(data = total_actual_infectious_cases_df_008_sameDayTracing, mapping = aes( x = seq(1:nrow(active_cases_df_NoDamp)), y = median_cases), color = CONST_PLOT_INTERVENTION_LVL2_MEDIAN_LINE_COLOUR) +
    geom_ribbon(data = total_actual_infectious_cases_df_008_sameDayTracing, mapping = aes(x = seq(1:nrow(active_cases_df_NoDamp)), ymax = seventy_eth_quantile_cases, ymin = thirty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_LVL2_MEDIAN_LINE_COLOUR, alpha = 0.1) +
    geom_ribbon(data = total_actual_infectious_cases_df_008_sameDayTracing, mapping = aes(x = seq(1:nrow(active_cases_df_NoDamp)), ymax = sixty_eth_quantile_cases, ymin = forty_eth_quantile_cases), fill = CONST_PLOT_INTERVENTION_LVL2_MEDIAN_LINE_COLOUR, alpha = 0.2) +
    
    
    
    ggtitle(paste("Tested positive and isolating cases, Amplitude: ", "0.008",  ", Disease Model: SIR varying Tracing Strategy",       
                  "\nRed - No Tracing  ", # ncol(active_cases_df_NoDamp) - 11, " outbreaks",
                  "\nGreen - Delayed Tracing  ", # ncol(active_cases_df_HalfDamp) - 11, " outbreaks",
                  "\nBlue - Same day tracing ", # ncol(active_cases_df_FullDamp) - 11, " outbreaks",
                  sep ="")
    ) +
    theme(legend.position = "none") +
    xlab("Simulation day") +
    ylab("Total no. of cases (for runs with active cases > 15)") +
    # ylim(0, 800) + #003
    # ylim(0, 1000) + #004
    # ylim(0, 1150) + #005
    # ylim(0, 1300) + #006
    # ylim(0, 1450) + #007
    ylim(0, 1500) + #008
    # xlim(CONST_PLOT_START_DAY, CONST_PLOT_END_DAY)
    xlim(CONST_PLOT_START_DAY, 100)
  
}

#----- Histogram of infectious period ---- #
k = mean(infectious_period)
ggplot(mapping = aes(infectious_period)) +
  geom_histogram( binwidth=0.5, fill = CONST_PLOT_MEDIAN_LINE_COLOUR, alpha = 0.5 ) +
  geom_vline(aes(xintercept = k), color = CONST_PLOT_MEDIAN_LINE_COLOUR, linetype = 2) +
  ggtitle(paste("Infectious period for ", length(infectious_period), " agents in ",  length(outbreak_run_index_list), " realisations, amplitude = ", runs_amplitude,
                "\nMean infectious period = ", k, " days", sep ="")) +
  xlab("Length of infectious period") +
  ylab("No. of agents")

#----- End of Comparison of Control and School Closure individual run plots ---#

# From Robert
A_robert = 1.5502
k_robert = 0.0983

A_robert_school_closed = 2.04
k_robert_school_closed = 0.0829

CONST_DECIMAL_PLACES = 4


# Fit range: determined visually
FIT_RANGE_START = 25
FIT_RANGE_END = 50

# FIT_RANGES [10, 35], [0, 35] yielded close to Robert's fit

#----- Control - Curve fitting ----- #
# Removing curve fitting for daily active cases
if (FALSE)
{
  x <- seq(1:max_no_of_days)
  y <- active_cases_df$median_cases


  # Remove leading 0s (from first 20 days)) (Hacfix, TODO: code)
  leading_0s_in_y <- which(y[1:20] == 0)
  trim_x <- length(leading_0s_in_y)
  y <- y[ - leading_0s_in_y]
  x <- head(x, - trim_x)

  plot(x,  y)



  mult_lm <- lm(log(y[FIT_RANGE_START: FIT_RANGE_END]) ~ x[FIT_RANGE_START: FIT_RANGE_END])
  summary(mult_lm)

  # Fitting y = A * e^(k * t)
  #  => log(y) = log(A) + k * t

  lm_coef <- coef(mult_lm)

  A = round(exp(lm_coef[1]), CONST_DECIMAL_PLACES)
  k = round(lm_coef[2], CONST_DECIMAL_PLACES)

  # Debug lines
  plot(x,  log(y))
  lines(x, log(A) + (k * x) , col = "#00FF00", lwd = 1, lty = 1)
  lines(x, log(A_robert) + (k_robert * (x + trim_x)) , col = "#0000FF", lwd = 1, lty = 1)
  abline(v=c(FIT_RANGE_START, FIT_RANGE_END),  col = "#FF00FF", lwd = 1, lty = 2)

  # A_assume <- 1
  # k_assume <- 0.1
  #
  # add_nls <- nls(y[FIT_RANGE_START: FIT_RANGE_END] ~ A_assume*exp(k_assume * x[FIT_RANGE_START: FIT_RANGE_END]), start = list(A_assume = 0.5, k_assume = 0.2))
  # nls_coef <- coef(add_nls)
  # A_nls <- round(nls_coef[1], CONST_DECIMAL_PLACES)
  # k_nls <- round(nls_coef[2], CONST_DECIMAL_PLACES)
  #
  # # Debug line
  # lines(x, log(A_nls) + ( k_nls * x) )

  # plot_title <- paste("A = ", A, ", k = ", k, " \n ", "A' = ", A_robert, ", k' = ", k_robert,  sep ="")
  plot_title <- paste("A = ", A, ", k = ", k, sep ="")

  plot(x,  y,
       col = "#FF0000",
       type = "p",
       pch = 4,
       ylab = "y = A * exp(k * t)",
       ylim = c(0, 2500),
       # ylim = c(0, CONST_YLIM),
       xlab = paste("t (simulation days) offset by ", trim_x, " days", sep =""),
       main = plot_title)

  # Plot regression range
  abline(v=c(FIT_RANGE_START, FIT_RANGE_END),  col = "#FF00FF", lwd = 1, lty = 2)

  lines(x, A*exp(k * x), col = "#00FF00", lwd = 1, lty = 1)
  # lines(x, A_nls*exp(k_nls * x), col = "#00FF00", lwd = 1, lty = 1)

  # Robert's exp
  # lines(x, A_robert*exp(k_robert * (x + trim_x)), col = "#0000FF", lwd = 2, lty = 1)


  legend("topright",
         cex = 0.65,
         # legend = c("data", "fit", paste("fitting-range [",FIT_RANGE_START,",",FIT_RANGE_END, "]", sep=""), "Robert's fit"),
         legend = c("data", "fit", paste("fitting-range [",FIT_RANGE_START,",",FIT_RANGE_END, "]", sep="")),
         pch = c(4, NA, NA),
         lty = c(NA, 1, 2, 1),
         col = c("#FF0000", "#00FF00", "#FF00FF"),
         )
}
#----- End of Control - Curve fitting ----- #




# Stop the clock
cat("\n")
cat(proc.time() - ptm)

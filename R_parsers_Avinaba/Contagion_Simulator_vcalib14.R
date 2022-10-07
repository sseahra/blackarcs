# Start the clock!
start_time <- proc.time()

library(dplyr) # for dataframe utitlities
library(iterators)
library(jsonlite) # for read_json()
library(Matrix) # for Matrix and Sparse Matrix
# library(bit) # for list of list of contacts as bitmap, check Tag: @bitvector, increases run-time converting back to list of contact indices

# Calibration 03 - Tag: @cailb03 All initial infections are put in "asymptomatic non-isolated" or "presymptomatic non-isolated" state
# Calibration 04 - Tag: @calib04 Location wise contact matrices
# Calibration 05 - Tag: @calib05 Parallelising non-susceptible evolution
# Calibration 06 - Tag: @calib06 switch between checking possible infections from infectious set vs. checking possible infection for susceptible set
CONST_INFECTIOUS_COUNT_SWITCH = 5000 # Impossible condition
# Calibration 07 - Tag: @calib07 add infection venue details into infection history mat

# Calibration 08 - Tag: @calib08 add simulated event augments to contact matrices of particular days
# Note: This is still dependent of (synthetic) micro-census data
GENERATE_EVENT_CONTACT_PAIRS <- FALSE
EVENT_START_DATE <- 20 # starting sim day
EVENT_END_DATE <- 24 # ending sim day
COUNT_EVENT_ATTENDANCE <- 200 # desired size of the event
EVENT_DURATION_IN_SECONDS <- 4 * 60 * 60 # 4 hours, event duration

# Calibration 09 - Tag: @calib09 add infectiousness pdf
CONST_USE_INFECTIOUSNESS_PDF <- FALSE
inefctiousness_pdf <- function(day_index) { return(day_index^4 / (exp(0.8 * day_index) + 2)) }
START_DAY_INF_INDEX <- 0
END_DAY_INF_INDEX <- 210
MAX_INFECTIOUSNESS <- max(curve(inefctiousness_pdf(x), from = START_DAY_INF_INDEX, to = END_DAY_INF_INDEX)$y)
MIN_INFECTIOUSNESS <- min(curve(inefctiousness_pdf(x), from = START_DAY_INF_INDEX, to = END_DAY_INF_INDEX)$y)
normalised_infectiousness_pdf <- function(day_index) { return (((day_index^4 / (exp(0.8 * day_index) + 2)) - MIN_INFECTIOUSNESS)/(MAX_INFECTIOUSNESS - MIN_INFECTIOUSNESS)) }
curve(normalised_infectiousness_pdf(x), from = 0, to = 15) # Debug infectiousness PDF


# Calibration 10 - Tag: @calib10 add testing and isolation (requires extra compartments over traditional models)
# Note: models supporting testing and isolation for now: "SIR + Testing + Isolation"
MAX_COVID_TEST_CAPACITY_ANTIGEN_TYPE = 5000 # 10 # 3 # Available tests per-day
MAX_COVID_TEST_CAPACITY_RAPID_MOLECULAR_TYPE = 200 # 10 # 3 # Available tests per-day


# CONST_SYMPTOMATIC_INFECTIOUS_TEST_CHANCE = 0.5 # Debug hospitalisation 
CONST_SYMPTOMATIC_INFECTIOUS_TEST_CHANCE = 0.05 # Voluntary testing by symptomatic infectious population
CONST_ASYMPTOMATIC_TEST_CHANCE = 0.001 # 0.02 # Voluntary testing by susceptible and asymptomatic infectious population

TEST_ALLOCATION_STRATEGY_NAMES <- c("Antigen test first")
CONST_CHOSEN_TEST_ALLOCATION_STRATEGY <- "Antigen test first"

# Calibration 11 - Tag: @calib11 adding contact tracing, requires testing and isolation (and therfore requires extra compartments over traditional models)
# Note: models supporting contact tracing for now: "SIR + Testing + Isolation"
# Rough algorithm: "Whenever a positive result of a covid test is reported, the neighbours of the particular agent are queued for testing
#                   with appropriate delays"

CONST_ENABLE_CONTACT_TRACING = TRUE
CONTACT_TRACING_STRATEGY_NAMES <- c( "Conventional tracing", "Mobile app based tracing" )
CONST_CHOSEN_CONTACT_TRACING_STRATEGY <- "Mobile app based tracing"
CONST_CHOSEN_CLOSE_CONTACT_NUMBER <- 4 # Magic number
CONST_CHOSEN_CASUAL_CONTACT_NUMBER <- 9 # Magic number
CONST_SAMEDAY_TRACING_LEVEL <- 1 # How many levels of neighbours may be traced in a single day (before spilling the traced testing to the next day)
CONST_ISOLATE_TRACED_OVERFLOW = FALSE # Set to true for congruence with original Julia implement

# SIM_SCRIPT_NAME = 'Campbellton_LocalDebug_SIR_w_IPDF_random_import_refactor_vcalib09_for_210d_location_wise'
CONST_DAMP_FOR_COMMERCIAL = 1.0

# Oct 05
# SIM_SCRIPT_NAME = 'Oct05_Batch_01_Campbellton_SIR_sameday_tracing_without_2nd_level_random_import_vcalib14_00800_1mat_for_210d_aggregated'
SIM_SCRIPT_NAME = 'Oct05_Batch_01_Campbellton_SIR_sameday_tracing_random_import_vcalib14_00800_1mat_for_210d_aggregated'

# Sep 29
# SIM_SCRIPT_NAME = 'Sep29_Batch_01_Campbellton_SIR_sameday_tracing_random_import_vcalib14_00800_1mat_for_210d_aggregated'

# Sep 27 
# SIM_SCRIPT_NAME = 'Sep27_Batch_01_Campbellton_SIR_voluntary_testing_random_import_vcalib14_00800_1mat_for_210d_aggregated'

# SIM_SCRIPT_NAME = 'Sep27_Batch_01_Campbellton_SIR_classic_random_import_vcalib14_00800_1mat_for_210d_aggregated'

# Sep 26 
# SIM_SCRIPT_NAME = 'Sep26_Batch_01_Campbellton_SIR_sameday_tracing_fixed_import_vcalib14_00800_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'Sep26_Batch_01_Campbellton_SIR_voluntary_testing_fixed_import_vcalib14_00800_1mat_for_210d_aggregated'

# Sep 25
# SIM_SCRIPT_NAME = 'Sep25_Batch_01_Campbellton_SIR_sameday_tracing_random_import_vcalib14_00800_1mat_for_210d_aggregated'

# Sep 23
# SIM_SCRIPT_NAME = 'Sep23_Batch_01_Campbellton_SIR_sameday_tracing_random_import_vcalib14_00800_1mat_for_210d_aggregated'

# - Campbellton sims
# SIM_SCRIPT_NAME = 'LocalDebug_Campbellton_SIR_w_Hospitalisation_noTracing_testHalfInf_random_import_vcalib14_80000_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'LocalDebug_Campbellton_SIR_w_Hospitalisation_noTracing_random_import_vcalib14_80000_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'LocalDebug_Finally_Fixed_Campbellton_SIR_sameday_tracing_random_import_vcalib14_00800_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'IsolateLikeJulia_Campbellton_SIR_sameday_tracing_random_import_vcalib14_00800_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'LocalDebug_Fixed_Campbellton_SIR_sameday_tracing_random_import_vcalib14_00800_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'LocalDebug_Campbellton_SIR_w_Hospitalisation_noTracing_random_import_vcalib14_00800_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'Fixed_Campbellton_SIR_sameday_tracing_random_import_vcalib14_00800_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'LikeJarod_Campbellton_SIR_sameday_tracing_random_import_vcalib14_00800_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_sameday_tracing_random_import_vcalib14_00800_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'Corrected_LocalDebug_Campbellton_SIR_only_voluntary_testing_random_import_vcalib14_00800_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'LocalDebug_Campbellton_SIR_only_voluntary_testing_random_import_vcalib14_00800_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'LocalDebug_Campbellton_SIR_classic_random_import_vcalib14_00300_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'LocalDebug_Campbellton_SIR_classic_random_import_vcalib14_00800_1mat_for_210d_aggregated'

# Debug:
# SIM_SCRIPT_NAME = 'LocalDebug_Campbellton_SIR_hospitalisation_sameday_tracing_random_import_vcalib13_00300_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'LocalDebug_Campbellton_SIR_hospitalisation_sameday_tracing_random_import_vcalib13_00500_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'LocalDebug_Campbellton_SIR_hospitalisation_sameday_tracing_random_import_vcalib13_00800_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'LocalDebug_Campbellton_SIR_hospitalisation_sameday_tracing_random_import_vcalib13_03000_1mat_for_210d_aggregated'
# SIM_SCRIPT_NAME = 'LocalDebug_Campbellton_SIR_hospitalisation_sameday_tracing_random_import_vcalib13_30000_1mat_for_210d_aggregated'

# SIM_SCRIPT_NAME = 'LocalDebug_Campbellton_SIR_hospitalisation_sameday_tracing_random_import_vcalib14_00300_1mat_for_210d_location_based'


# - - Location wise -- Control 30days --

# - - - SI Recovered, Dead, Hospitalisation and ICU with Same day tracing -- with same day testing of 1st level of neighbour
# SIM_SCRIPT_NAME = 'Campbellton_SIR_hospitalisation_sameday_tracing_random_import_vcalib14_00025_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_hospitalisation_sameday_tracing_random_import_vcalib14_00050_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_hospitalisation_sameday_tracing_random_import_vcalib14_00100_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_hospitalisation_sameday_tracing_random_import_vcalib14_00200_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_hospitalisation_sameday_tracing_random_import_vcalib14_00300_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_hospitalisation_sameday_tracing_random_import_vcalib14_00400_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_hospitalisation_sameday_tracing_random_import_vcalib14_00500_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_hospitalisation_sameday_tracing_random_import_vcalib14_00600_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_hospitalisation_sameday_tracing_random_import_vcalib14_00700_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_hospitalisation_sameday_tracing_random_import_vcalib14_00800_30mat_for_210d_location_based'
# - - - End of SI Recovered, Dead, Hospitalisation and ICU with Same day tracing -- with same day testing of 1st level of neighbour

# - - - Only Voluntary testing and isolation
# SIM_SCRIPT_NAME = 'Campbellton_SIR_only_voluntary_testing_random_import_vcalib13_00300_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_only_voluntary_testing_random_import_vcalib13_00400_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_only_voluntary_testing_random_import_vcalib13_00500_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_only_voluntary_testing_random_import_vcalib13_00600_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_only_voluntary_testing_random_import_vcalib13_00700_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_only_voluntary_testing_random_import_vcalib13_00800_30mat_for_210d_location_based'
# - - - End of Only Voluntary testing

# - - - Same day tracing -- with same day testing of 1st level of neighbour
# SIM_SCRIPT_NAME = 'Campbellton_SIR_sameday_tracing_random_import_vcalib13_00300_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_sameday_tracing_random_import_vcalib13_00400_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_sameday_tracing_random_import_vcalib13_00500_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_sameday_tracing_random_import_vcalib13_00600_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_sameday_tracing_random_import_vcalib13_00700_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_sameday_tracing_random_import_vcalib13_00800_30mat_for_210d_location_based'

# - - - End of Same day tracing -- with same day testing of 1st level of neighbour

# - - - Delayed tracing
# SIM_SCRIPT_NAME = 'Campbellton_SIR_delay03_tracing_random_import_vcalib13_00300_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_delay03_tracing_random_import_vcalib13_00400_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_delay03_tracing_random_import_vcalib13_00500_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_delay03_tracing_random_import_vcalib13_00600_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_delay03_tracing_random_import_vcalib13_00700_30mat_for_210d_location_based'
# SIM_SCRIPT_NAME = 'Campbellton_SIR_delay03_tracing_random_import_vcalib13_00800_30mat_for_210d_location_based'

# - - - End of Delayed tracing

# - - End of Location wise -- Control 30days

# - End of Campbellton sims




TOTAL_SIMULATION_DAYS <- 210
TOTAL_SIMULATION_RUNS <- 1

CONST_CHOSEN_DISEASE_MODEL <- "SIR + Testing + Isolation + Contact Tracing"
# Human readable disease model names
# Note: this is loosely implemented with manual string matching
DISEASE_MODEL_NAMES <- c ("SIR",
                          "Complex",
                          "SIR + Testing + Isolation",
                          "SIR + Testing + Isolation + Contact Tracing",
                          "SIR + Testing + Isolation + Hospitalisation + Contact Tracing")

# Tag: @calib04
CONST_CHOSEN_CONTACT_MATRIX_TYPE <- "Aggregated"
# Note: this too is loosely implemented with manual string matching
CONTACT_MATRIX_TYPE_NAMES <- c("Aggregated", "Location based")


# for Omicron, Amplitude = 0.00375 = CONST_MAX_CHANCE * RELATIVE_INFECTIVITY


# Amplitude of infection
# CONST_MAX_CHANCE <- 0.00375/3.42257472363763 # From Robert

# To check for linear variance between R0 and Amplitude

# Amplitude of infection
# Amplitude of infection      |   For Aggregated            |   For Location wise
# CONST_MAX_CHANCE <- 0.00050 #
# CONST_MAX_CHANCE <- 0.00050 #
# CONST_MAX_CHANCE <- 0.00100 # ~ 29.74% outbreaks - 500 runs | ~ 25.2% outbreaks - 500 runs
# CONST_MAX_CHANCE <- 0.00200 # ~ 75% outbreaks - 400 runs    | ~ 75% - 300 runs
# CONST_MAX_CHANCE <- 0.00250 # ~ 94% outbreaks - 200 runs    | ~ 78.57 % - 140 runs
# CONST_MAX_CHANCE <- 0.00300 # - 95.5% outbreaks - 200 runs  | ~ 92.5% - 120 runs
# CONST_MAX_CHANCE <- 0.00370 # - 95.5% outbreaks - 200 runs
# CONST_MAX_CHANCE <- 0.00400 # - 95% outbreaks - 200 runs
# CONST_MAX_CHANCE <- 0.00500 # - 98% outbreaks - 200 runs
# CONST_MAX_CHANCE <- 0.00600 # - 99.5% outbreaks - 200 runs
# CONST_MAX_CHANCE <- 0.00700 # - 99.5% outbreaks - 200 runs
# CONST_MAX_CHANCE <- 0.00750 # - ??% outbreaks - 200 runs
CONST_MAX_CHANCE <- 0.00800 # - 100% outbreaks - 200 runs
# CONST_MAX_CHANCE <- 0.00900 # - 100% outbreaks - 200 runs
# CONST_MAX_CHANCE <- 0.01000 # Calibration for Wild x SIR - 99.5% outbreaks - 200 runs
# CONST_MAX_CHANCE <- 0.01500
# CONST_MAX_CHANCE <- 0.02500
# CONST_MAX_CHANCE <- 0.03000
# CONST_MAX_CHANCE <- 0.05000
# CONST_MAX_CHANCE <- 0.30000

cat("\n")
cat("CONST_MAX_CHANCE: ", CONST_MAX_CHANCE)
cat("\nGENERATE_EVENT_CONTACT_PAIRS: ", GENERATE_EVENT_CONTACT_PAIRS)
if(GENERATE_EVENT_CONTACT_PAIRS){
  cat(", from ", EVENT_START_DATE, " to ", EVENT_END_DATE, " sim day", sep = "")
}
cat("\nCONST_USE_INFECTIOUSNESS_PDF: ", CONST_USE_INFECTIOUSNESS_PDF)


CONST_TIME_TILL_76p_CHANCE <- 1200 # seconds (20 minutes)

# Tag: @calib10
# Test allocation type configuration check
# -----
if (CONST_CHOSEN_TEST_ALLOCATION_STRATEGY %in% TEST_ALLOCATION_STRATEGY_NAMES == FALSE){
  cat("\n Required covid test allocation strategy: ", CONST_CHOSEN_TEST_ALLOCATION_STRATEGY, " is not implemented yet", sep ="")
  cat("\nTerminating simulation\n", sep = "")
  cat("\n", proc.time() - start_time, "\n")
  stop()
}

# Test parameters
# Note: Summary assessment of diagnostic accuracy of (rapid) antigen test and molecular based tests of SARS-CoV-2 infection.
#       Provides a basis for testing subroutine for a network model of contagion spread.
# Ref: Dinnes J, Deeks JJ, Berhane S, Taylor M, Adriano A, Davenport C, Dittrich S, Emperador D, Takwoingi Y, Cunningham J, Beese S, Domen J, Dretzke J, Ferrante di Ruffano L, Harris IM, Price MJ, Taylor-Phillips S, Hooft L, Leeflang MMG, McInnes MDF, Spijker R, Van den Bruel A. Rapid, point‐of‐care antigen and molecular‐based tests for diagnosis of SARS‐CoV‐2 infection. Cochrane Database of Systematic Reviews 2021, Issue 3. Art. No.: CD013705. Dccessed 08 July 2022.
# URI: https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD013705.pub2/full
{
  COVID_TEST_COLNAMES  =  c("Type",
                            "Result delay",
                            "Symptomatic TPR", # Sensitivity
                            "Symptomatic TNR", # Specificity
                            "Symptomatic FNR", # Miss Rate = 1 - Sensitivity
                            "Symptomatic FPR", # Fall out = 1 - Specificity

                            "Asymptomatic TPR", # Sensitivity
                            "Asymptomatic TNR", # Specificity
                            "Asymptomatic FNR", # Miss Rate = 1 - Sensitivity
                            "Asymptomatic FPR", # Fall out = 1 - Specificity

                            "Max test capacity") # No. of such tests available per day

  covid_test_antigen_mean <- data.frame("antigen", # Type
                                        15/(24 * 60), # Result delay: 15 minutes in days
                                        0.72, # Symptomatic TPR
                                        0.995, # Symptomatic TNR
                                        0.28, # Symptomatic FNR
                                        0.005, # Symptomatic FPR

                                        0.581, # Asymptomatic TPR
                                        0.989, # Asymptomatic TNR
                                        0.419, # Asymptomatic FNR
                                        0.011, # Asymptomatic FPR

                                        MAX_COVID_TEST_CAPACITY_ANTIGEN_TYPE) # Max test capacity
  colnames(covid_test_antigen_mean) <- COVID_TEST_COLNAMES
  covid_test_df <- covid_test_antigen_mean


  covid_test_rapid_molecular_mean <- data.frame("rapid molecular", # Type
                                                1.5, # Result delay: 24 ~ 48 hours
                                                0.951, # Symptomatic TPR
                                                0.988, # Symptomatic TNR
                                                0.049, # Symptomatic FNR
                                                0.012, # Symptomatic FPR

                                                0.951, # Asymptomatic TPR
                                                0.988, # Asymptomatic TNR
                                                0.049, # Asymptomatic FNR
                                                0.012, # Asymptomatic FPR

                                                MAX_COVID_TEST_CAPACITY_RAPID_MOLECULAR_TYPE) # Max test capacity
  colnames(covid_test_rapid_molecular_mean) <- COVID_TEST_COLNAMES
  covid_test_df <- rbind(covid_test_df, covid_test_rapid_molecular_mean)
}

# Tag: @calib11
if (CONST_ENABLE_CONTACT_TRACING && (CONST_CHOSEN_CONTACT_TRACING_STRATEGY %in% CONTACT_TRACING_STRATEGY_NAMES == FALSE) ){
  cat("\n Required contact tracing strategy: ", CONST_CHOSEN_CONTACT_TRACING_STRATEGY, " is not implemented yet", sep ="")
  cat("\nTerminating simulation\n", sep = "")
  cat("\n", proc.time() - start_time, "\n")
  stop()
} else {
  cat("\nCONST_ENABLE_CONTACT_TRACING: ", CONST_ENABLE_CONTACT_TRACING)
  if (CONST_ENABLE_CONTACT_TRACING) { cat("\n '-CONST_CHOSEN_CONTACT_TRACING_STRATEGY: ", CONST_CHOSEN_CONTACT_TRACING_STRATEGY, sep = "") }
}
# Ref:
# Time is of the essence: impact of delays on effectiveness of contact tracing for COVID-19, a modelling study
# Mirjam E. Kretzschmar, Ganna Rozhnova, Martin Bootsma, Michiel van Boven, Janneke van de Wijgert, Marc Bonten
# medRxiv 2020.05.09.20096289; doi: https://doi.org/10.1101/2020.05.09.20096289
# Now published in The Lancet Public Health doi: 10.1016/S2468-2667(20)30157-2
# URI: https://www.thelancet.com/journals/lanpub/article/PIIS2468-2667(20)30157-2/fulltext#seccestitle80

# The authors estimated the relationship between reproduction number (R0) and Contact testing strategy
# https://www.thelancet.com/journals/lanpub/article/PIIS2468-2667(20)30157-2/fulltext#seccestitle160
# running stochastic simulations assuming without physical distancing, individuals have on average 4 close contacts
# and nine 9 contacts per day.
# These magic numbers based assuming number of close contacts follows a Possion distribution with mean mu1
# and casual contacts follows a negative binomial distribution mu2.
# Ref: Section 2.2 https://www.thelancet.com/cms/10.1016/S2468-2667(20)30157-2/attachment/b3b417b1-0a5e-4e37-8edc-3bdc0246f819/mmc1.pdf


# Remarks on the parameters with respect to our simulation
# Isolation parameters: author's parameter | Remarks
# ---
# Testing coverage: 80%  | Already parameterised using covid test seeking chance
# (D1) Testing delay/Isolation time: 4 days | Already parameterised using covid test result delay and efficacy and isolation compartments

# Conventional Tracing parameters: author's parameter | Remarks
# ---
# Testing coverage: 80% | Already parameterised using covid test result delay and efficacy and isolation compartments
# (Implicit) Detection of close and casual contacts | To filter from all neighbours w.r.t contact time
# (D1) Testing delay/Isolation time: 4 days (from day of positive test) | Already parameterised using covid test result delay and efficacy and isolation compartments
# (D2) Time to trace close contacts: 3 days | need to parameterise
# Time to trace other contacts: 3 days | Not sure how this works
# Tracing coverage of close contacts: 80% | need to parameterise
# Tracing coverage of casual contacts: 50% | need to parameterise
# Time traced back: 7 days | need to parameterise

# Mobile app based Tracing parameters: author's parameter | Remarks
# ---
# Testing coverage: [20%, 100%] | Already parameterised using covid test result delay and efficacy and isolation compartments
# (Implicit) Detection of close and casual contacts | To filter from all neighbours w.r.t contact time
# (D1) Testing delay/Isolation time: 0 days (from day of positive test) | Already parameterised using covid test result delay and efficacy and isolation compartments
# (D2) Time to trace close contacts: 0 days | need to parameterise
# Time to trace other contacts: 0 days | Not sure how this works
# Tracing coverage of close contacts: [20%, 100%] | need to parameterise
# Tracing coverage of casual contacts: [20%, 100%] | need to parameterise
# Time traced back: 7 days | need to parameterise

if( CONST_ENABLE_CONTACT_TRACING && (CONST_CHOSEN_CONTACT_TRACING_STRATEGY == "Conventional tracing") ){
  # Conventional Contact Tracing parameters
  CONST_CONTACT_TRACING_DELAY = 3 # "If a positive result is observed for simulation day D, request test on D + 3, for all traced contacts for range [D-6, D]"
  CONST_CONTACT_TRACING_COVERAGE_CLOSE_CONTACTS = 0.8 # "Randomly sample 80% of the traced close contacts for testing"
  CONST_CONTACT_TRACING_COVERAGE_CASUAL_CONTACTS = 0.5 # "Randomly sample 50% of the traced casual contacts for testing"
  CONST_CONTACT_TRACING_TIME_WINDOW = 7 # "Detect and discern close and casual contacts from the day of receiving a positive result + tracing delay"

} else if( CONST_ENABLE_CONTACT_TRACING && (CONST_CHOSEN_CONTACT_TRACING_STRATEGY == "Mobile app based tracing") ){
  # Mobile app based Contact Tracing parameters
  CONST_CONTACT_TRACING_DELAY = 0 # "If a positive result is observed for simulation day D, request test on D, for all traced contacts for range [D-6, D]"
  CONST_CONTACT_TRACING_COVERAGE_CLOSE_CONTACTS = 0.8 # "Randomly sample 80% of the traced close contacts for testing"
  CONST_CONTACT_TRACING_COVERAGE_CASUAL_CONTACTS = 0.8 # "Randomly sample 80% of the traced casual contacts for testing"
  CONST_CONTACT_TRACING_TIME_WINDOW = 7 # "Detect and discern close and casual contacts from the day of receiving a positive result + tracing delay"
}


# @Vaccination initial proportion

# Observational proportional data of vaccination status of population from 5 years and above
# CONST_y0 = 0.116909651
# CONST_y1 = 0.05952125148
# CONST_y2 = 0.5539364618
# CONST_y3 = 0.2175267146

# Caliberation mode (for models without vaccines)
CONST_y0 = 1.0
CONST_y1 = 0.0
CONST_y2 = 0.0
CONST_y3 = 0.0

# cat("Weighted diminish of max chance due to vaccination efficacy: ", (CONST_y0*1 + CONST_y1*(1 - CONST_epsilon_inf_omicron_01) + CONST_y2 * (1 - CONST_epsilon_inf_omicron_02) + CONST_y3 * (1 - CONST_epsilon_inf_omicron_03))/(CONST_y0 + CONST_y1 + CONST_y2 + CONST_y3))

COUNT_FIRST_DAY_DISEASE_IMPORT <- 4 # From Robert

# Base Infection probability function: MAX_CHANCE * tanh( <contact_time> / TIME_TILL_76p_CHANCE )

# Infection probability function <- contact time
getInfection_probability <- function(contact_time) {

  TIME_TILL_76p_CHANCE <- CONST_TIME_TILL_76p_CHANCE

  MAX_CHANCE <- CONST_MAX_CHANCE

  # TODO: Add a minimum cutoff time like Dr. Watmough suggested
  # Note: Since I am sequentially drawing from each contact as of now,
  #        must change the algorithm first @"Tag: Sequence of probabilities"

  return (MAX_CHANCE * tanh(contact_time / TIME_TILL_76p_CHANCE))

}


# Infection probability function <- relative infectivity of the variant and contact time
getInfection_probability_w_variant <- function(relative_infectivity, contact_time) {

  TIME_TILL_76p_CHANCE <- CONST_TIME_TILL_76p_CHANCE

  MAX_CHANCE <- CONST_MAX_CHANCE

  VARIANT_INFECTIVITY <- relative_infectivity

  return (MAX_CHANCE * VARIANT_INFECTIVITY * tanh(contact_time / TIME_TILL_76p_CHANCE))

}

# Infection probability w.r.t infectivity of the variant, vaccination status of potentially infected and contact time
getInfection_probability_w_variant_w_vaccine <- function(vaccine_odds, relative_infectivity, contact_time){


  TIME_TILL_76p_CHANCE <- CONST_TIME_TILL_76p_CHANCE

  MAX_CHANCE <- CONST_MAX_CHANCE

  VARIANT_INFECTIVITY <- relative_infectivity
  VACCINE_ODDS <- vaccine_odds # 1 - prob(getting infected with vaccine dose i)/prob(getting infected without vaccine)

  infection_prob_without_vaccine <- MAX_CHANCE * VARIANT_INFECTIVITY * tanh(contact_time / TIME_TILL_76p_CHANCE)

  return_prob <- infection_prob_without_vaccine

  if(!is.na(vaccine_odds)){
    return_prob <- (1 - VACCINE_ODDS) * infection_prob_without_vaccine

    # Debug:
    # cat("[Infection event calc] Contact time: ", contact_time,
    #     ", variant infectivity: ", VARIANT_INFECTIVITY,
    #     ", vaccination odds: ", VACCINE_ODDS,
    #     ", base infection chance: ", infection_prob_without_vaccine,
    #     ", corrected chance for vaccine: ", return_prob, "\n", sep ="")
  }

  else {
    # cat("[Infection event calc] Contact time: ", contact_time,
    #     ", variant infectivity: ", VARIANT_INFECTIVITY,
    #     # ", vaccination odds: ", VACCINE_ODDS,
    #     ", base infection chance: ", infection_prob_without_vaccine,
    #     # ", corrected chance for vaccine: ", return_prob,
    #     "\n", sep ="")
  }


  return (return_prob)
}

# Scale vaccine odds from 0.0 to 1.0 as a function of simulation day 0 to 14
# TODO: Associate curves with waning

# Linear scaling
scaleVaccineOddsLinear <- function(day_since_vaccinated){
  CONST_FULL_IMMUNITY_DAY = 14

  return (min( (day_since_vaccinated/CONST_FULL_IMMUNITY_DAY), 1.0))
}


# contact matrix (pairlist)
# CONTACT_MATRIX_DIR <- 'schedules_Campbellton_Control_Jan_12_2022'
CONTACT_MATRIX_DIR <- 'SIR_calib_contact_matrix'
# CONTACT_MATRIX_DIR <- 'schedules_Campbellton_NoSchool_Jan_12_2022'
# CONTACT_MATRIX_DIR <- 'Riverview_Schedule_02Jun22'
# CONTACT_MATRIX_DIR <- 'Riverview_Schedule_09Jun22'

# Contact matrix type configuration check
# -----
if (CONST_CHOSEN_CONTACT_MATRIX_TYPE %in% CONTACT_MATRIX_TYPE_NAMES == FALSE){
  cat("\n Required contact matrix type: ", CONST_CHOSEN_CONTACT_MATRIX_TYPE, " is not implemented yet", sep ="")
  cat("\nTerminating simulation\n", sep = "")
  cat("\n", proc.time() - start_time, "\n")
  stop()
}

if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Aggregated") {
LIST_OF_CONTACT_MATRIX_FILEPATH  <-
  list.files(CONTACT_MATRIX_DIR, full.names = TRUE, pattern="contact\\_matrix\\-total\\-real\\.csv$", ignore.case = TRUE)
} else if (CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Location based"){
  LIST_OF_CONTACT_MATRIX_FILEPATH  <-
    list.files(CONTACT_MATRIX_DIR, full.names = TRUE, pattern="contact\\_matrix\\-per\\-location\\-real\\.csv$", ignore.case = TRUE)
}

# Quick and dirty debug counters
COUNT_NO_OF_INFECTION_COIN_FLIPS = 0
COUNT_NO_OF_INFECTION_EVENTS = 0



# Demography metadata
DEMOGRAPHIC_DATA_FILEPATH <- 'scenarioBenchmark/demographicInfo-Campbellton.json'


# output directory
MAIN_DIR = paste(SIM_SCRIPT_NAME, "_output", sep = "")
dir.create(file.path(MAIN_DIR) , showWarnings = FALSE)

SUB_DIR = CONTACT_MATRIX_DIR
dir.create(file.path(MAIN_DIR, SUB_DIR) , showWarnings = FALSE)


OUTPUT_DIRECTORY <- paste(MAIN_DIR,  "/",
                          SUB_DIR, "/",
                          sep = "")

{
# Variants of Concern
#
# Ref: https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection/health-professionals/testing-diagnosing-case-reporting/sars-cov-2-variants-national-definitions-classifications-public-health-actions.html#a3
# Last accessed on: 15 September 2021
#
# WHO Label | Pango Lineage | **Relative infectivity | **Vaccine efficacy per dose | Initial spread proportions
#
# ** From other sources listed below

VOC_COLNAMES =  c("WHO label",
                  "variant",
                  "Relative infectivity",
                  "Vaccine dose 1 efficacy",
                  "Vaccine dose 2 efficacy",
                  "Vaccine dose 3 efficacy",
                  "Proportion")

CONST_mu_1 = 1.0 # infectiousness of wild variant treated as default
CONST_mu_2 = 1.5 # infectiousness of Alpha w.r.t Wild variant
CONST_mu_3 = 1.8 # infectiousness of Delta w.r.t Alpha variant
CONST_mu_4 = 3.42257472363763 # infectiousness of Omicron w.r.t Wild variant, as per Robert
# CONST_mu_4 = 3.9 # infectiousness of Omicron w.r.t Wild variant, as suggested by Dr. Seahra

# For people who are vaccinated efficacy odds parameters (for wild variant)
CONST_epsilon_inf_wild_01 <- 0.6  # 1 - CONST_epsilon_inf_wild_01 * prob of infection without vaccine = Prob infection from wild variant with 1 dose
CONST_epsilon_inf_wild_02 <- 0.95 # 1 - CONST_epsilon_inf_wild_02 * prob of infection without vaccine = Prob infection from wild variant with 2 dose
CONST_epsilon_inf_wild_03 <- 0.95 # TODO: Check with Dr. Seahra, 1 - CONST_epsilon_inf_wild_03 * prob of infection without vaccine = Prob infection from wild variant with 3 dose

voc_default <- data.frame("Wild",
                          "A",
                          CONST_mu_1,
                          CONST_epsilon_inf_wild_01,
                          CONST_epsilon_inf_wild_02,
                          CONST_epsilon_inf_wild_03,
                          NA)

colnames(voc_default) <- VOC_COLNAMES
voc_df <- voc_default


# According to early study in UK Oct - Nov 2020
# - Leung, Shum, Leung, Lam, & Wu et al, 2021
# Ref: https://pubmed.ncbi.nlm.nih.gov/33413740/


# For people who are vaccinated efficacy odds parameters (for Alpha variant)
CONST_epsilon_inf_alpha_01 <- 0.6
CONST_epsilon_inf_alpha_02 <- 0.95
CONST_epsilon_inf_alpha_03 <- 0.95 # TODO: Check with Dr. Seahra

voc_01 <- data.frame("Alpha",
                     "B.1.1.7",
                     CONST_mu_1 * CONST_mu_2,
                     CONST_epsilon_inf_alpha_01,
                     CONST_epsilon_inf_alpha_02,
                     CONST_epsilon_inf_alpha_03,
                     NA)

colnames(voc_01) <- VOC_COLNAMES
voc_df <- rbind(voc_df, voc_01)


voc_02 <- data.frame("Beta",
                     list(c("B.1.351", "B.1.351.1", "B.1.351.2", "B.1.351.3", "B.1.351.4")),
                     NA,
                     NA,
                     NA,
                     NA,
                     NA)

colnames(voc_02) <- VOC_COLNAMES
voc_df <- rbind(voc_df, voc_02)

voc_03 <- data.frame("Gamma",
                     list(c("P.1", "P.1.1", "P.1.2")),
                     NA,
                     NA,
                     NA,
                     NA,
                     NA)

colnames(voc_03) <- VOC_COLNAMES
voc_df <- rbind(voc_df, voc_03)


# For people who are vaccinated efficacy odds parameters (for Delta variant)
CONST_epsilon_inf_delta_01 <- 0.35
CONST_epsilon_inf_delta_02 <- 0.85
CONST_epsilon_inf_delta_03 <- 0.85 # TODO: Check with Dr. Seahra

voc_04 <- data.frame("Delta",
                     list(c("B.1.617.2", "AY.1", "AY.2", "AY.3", "AY.3.1")),
                     CONST_mu_1 * CONST_mu_2 * CONST_mu_3,
                     CONST_epsilon_inf_delta_01,
                     CONST_epsilon_inf_delta_02,
                     NA,
                     NA)

colnames(voc_04) <- VOC_COLNAMES
voc_df <- rbind(voc_df, voc_04)


# For people who are vaccinated, efficacy odds parameters for Omicron variant
CONST_epsilon_inf_omicron_01 <- 0.175
CONST_epsilon_inf_omicron_02 <- 0.425
CONST_epsilon_inf_omicron_03 <- 0.725

voc_05 <- data.frame("Omicron",
                     "B.1.1.529",
                     CONST_mu_1 * CONST_mu_4, # CONST_mu_1 * CONST_mu_2 * CONST_mu_3 * CONST_mu_4,
                     CONST_epsilon_inf_omicron_01,
                     CONST_epsilon_inf_omicron_02,
                     CONST_epsilon_inf_omicron_03,
                     NA)

colnames(voc_05) <- VOC_COLNAMES
voc_df <- rbind(voc_df, voc_05)

}




# Utility function simulating SQL Update Query
# Ref: https://stackoverflow.com/a/34096575/15175554
{
  mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
    condition <- eval(substitute(condition), .data, envir)
    .data[condition, ] <- .data[condition, ] %>% mutate(...)
    .data
  }
}


# To determine relative proportion of variants in target geography (Cambellton, NB)
{
  # Based on Health Canada, ref: https://health-infobase.canada.ca/covid-19/epidemiological-summary-covid-19-cases.html#VOC
  # Accessed on: 13 Oct 2021, 14:26 AST
  #
  # and https://nccid.ca/covid-19-variants/
  #
  # as of October 8, 2021, 4 pm EDT
  #
  # New Brunswick has the following active variants proportions
  #  - Alpha, B.1.1.7, 0.9 %
  #  - Delta,  B.1.617.2, 98.9 %
  # --------------------------------------------
  # as of Jan 28, 2022, 4 pm EDT
  #
  # New Brunswick has the following active variants proportions
  #  - Delta,    B.1.617.2, 7.9 %
  #  - Omicron,  B.1.617.2, 92.0 %

  # Wild Calibration mode:
  voc_df <- voc_df %>% mutate_cond(`WHO label` == "Wild" & `variant` == "A", 'Proportion' = 100.0)

  # Omicron calibration mode:
  # voc_df <- voc_df %>% mutate_cond(`WHO label` == "Omicron" & `variant` == "B.1.1.529", 'Proportion' = 100.0)

  # Debug initial infection setup [Works - Feb 2]
  # voc_df <- voc_df %>% mutate_cond(`WHO label` == "Omicron" & `variant` == "B.1.1.529", 'Proportion' = 20)
  # voc_df <- voc_df %>% mutate_cond(`WHO label` == "Delta" & `variant` == "B.1.617.2", 'Proportion' = 20)
  # voc_df <- voc_df %>% mutate_cond(`WHO label` == "Wild" & `variant` == "A", 'Proportion' = 20)
  # voc_df <- voc_df %>% mutate_cond(`WHO label` == "Alpha" & `variant` == "B.1.1.7", 'Proportion' = 20)
  # voc_df <- voc_df %>% mutate_cond(`WHO label` == "Delta" & `variant` == "B.1.617.2", 'Proportion' = 20)

  # voc_df <- voc_df %>% mutate_cond(`WHO label` == "Alpha" & `variant` == "B.1.1.7", 'Proportion' = 0.9)
  # voc_df <- voc_df %>% mutate_cond(`WHO label` == "Delta" & `variant` == "B.1.617.2", 'Proportion' = 98.9)

  # Debug: infectivity proportions post transition
  # voc_df <- voc_df %>% mutate_cond(`WHO label` == "Wild" & `variant` == "A", 'Proportion' = 33.34)
  # voc_df <- voc_df %>% mutate_cond(`WHO label` == "Alpha" & `variant` == "B.1.1.7", 'Proportion' = 33.33)
  # voc_df <- voc_df %>% mutate_cond(`WHO label` == "Delta" & `variant` == "B.1.617.2", 'Proportion' = 33.33)

  # voc_df <- voc_df %>% mutate_cond(`WHO label` == "Wild" & `variant` == "A", 'Proportion' = 6/9)
  # voc_df <- voc_df %>% mutate_cond(`WHO label` == "Alpha" & `variant` == "B.1.1.7", 'Proportion' = 2/9)
  # voc_df <- voc_df %>% mutate_cond(`WHO label` == "Delta" & `variant` == "B.1.617.2", 'Proportion' = 1/9)
}



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

# Disease model configuration check
# -----
if (CONST_CHOSEN_DISEASE_MODEL %in% DISEASE_MODEL_NAMES == FALSE){
  cat("\n Required disease model: ", CONST_CHOSEN_DISEASE_MODEL, " is not implemented yet", sep ="")
  cat("\nTerminating simulation\n", sep = "")
  cat("\n", proc.time() - start_time, "\n")
  stop()
}

# If-else ladder of disease model construction follows

if (CONST_CHOSEN_DISEASE_MODEL == "Complex") {
  # Begin construction of complex disease model
  # ----------------------------
  cat("\nConstructing disease model: ", CONST_CHOSEN_DISEASE_MODEL, sep = "")
  {

    disease_model <- graph( edges = c("susceptible", "00_latent_infections_not_isolated", # φ(1, 0) - (1 - q) → 1)
                                      "susceptible", "00_latent_infections_isolated", # φ(1, 0) - (q) → 2)

                                      "00_latent_infections_not_isolated", "00_pre_symptomatic_non_isolated", # π(1 → 3)
                                      "00_latent_infections_isolated", "00_pre_symptomatic_isolated", # π(2 → 4)

                                      "00_pre_symptomatic_non_isolated", "00_mild_non_isolated", # π(3 → 5)
                                      "00_pre_symptomatic_non_isolated", "00_mild_isolated", # π(3 → 6)
                                      "00_pre_symptomatic_non_isolated", "00_hospitalized", # π(3 → 7)
                                      "00_pre_symptomatic_non_isolated", "00_hospitalized_ICU", # π(3 → 8)

                                      "00_pre_symptomatic_isolated", "00_mild_isolated", # π(4 → 6)
                                      "00_pre_symptomatic_isolated", "00_hospitalized", # π(4 → 7)
                                      "00_pre_symptomatic_isolated", "00_hospitalized_ICU", # π(4 → 8)

                                      "00_mild_isolated", "00_recovered", # π(5 → r)
                                      "00_mild_non_isolated", "00_recovered", # π(6 → r)
                                      "00_hospitalized", "00_recovered", # π(7 → r)
                                      "00_hospitalized_ICU", "00_recovered", # π(8 → r)

                                      "00_hospitalized", "00_dead", # π(7 → d)
                                      "00_hospitalized_ICU", "00_dead"), # π(8 → d)
                            directed = TRUE)

    # Vaccinations
    disease_model <- disease_model + graph( edges = c("susceptible","received_dose1",
                                      "received_dose1","received_dose2",
                                      "received_dose2","received_dose3"), directed = TRUE)

    # Vaccination/day parameters, look up @Vaccination Rates
    E(disease_model, path = c("susceptible", "received_dose1") )$weight <- 0.0
    E(disease_model, path = c("susceptible", "received_dose1") )$label <- paste0("BASED ON VACCINE DISBURSAL DATA")

    E(disease_model, path = c("received_dose1", "received_dose2") )$weight <- 0.0
    E(disease_model, path = c("received_dose1", "received_dose2") )$label <- paste0("BASED ON VACCINE DISBURSAL DATA")

    E(disease_model, path = c("received_dose2", "received_dose3") )$weight <- 0.0
    E(disease_model, path = c("received_dose2", "received_dose3") )$label <- paste0("BASED ON VACCINE DISBURSAL DATA")


    # For people who are not vaccinated: Infection transition/day parameters
    # Note: change to a matrix format with const_label, received_dose_1, received_dose_2, ...

    CONST_CHI <- 2/5         # inverse of mean length of infected but not infectious (latent) period
    CONST_KAPPA <- 2/5       # inverse of mean length of pre-symptomatic period
    CONST_rho <- 1/10        # inverse of mean length of non-ICU based hospitalization
    CONST_sigma <- 1/10      # inverse of mean length of ICU based hospitalization
    CONST_eta <- 1/5         # inverse of mean length of of asymptomatic/mild symptomatic period

    # Calibrating with Robert's help
    CONST_nu_m <- 0.9812     # fraction of cases never hospitalized (-> mild)
    CONST_nu_c <- 0.0028      # fraction of cases hospitalized in ICU for part of stay
    CONST_nu_h <- 0.0160       # fraction of cases hospitalized but never in ICU

    # CONST_nu_m <- 0.960       # fraction of cases never hospitalized (-> mild)
    # CONST_nu_c <- 0.006       # fraction of cases hospitalized in ICU for part of stay
    # CONST_nu_h <- 0.034       # fraction of cases hospitalized but never in ICU

    # CONST_nu_m + CONST_nu_c + CONST_nu_h must be 1

    CONST_f_c <- 1/2        # fraction of ICU hospitalization resulting in death
    CONST_f_h <- 1/10       # fraction of non-ICU hospitalization resulting in death

    # Calibrating with Robert's help
    CONST_p <- 0.45         # fraction of non-hospitalized cases that isolate late
    # CONST_p <- 0.27         # fraction of non-hospitalized cases that isolate late
    CONST_q <- 0.0694       # fraction of all cases that isolate early


    CONST_epsilon_trans_01 <- 0.0     # Assuming no change in transmission
    CONST_epsilon_trans_02 <- 0.0     # Assuming no change in transmission
    CONST_epsilon_trans_03 <- 0.0     # TODO: Check with Dr. Seahra, Assuming no change in transmission

    CONST_epsilon_serious_01 <- 0.5   # Assuming for every 100 cases of hospitalization, 50 cases are with 1 dose of vaccine vs. no vaccination
    CONST_epsilon_serious_02 <- 0.75  # For every 100 cases of hospitalization, 25 cases are with 2 doses of vaccine vs. no vaccination
    CONST_epsilon_serious_03 <- 0.75  # TODO: Check with Dr. Seahra, For every 100 cases of hospitalization, 25 cases are with 2 doses of vaccine vs. no vaccination


    # Calibration mode:

    # From susceptible
    E(disease_model, path = c("susceptible", "00_latent_infections_isolated") )$weight = "CALCULATE_FROM_CONTACT_MATRIX"
    E(disease_model, path = c("susceptible", "00_latent_infections_isolated") )$label = paste0("if infected, then q = ", CONST_q)

    E(disease_model, path = c("susceptible", "00_latent_infections_not_isolated") )$weight = "CALCULATE_FROM_CONTACT_MATRIX"
    E(disease_model, path = c("susceptible", "00_latent_infections_not_isolated") )$label = paste0("if infected, then (1 - q) = ", (1 - CONST_q))


    # From Latent
    E(disease_model, path = c("00_latent_infections_not_isolated", "00_pre_symptomatic_non_isolated") )$weight <- CONST_CHI
    E(disease_model, path = c("00_latent_infections_not_isolated", "00_pre_symptomatic_non_isolated") )$label <- paste0("(1 → 3) = ", CONST_CHI)

    E(disease_model, path = c("00_latent_infections_isolated", "00_pre_symptomatic_isolated") )$weight <- CONST_CHI
    E(disease_model, path = c("00_latent_infections_isolated", "00_pre_symptomatic_isolated") )$label <- paste0("(2 → 4) = ", CONST_CHI)


    # From Pre-symptomatic
    E(disease_model, path = c("00_pre_symptomatic_non_isolated", "00_mild_non_isolated") )$weight <- CONST_nu_m * (1 - CONST_p) * CONST_KAPPA
    E(disease_model, path = c("00_pre_symptomatic_non_isolated", "00_mild_non_isolated") )$label  <- paste0("(3 → 5) = ", CONST_nu_m * (1 - CONST_p) * CONST_KAPPA)

    E(disease_model, path = c("00_pre_symptomatic_non_isolated", "00_mild_isolated") )$weight <- CONST_nu_m * CONST_p * CONST_KAPPA
    E(disease_model, path = c("00_pre_symptomatic_non_isolated", "00_mild_isolated") )$label <- paste0("(3 → 6) = ", CONST_nu_m * CONST_p * CONST_KAPPA)

    E(disease_model, path = c("00_pre_symptomatic_non_isolated", "00_hospitalized") )$weight <- CONST_nu_h * CONST_KAPPA
    E(disease_model, path = c("00_pre_symptomatic_non_isolated", "00_hospitalized") )$label <- paste0("(3 → 7) = ", CONST_nu_h  * CONST_KAPPA)

    E(disease_model, path = c("00_pre_symptomatic_non_isolated", "00_hospitalized_ICU") )$weight <- CONST_nu_c * CONST_KAPPA
    E(disease_model, path = c("00_pre_symptomatic_non_isolated", "00_hospitalized_ICU") )$label <- paste0("(3 → 8) = ", CONST_nu_c  * CONST_KAPPA)


    E(disease_model, path = c("00_pre_symptomatic_isolated", "00_mild_isolated") )$weight <- CONST_nu_m * CONST_KAPPA
    E(disease_model, path = c("00_pre_symptomatic_isolated", "00_mild_isolated") )$label <- paste0("(4 → 6) = ", CONST_nu_m * CONST_KAPPA)

    E(disease_model, path = c("00_pre_symptomatic_isolated", "00_hospitalized") )$weight <- CONST_nu_h * CONST_KAPPA
    E(disease_model, path = c("00_pre_symptomatic_isolated", "00_hospitalized") )$label <- paste0("(4 → 7) = ", CONST_nu_h  * CONST_KAPPA)

    E(disease_model, path = c("00_pre_symptomatic_isolated", "00_hospitalized_ICU") )$weight <- CONST_nu_c * CONST_KAPPA
    E(disease_model, path = c("00_pre_symptomatic_isolated", "00_hospitalized_ICU") )$label <- paste0("(4 → 8) = ", CONST_nu_c  * CONST_KAPPA)



    # From mild
    E(disease_model, path = c("00_mild_isolated", "00_recovered") )$weight <- CONST_eta
    E(disease_model, path = c("00_mild_isolated", "00_recovered") )$label <- paste0("(6 → r) = ", CONST_eta)

    E(disease_model, path = c("00_mild_non_isolated", "00_recovered") )$weight <- CONST_eta
    E(disease_model, path = c("00_mild_non_isolated", "00_recovered") )$label <- paste0("(5 → r) = ", CONST_eta)



    # From hospitalized
    E(disease_model, path = c("00_hospitalized", "00_recovered") )$weight <- CONST_rho * (1 - CONST_f_h)
    E(disease_model, path = c("00_hospitalized", "00_recovered") )$label <- paste0("(7 → r) = ", CONST_rho * (1 - CONST_f_h))

    E(disease_model, path = c("00_hospitalized", "00_dead") )$weight <- CONST_rho * CONST_f_h
    E(disease_model, path = c("00_hospitalized", "00_dead") )$label <- paste0("(7 → d) = ", CONST_rho * CONST_f_h)



    # From hospitalized with ICU usage
    E(disease_model, path = c("00_hospitalized_ICU", "00_recovered") )$weight <- CONST_sigma * (1 - CONST_f_c)
    E(disease_model, path = c("00_hospitalized_ICU", "00_recovered") )$label <- paste0("(8 → r) = ", CONST_sigma * (1 - CONST_f_c))

    E(disease_model, path = c("00_hospitalized_ICU", "00_dead") )$weight <- CONST_sigma * CONST_f_c
    E(disease_model, path = c("00_hospitalized_ICU", "00_dead") )$label <- paste0("(8 → d) = ", CONST_sigma * CONST_f_c)



  }

  # Infection parameters for people with 1st dose of vaccination
  {
    # Infection states for 1 doses:
    disease_model <- disease_model+
      graph( edges = c("01_latent_infections_not_isolated", "01_pre_symptomatic_non_isolated", # π(1 → 3)
                       "01_latent_infections_isolated", "01_pre_symptomatic_isolated", # π(2 → 4)

                       "01_pre_symptomatic_non_isolated", "01_mild_non_isolated", # π(3 → 5)
                       "01_pre_symptomatic_non_isolated", "01_mild_isolated", # π(3 → 6)
                       "01_pre_symptomatic_non_isolated", "01_hospitalized", # π(3 → 7)
                       "01_pre_symptomatic_non_isolated", "01_hospitalized_ICU", # π(3 → 8)

                       "01_pre_symptomatic_isolated", "01_mild_isolated", # π(4 → 6)
                       "01_pre_symptomatic_isolated", "01_hospitalized", # π(4 → 7)
                       "01_pre_symptomatic_isolated", "01_hospitalized_ICU", # π(4 → 8)

                       "01_mild_isolated", "01_recovered", # π(5 → r)
                       "01_mild_non_isolated", "01_recovered", # π(6 → r)
                       "01_hospitalized", "01_recovered", # π(7 → r)
                       "01_hospitalized_ICU", "01_recovered", # π(8 → r)

                       "01_hospitalized", "01_dead", # π(7 → d)
                       "01_hospitalized_ICU", "01_dead"), # π(8 → d)
             directed = TRUE)

    # From Latent
    E(disease_model, path = c("01_latent_infections_not_isolated", "01_pre_symptomatic_non_isolated") )$weight <- CONST_CHI
    E(disease_model, path = c("01_latent_infections_not_isolated", "01_pre_symptomatic_non_isolated") )$label <- paste0("(1 → 3) = ", CONST_CHI)

    E(disease_model, path = c("01_latent_infections_isolated", "01_pre_symptomatic_isolated") )$weight <- CONST_CHI
    E(disease_model, path = c("01_latent_infections_isolated", "01_pre_symptomatic_isolated") )$label <- paste0("(2 → 4) = ", CONST_CHI)


    # From Pre-symptomatic
    E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_mild_non_isolated") )$weight <- ( CONST_nu_m + CONST_epsilon_serious_01 * (1 - CONST_nu_m)  )* (1 - CONST_p) * CONST_KAPPA
    E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_mild_non_isolated") )$label  <- paste0("(3 → 5) = ", ( CONST_nu_m + CONST_epsilon_serious_01 * (1 - CONST_nu_m)  )* (1 - CONST_p) * CONST_KAPPA)

    E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_mild_isolated") )$weight <- ( CONST_nu_m + CONST_epsilon_serious_01 * (1 - CONST_nu_m)  ) * CONST_p * CONST_KAPPA
    E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_mild_isolated") )$label <- paste0("(3 → 6) = ", ( CONST_nu_m + CONST_epsilon_serious_01 * (1 - CONST_nu_m)  ) * CONST_p * CONST_KAPPA)

    E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_hospitalized") )$weight <- CONST_nu_h * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
    E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_hospitalized") )$label <- paste0("(3 → 7) = ", CONST_nu_h * (1 - CONST_epsilon_serious_01)  * CONST_KAPPA)

    E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_hospitalized_ICU") )$weight <- CONST_nu_c * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
    E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_hospitalized_ICU") )$label <- paste0("(3 → 8) = ", CONST_nu_c  * (1 - CONST_epsilon_serious_01) * CONST_KAPPA)



    E(disease_model, path = c("01_pre_symptomatic_isolated", "01_mild_isolated") )$weight <- ( CONST_nu_m + CONST_epsilon_serious_01 * (1 - CONST_nu_m)  ) * CONST_KAPPA
    E(disease_model, path = c("01_pre_symptomatic_isolated", "01_mild_isolated") )$label <- paste0("(4 → 6) = ", ( CONST_nu_m + CONST_epsilon_serious_01 * (1 - CONST_nu_m)  ) * CONST_KAPPA)

    E(disease_model, path = c("01_pre_symptomatic_isolated", "01_hospitalized") )$weight <- CONST_nu_h * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
    E(disease_model, path = c("01_pre_symptomatic_isolated", "01_hospitalized") )$label <- paste0("(4 → 7) = ", CONST_nu_h  * (1 - CONST_epsilon_serious_01) * CONST_KAPPA)

    E(disease_model, path = c("01_pre_symptomatic_isolated", "01_hospitalized_ICU") )$weight <- CONST_nu_c * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
    E(disease_model, path = c("01_pre_symptomatic_isolated", "01_hospitalized_ICU") )$label <- paste0("(4 → 8) = ", CONST_nu_c * (1 - CONST_epsilon_serious_01) * CONST_KAPPA)




    # From mild
    E(disease_model, path = c("01_mild_isolated", "01_recovered") )$weight <- CONST_eta
    E(disease_model, path = c("01_mild_isolated", "01_recovered") )$label <- paste0("(6 → r) = ", CONST_eta)

    E(disease_model, path = c("01_mild_non_isolated", "01_recovered") )$weight <- CONST_eta
    E(disease_model, path = c("01_mild_non_isolated", "01_recovered") )$label <- paste0("(5 → r) = ", CONST_eta)



    # From hospitalized
    E(disease_model, path = c("01_hospitalized", "01_recovered") )$weight <- CONST_rho * (1 - CONST_f_h)
    E(disease_model, path = c("01_hospitalized", "01_recovered") )$label <- paste0("(7 → r) = ", CONST_rho * (1 - CONST_f_h))

    E(disease_model, path = c("01_hospitalized", "01_dead") )$weight <- CONST_rho * CONST_f_h
    E(disease_model, path = c("01_hospitalized", "01_dead") )$label <- paste0("(7 → d) = ", CONST_rho * CONST_f_h)


    # From hospitalized with ICU usage
    E(disease_model, path = c("01_hospitalized_ICU", "01_recovered") )$weight <- CONST_sigma * (1 - CONST_f_c)
    E(disease_model, path = c("01_hospitalized_ICU", "01_recovered") )$label <- paste0("(8 → r) = ", CONST_sigma * (1 - CONST_f_c))

    E(disease_model, path = c("01_hospitalized_ICU", "01_dead") )$weight <- CONST_sigma * CONST_f_c
    E(disease_model, path = c("01_hospitalized_ICU", "01_dead") )$label <- paste0("(8 → d) = ", CONST_sigma * CONST_f_c)


    # For all the states of Wild, Alpha, Beta, Delta infection with 1 dose of vaccine
    # Connect to main transition graph
    disease_model <- disease_model + edge("received_dose1", "01_latent_infections_isolated",
                                          weight = "CALCULATE_FROM_CONTACT_MATRIX",
                                          label = paste0("if infected, then q = ", CONST_q))

    disease_model <- disease_model + edge("received_dose1", "01_latent_infections_not_isolated",
                                          weight = "CALCULATE_FROM_CONTACT_MATRIX",
                                          label = paste0("if infected, then (1 - q) = ", (1 - CONST_q)))

  }



  # Infection parameters for people with 2nd dose of vaccination
  {
    # Infection states for 2 doses:
    disease_model <- disease_model+
      graph( edges = c("02_latent_infections_not_isolated", "02_pre_symptomatic_non_isolated", # π(1 → 3)
                       "02_latent_infections_isolated", "02_pre_symptomatic_isolated", # π(2 → 4)

                       "02_pre_symptomatic_non_isolated", "02_mild_non_isolated", # π(3 → 5)
                       "02_pre_symptomatic_non_isolated", "02_mild_isolated", # π(3 → 6)
                       "02_pre_symptomatic_non_isolated", "02_hospitalized", # π(3 → 7)
                       "02_pre_symptomatic_non_isolated", "02_hospitalized_ICU", # π(3 → 8)

                       "02_pre_symptomatic_isolated", "02_mild_isolated", # π(4 → 6)
                       "02_pre_symptomatic_isolated", "02_hospitalized", # π(4 → 7)
                       "02_pre_symptomatic_isolated", "02_hospitalized_ICU", # π(4 → 8)

                       "02_mild_isolated", "02_recovered", # π(5 → r)
                       "02_mild_non_isolated", "02_recovered", # π(6 → r)
                       "02_hospitalized", "02_recovered", # π(7 → r)
                       "02_hospitalized_ICU", "02_recovered", # π(8 → r)

                       "02_hospitalized", "02_dead", # π(7 → d)
                       "02_hospitalized_ICU", "02_dead"), # π(8 → d)
             directed = TRUE)

    # From Latent
    E(disease_model, path = c("02_latent_infections_not_isolated", "02_pre_symptomatic_non_isolated") )$weight <- CONST_CHI
    E(disease_model, path = c("02_latent_infections_not_isolated", "02_pre_symptomatic_non_isolated") )$label <- paste0("(1 → 3) = ", CONST_CHI)

    E(disease_model, path = c("02_latent_infections_isolated", "02_pre_symptomatic_isolated") )$weight <- CONST_CHI
    E(disease_model, path = c("02_latent_infections_isolated", "02_pre_symptomatic_isolated") )$label <- paste0("(2 → 4) = ", CONST_CHI)


    # From Pre-symptomatic
    E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_mild_non_isolated") )$weight <- ( CONST_nu_m + CONST_epsilon_serious_02 * (1 - CONST_nu_m)  )* (1 - CONST_p) * CONST_KAPPA
    E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_mild_non_isolated") )$label  <- paste0("(3 → 5) = ", ( CONST_nu_m + CONST_epsilon_serious_02 * (1 - CONST_nu_m)  )* (1 - CONST_p) * CONST_KAPPA)

    E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_mild_isolated") )$weight <- ( CONST_nu_m + CONST_epsilon_serious_02 * (1 - CONST_nu_m)  ) * CONST_p * CONST_KAPPA
    E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_mild_isolated") )$label <- paste0("(3 → 6) = ", ( CONST_nu_m + CONST_epsilon_serious_02 * (1 - CONST_nu_m)  ) * CONST_p * CONST_KAPPA)

    E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_hospitalized") )$weight <- CONST_nu_h * (1 - CONST_epsilon_serious_02) * CONST_KAPPA
    E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_hospitalized") )$label <- paste0("(3 → 7) = ", CONST_nu_h * (1 - CONST_epsilon_serious_02)  * CONST_KAPPA)

    E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_hospitalized_ICU") )$weight <- CONST_nu_c * (1 - CONST_epsilon_serious_02) * CONST_KAPPA
    E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_hospitalized_ICU") )$label <- paste0("(3 → 8) = ", CONST_nu_c  * (1 - CONST_epsilon_serious_02) * CONST_KAPPA)



    E(disease_model, path = c("02_pre_symptomatic_isolated", "02_mild_isolated") )$weight <- ( CONST_nu_m + CONST_epsilon_serious_02 * (1 - CONST_nu_m)  ) * CONST_KAPPA
    E(disease_model, path = c("02_pre_symptomatic_isolated", "02_mild_isolated") )$label <- paste0("(4 → 6) = ", ( CONST_nu_m + CONST_epsilon_serious_02 * (1 - CONST_nu_m)  ) * CONST_KAPPA)

    E(disease_model, path = c("02_pre_symptomatic_isolated", "02_hospitalized") )$weight <- CONST_nu_h * (1 - CONST_epsilon_serious_02) * CONST_KAPPA
    E(disease_model, path = c("02_pre_symptomatic_isolated", "02_hospitalized") )$label <- paste0("(4 → 7) = ", CONST_nu_h  * (1 - CONST_epsilon_serious_02) * CONST_KAPPA)

    E(disease_model, path = c("02_pre_symptomatic_isolated", "02_hospitalized_ICU") )$weight <- CONST_nu_c * (1 - CONST_epsilon_serious_02) * CONST_KAPPA
    E(disease_model, path = c("02_pre_symptomatic_isolated", "02_hospitalized_ICU") )$label <- paste0("(4 → 8) = ", CONST_nu_c * (1 - CONST_epsilon_serious_02) * CONST_KAPPA)




    # From mild
    E(disease_model, path = c("02_mild_isolated", "02_recovered") )$weight <- CONST_eta
    E(disease_model, path = c("02_mild_isolated", "02_recovered") )$label <- paste0("(6 → r) = ", CONST_eta)

    E(disease_model, path = c("02_mild_non_isolated", "02_recovered") )$weight <- CONST_eta
    E(disease_model, path = c("02_mild_non_isolated", "02_recovered") )$label <- paste0("(5 → r) = ", CONST_eta)



    # From hospitalized
    E(disease_model, path = c("02_hospitalized", "02_recovered") )$weight <- CONST_rho * (1 - CONST_f_h)
    E(disease_model, path = c("02_hospitalized", "02_recovered") )$label <- paste0("(7 → r) = ", CONST_rho * (1 - CONST_f_h))

    E(disease_model, path = c("02_hospitalized", "02_dead") )$weight <- CONST_rho * CONST_f_h
    E(disease_model, path = c("02_hospitalized", "02_dead") )$label <- paste0("(7 → d) = ", CONST_rho * CONST_f_h)


    # From hospitalized with ICU usage
    E(disease_model, path = c("02_hospitalized_ICU", "02_recovered") )$weight <- CONST_sigma * (1 - CONST_f_c)
    E(disease_model, path = c("02_hospitalized_ICU", "02_recovered") )$label <- paste0("(8 → r) = ", CONST_sigma * (1 - CONST_f_c))

    E(disease_model, path = c("02_hospitalized_ICU", "02_dead") )$weight <- CONST_sigma * CONST_f_c
    E(disease_model, path = c("02_hospitalized_ICU", "02_dead") )$label <- paste0("(8 → d) = ", CONST_sigma * CONST_f_c)

    # For all the states of Wild, Alpha, Beta, Delta infection with 2 doses of vaccine
    # Connect to main transition graph
    disease_model <- disease_model + edge("received_dose2", "02_latent_infections_isolated",
                                          weight = "CALCULATE_FROM_CONTACT_MATRIX",
                                          label = paste0("if infected, then q = ", CONST_q))

    disease_model <- disease_model + edge("received_dose2", "02_latent_infections_not_isolated",
                                          weight = "CALCULATE_FROM_CONTACT_MATRIX",
                                          label = paste0("if infected, then (1 - q) = ", (1 - CONST_q)))

  }



  # Infection parameters for people with 3rd dose of vaccination
  {
    # Infection states for 3 doses:
    disease_model <- disease_model+
      graph( edges = c("03_latent_infections_not_isolated", "03_pre_symptomatic_non_isolated", # π(1 → 3)
                       "03_latent_infections_isolated", "03_pre_symptomatic_isolated", # π(2 → 4)

                       "03_pre_symptomatic_non_isolated", "03_mild_non_isolated", # π(3 → 5)
                       "03_pre_symptomatic_non_isolated", "03_mild_isolated", # π(3 → 6)
                       "03_pre_symptomatic_non_isolated", "03_hospitalized", # π(3 → 7)
                       "03_pre_symptomatic_non_isolated", "03_hospitalized_ICU", # π(3 → 8)

                       "03_pre_symptomatic_isolated", "03_mild_isolated", # π(4 → 6)
                       "03_pre_symptomatic_isolated", "03_hospitalized", # π(4 → 7)
                       "03_pre_symptomatic_isolated", "03_hospitalized_ICU", # π(4 → 8)

                       "03_mild_isolated", "03_recovered", # π(5 → r)
                       "03_mild_non_isolated", "03_recovered", # π(6 → r)
                       "03_hospitalized", "03_recovered", # π(7 → r)
                       "03_hospitalized_ICU", "03_recovered", # π(8 → r)

                       "03_hospitalized", "03_dead", # π(7 → d)
                       "03_hospitalized_ICU", "03_dead"), # π(8 → d)
             directed = TRUE)

    # From Latent
    E(disease_model, path = c("03_latent_infections_not_isolated", "03_pre_symptomatic_non_isolated") )$weight <- CONST_CHI
    E(disease_model, path = c("03_latent_infections_not_isolated", "03_pre_symptomatic_non_isolated") )$label <- paste0("(1 → 3) = ", CONST_CHI)

    E(disease_model, path = c("03_latent_infections_isolated", "03_pre_symptomatic_isolated") )$weight <- CONST_CHI
    E(disease_model, path = c("03_latent_infections_isolated", "03_pre_symptomatic_isolated") )$label <- paste0("(2 → 4) = ", CONST_CHI)


    # From Pre-symptomatic
    E(disease_model, path = c("03_pre_symptomatic_non_isolated", "03_mild_non_isolated") )$weight <- ( CONST_nu_m + CONST_epsilon_serious_03 * (1 - CONST_nu_m)  )* (1 - CONST_p) * CONST_KAPPA
    E(disease_model, path = c("03_pre_symptomatic_non_isolated", "03_mild_non_isolated") )$label  <- paste0("(3 → 5) = ", ( CONST_nu_m + CONST_epsilon_serious_03 * (1 - CONST_nu_m)  )* (1 - CONST_p) * CONST_KAPPA)

    E(disease_model, path = c("03_pre_symptomatic_non_isolated", "03_mild_isolated") )$weight <- ( CONST_nu_m + CONST_epsilon_serious_03 * (1 - CONST_nu_m)  ) * CONST_p * CONST_KAPPA
    E(disease_model, path = c("03_pre_symptomatic_non_isolated", "03_mild_isolated") )$label <- paste0("(3 → 6) = ", ( CONST_nu_m + CONST_epsilon_serious_03 * (1 - CONST_nu_m)  ) * CONST_p * CONST_KAPPA)

    E(disease_model, path = c("03_pre_symptomatic_non_isolated", "03_hospitalized") )$weight <- CONST_nu_h * (1 - CONST_epsilon_serious_03) * CONST_KAPPA
    E(disease_model, path = c("03_pre_symptomatic_non_isolated", "03_hospitalized") )$label <- paste0("(3 → 7) = ", CONST_nu_h * (1 - CONST_epsilon_serious_03)  * CONST_KAPPA)

    E(disease_model, path = c("03_pre_symptomatic_non_isolated", "03_hospitalized_ICU") )$weight <- CONST_nu_c * (1 - CONST_epsilon_serious_03) * CONST_KAPPA
    E(disease_model, path = c("03_pre_symptomatic_non_isolated", "03_hospitalized_ICU") )$label <- paste0("(3 → 8) = ", CONST_nu_c  * (1 - CONST_epsilon_serious_03) * CONST_KAPPA)



    E(disease_model, path = c("03_pre_symptomatic_isolated", "03_mild_isolated") )$weight <- ( CONST_nu_m + CONST_epsilon_serious_03 * (1 - CONST_nu_m)  ) * CONST_KAPPA
    E(disease_model, path = c("03_pre_symptomatic_isolated", "03_mild_isolated") )$label <- paste0("(4 → 6) = ", ( CONST_nu_m + CONST_epsilon_serious_03 * (1 - CONST_nu_m)  ) * CONST_KAPPA)

    E(disease_model, path = c("03_pre_symptomatic_isolated", "03_hospitalized") )$weight <- CONST_nu_h * (1 - CONST_epsilon_serious_03) * CONST_KAPPA
    E(disease_model, path = c("03_pre_symptomatic_isolated", "03_hospitalized") )$label <- paste0("(4 → 7) = ", CONST_nu_h  * (1 - CONST_epsilon_serious_03) * CONST_KAPPA)

    E(disease_model, path = c("03_pre_symptomatic_isolated", "03_hospitalized_ICU") )$weight <- CONST_nu_c * (1 - CONST_epsilon_serious_03) * CONST_KAPPA
    E(disease_model, path = c("03_pre_symptomatic_isolated", "03_hospitalized_ICU") )$label <- paste0("(4 → 8) = ", CONST_nu_c * (1 - CONST_epsilon_serious_03) * CONST_KAPPA)




    # From mild
    E(disease_model, path = c("03_mild_isolated", "03_recovered") )$weight <- CONST_eta
    E(disease_model, path = c("03_mild_isolated", "03_recovered") )$label <- paste0("(6 → r) = ", CONST_eta)

    E(disease_model, path = c("03_mild_non_isolated", "03_recovered") )$weight <- CONST_eta
    E(disease_model, path = c("03_mild_non_isolated", "03_recovered") )$label <- paste0("(5 → r) = ", CONST_eta)



    # From hospitalized
    E(disease_model, path = c("03_hospitalized", "03_recovered") )$weight <- CONST_rho * (1 - CONST_f_h)
    E(disease_model, path = c("03_hospitalized", "03_recovered") )$label <- paste0("(7 → r) = ", CONST_rho * (1 - CONST_f_h))

    E(disease_model, path = c("03_hospitalized", "03_dead") )$weight <- CONST_rho * CONST_f_h
    E(disease_model, path = c("03_hospitalized", "03_dead") )$label <- paste0("(7 → d) = ", CONST_rho * CONST_f_h)


    # From hospitalized with ICU usage
    E(disease_model, path = c("03_hospitalized_ICU", "03_recovered") )$weight <- CONST_sigma * (1 - CONST_f_c)
    E(disease_model, path = c("03_hospitalized_ICU", "03_recovered") )$label <- paste0("(8 → r) = ", CONST_sigma * (1 - CONST_f_c))

    E(disease_model, path = c("03_hospitalized_ICU", "03_dead") )$weight <- CONST_sigma * CONST_f_c
    E(disease_model, path = c("03_hospitalized_ICU", "03_dead") )$label <- paste0("(8 → d) = ", CONST_sigma * CONST_f_c)

    # For all the states of Wild, Alpha, Beta, Delta infection with 3 doses of vaccine
    # Connect to main transition graph
    disease_model <- disease_model + edge("received_dose3", "03_latent_infections_isolated",
                                          weight = "CALCULATE_FROM_CONTACT_MATRIX",
                                          label = paste0("if infected, then q = ", CONST_q))

    disease_model <- disease_model + edge("received_dose3", "03_latent_infections_not_isolated",
                                          weight = "CALCULATE_FROM_CONTACT_MATRIX",
                                          label = paste0("if infected, then (1 - q) = ", (1 - CONST_q)))

  }




  {

    # Omicron specific transitions

    CONST_CHI_omicron <- 2/3         # inverse of mean length of infected but not infectious (latent) period
    CONST_KAPPA_omicron <- 2/3       # inverse of mean length of pre-symptomatic period
    CONST_rho_omicron <- 1/4        # inverse of mean length of non-ICU based hospitalization
    CONST_sigma_omicron <- 0.1234567901      # inverse of mean length of ICU based hospitalization
    CONST_eta_omicron <- 1/5         # inverse of mean length of of asymptomatic/mild symptomatic period

    CONST_nu_m_omicron <- 0.98906     # fraction of cases never hospitalized (-> mild)
    CONST_nu_c_omicron <- 0.001641      # fraction of cases hospitalized in ICU for part of stay
    CONST_nu_h_omicron <- 0.009299       # fraction of cases hospitalized but never in ICU

    # CONST_nu_m + CONST_nu_c + CONST_nu_h must be 1

    CONST_f_c_omicron <- 1/2        # fraction of ICU hospitalization resulting in death
    CONST_f_h_omicron <- 1/10       # fraction of non-ICU hospitalization resulting in death

    # Calibrating with Robert's help
    CONST_p_omicron <- 0.45         # fraction of non-hospitalized cases that isolate late
    CONST_q_omicron <- 0.0694       # fraction of all cases that isolate early

    CONST_epsilon_trans_omicron_01 <- 0.0     # Assuming no change in transmission
    CONST_epsilon_trans_omicron_02 <- 0.0     # Assuming no change in transmission
    CONST_epsilon_trans_omicron_03 <- 0.0     # Assuming no change in transmission

    CONST_epsilon_serious_omicron_01 <- 0.5   # Assuming for every 100 cases of hospitalization, 50 cases are with 1 dose of vaccine vs. no vaccination
    CONST_epsilon_serious_omicron_02 <- 0.75  # For every 100 cases of hospitalization, 25 cases are with 2 doses of vaccine vs. no vaccination
    CONST_epsilon_serious_omicron_03 <- 0.75  # For every 100 cases of hospitalization, 25 cases are with 3 doses of vaccine vs. no vaccination

    disease_model <- disease_model +
                      graph( edges = c(
                                      "susceptible", "00_latent_infections_not_isolated_omicron", # φ(1, 0) - (1 - q) → 1)
                                      "susceptible", "00_latent_infections_isolated_omicron", # φ(1, 0) - (q) → 2)

                                      "00_latent_infections_not_isolated_omicron", "00_pre_symptomatic_non_isolated_omicron", # π(1 → 3)
                                      "00_latent_infections_isolated_omicron", "00_pre_symptomatic_isolated_omicron", # π(2 → 4)

                                      "00_pre_symptomatic_non_isolated_omicron", "00_mild_non_isolated_omicron", # π(3 → 5)
                                      "00_pre_symptomatic_non_isolated_omicron", "00_mild_isolated_omicron", # π(3 → 6)
                                      "00_pre_symptomatic_non_isolated_omicron", "00_hospitalized_omicron", # π(3 → 7)
                                      "00_pre_symptomatic_non_isolated_omicron", "00_hospitalized_ICU_omicron", # π(3 → 8)

                                      "00_pre_symptomatic_isolated_omicron", "00_mild_isolated_omicron", # π(4 → 6)
                                      "00_pre_symptomatic_isolated_omicron", "00_hospitalized_omicron", # π(4 → 7)
                                      "00_pre_symptomatic_isolated_omicron", "00_hospitalized_ICU_omicron", # π(4 → 8)

                                      "00_mild_isolated_omicron", "00_recovered_omicron", # π(5 → r)
                                      "00_mild_non_isolated_omicron", "00_recovered_omicron", # π(6 → r)
                                      "00_hospitalized_omicron", "00_recovered_omicron", # π(7 → r)
                                      "00_hospitalized_ICU_omicron", "00_recovered_omicron", # π(8 → r)

                                      "00_hospitalized_omicron", "00_dead_omicron", # π(7 → d)
                                      "00_hospitalized_ICU_omicron", "00_dead_omicron"), # π(8 → d)
                            directed = TRUE)

    # From susceptible
    E(disease_model, path = c("susceptible", "00_latent_infections_isolated_omicron") )$weight = "CALCULATE_FROM_CONTACT_MATRIX"
    E(disease_model, path = c("susceptible", "00_latent_infections_isolated_omicron") )$label = paste0("if infected, then q = ", CONST_q_omicron)

    E(disease_model, path = c("susceptible", "00_latent_infections_not_isolated_omicron") )$weight = "CALCULATE_FROM_CONTACT_MATRIX"
    E(disease_model, path = c("susceptible", "00_latent_infections_not_isolated_omicron") )$label = paste0("if infected, then (1 - q) = ", (1 - CONST_q_omicron))


    # From Latent
    E(disease_model, path = c("00_latent_infections_not_isolated_omicron", "00_pre_symptomatic_non_isolated_omicron") )$weight <- CONST_CHI_omicron
    E(disease_model, path = c("00_latent_infections_not_isolated_omicron", "00_pre_symptomatic_non_isolated_omicron") )$label <- paste0("(1 → 3) = ", CONST_CHI_omicron)

    E(disease_model, path = c("00_latent_infections_isolated_omicron", "00_pre_symptomatic_isolated_omicron") )$weight <- CONST_CHI_omicron
    E(disease_model, path = c("00_latent_infections_isolated_omicron", "00_pre_symptomatic_isolated_omicron") )$label <- paste0("(2 → 4) = ", CONST_CHI_omicron)


    # From Pre-symptomatic
    E(disease_model, path = c("00_pre_symptomatic_non_isolated_omicron", "00_mild_non_isolated_omicron") )$weight <- CONST_nu_m_omicron * (1 - CONST_p_omicron) * CONST_KAPPA_omicron
    E(disease_model, path = c("00_pre_symptomatic_non_isolated_omicron", "00_mild_non_isolated_omicron") )$label  <- paste0("(3 → 5) = ", CONST_nu_m_omicron * (1 - CONST_p_omicron) * CONST_KAPPA_omicron)

    E(disease_model, path = c("00_pre_symptomatic_non_isolated_omicron", "00_mild_isolated_omicron") )$weight <- CONST_nu_m_omicron * CONST_p_omicron * CONST_KAPPA_omicron
    E(disease_model, path = c("00_pre_symptomatic_non_isolated_omicron", "00_mild_isolated_omicron") )$label <- paste0("(3 → 6) = ", CONST_nu_m_omicron * CONST_p_omicron * CONST_KAPPA_omicron)

    E(disease_model, path = c("00_pre_symptomatic_non_isolated_omicron", "00_hospitalized_omicron") )$weight <- CONST_nu_h_omicron * CONST_KAPPA_omicron
    E(disease_model, path = c("00_pre_symptomatic_non_isolated_omicron", "00_hospitalized_omicron") )$label <- paste0("(3 → 7) = ", CONST_nu_h_omicron  * CONST_KAPPA_omicron)

    E(disease_model, path = c("00_pre_symptomatic_non_isolated_omicron", "00_hospitalized_ICU_omicron") )$weight <- CONST_nu_c_omicron * CONST_KAPPA_omicron
    E(disease_model, path = c("00_pre_symptomatic_non_isolated_omicron", "00_hospitalized_ICU_omicron") )$label <- paste0("(3 → 8) = ", CONST_nu_c_omicron  * CONST_KAPPA_omicron)


    E(disease_model, path = c("00_pre_symptomatic_isolated_omicron", "00_mild_isolated_omicron") )$weight <- CONST_nu_m_omicron * CONST_KAPPA_omicron
    E(disease_model, path = c("00_pre_symptomatic_isolated_omicron", "00_mild_isolated_omicron") )$label <- paste0("(4 → 6) = ", CONST_nu_m * CONST_KAPPA_omicron)

    E(disease_model, path = c("00_pre_symptomatic_isolated_omicron", "00_hospitalized_omicron") )$weight <- CONST_nu_h_omicron * CONST_KAPPA_omicron
    E(disease_model, path = c("00_pre_symptomatic_isolated_omicron", "00_hospitalized_omicron") )$label <- paste0("(4 → 7) = ", CONST_nu_h_omicron  * CONST_KAPPA_omicron)

    E(disease_model, path = c("00_pre_symptomatic_isolated_omicron", "00_hospitalized_ICU_omicron") )$weight <- CONST_nu_c * CONST_KAPPA_omicron
    E(disease_model, path = c("00_pre_symptomatic_isolated_omicron", "00_hospitalized_ICU_omicron") )$label <- paste0("(4 → 8) = ", CONST_nu_c_omicron  * CONST_KAPPA_omicron)



    # # From mild
    E(disease_model, path = c("00_mild_isolated_omicron", "00_recovered_omicron") )$weight <- CONST_eta_omicron
    E(disease_model, path = c("00_mild_isolated_omicron", "00_recovered_omicron") )$label <- paste0("(6 → r) = ", CONST_eta_omicron)

    E(disease_model, path = c("00_mild_non_isolated_omicron", "00_recovered_omicron") )$weight <- CONST_eta_omicron
    E(disease_model, path = c("00_mild_non_isolated_omicron", "00_recovered_omicron") )$label <- paste0("(5 → r) = ", CONST_eta_omicron)



    # From hospitalized
    E(disease_model, path = c("00_hospitalized_omicron", "00_recovered_omicron") )$weight <- CONST_rho_omicron * (1 - CONST_f_h_omicron)
    E(disease_model, path = c("00_hospitalized_omicron", "00_recovered_omicron") )$label <- paste0("(7 → r) = ", CONST_rho_omicron * (1 - CONST_f_h_omicron))

    E(disease_model, path = c("00_hospitalized_omicron", "00_dead_omicron") )$weight <- CONST_rho_omicron * CONST_f_h_omicron
    E(disease_model, path = c("00_hospitalized_omicron", "00_dead_omicron") )$label <- paste0("(7 → d) = ", CONST_rho_omicron * CONST_f_h_omicron)



    # From hospitalized with ICU usage
    E(disease_model, path = c("00_hospitalized_ICU_omicron", "00_recovered_omicron") )$weight <- CONST_sigma_omicron * (1 - CONST_f_c_omicron)
    E(disease_model, path = c("00_hospitalized_ICU_omicron", "00_recovered_omicron") )$label <- paste0("(8 → r) = ", CONST_sigma_omicron * (1 - CONST_f_c_omicron))

    E(disease_model, path = c("00_hospitalized_ICU_omicron", "00_dead_omicron") )$weight <- CONST_sigma_omicron * CONST_f_c_omicron
    E(disease_model, path = c("00_hospitalized_ICU_omicron", "00_dead_omicron") )$label <- paste0("(8 → d) = ", CONST_sigma_omicron * CONST_f_c_omicron)
  }

  # Infection parameters for people with 1 dose of vaccination w.r.t Omicron
  {
    disease_model <- disease_model+
      graph( edges = c("01_latent_infections_not_isolated_omicron", "01_pre_symptomatic_non_isolated_omicron", # π(1 → 3)
                       "01_latent_infections_isolated_omicron", "01_pre_symptomatic_isolated_omicron", # π(2 → 4)

                       "01_pre_symptomatic_non_isolated_omicron", "01_mild_non_isolated_omicron", # π(3 → 5)
                       "01_pre_symptomatic_non_isolated_omicron", "01_mild_isolated_omicron", # π(3 → 6)
                       "01_pre_symptomatic_non_isolated_omicron", "01_hospitalized_omicron", # π(3 → 7)
                       "01_pre_symptomatic_non_isolated_omicron", "01_hospitalized_ICU_omicron", # π(3 → 8)

                       "01_pre_symptomatic_isolated_omicron", "01_mild_isolated_omicron", # π(4 → 6)
                       "01_pre_symptomatic_isolated_omicron", "01_hospitalized_omicron", # π(4 → 7)
                       "01_pre_symptomatic_isolated_omicron", "01_hospitalized_ICU_omicron", # π(4 → 8)

                       "01_mild_isolated_omicron", "01_recovered_omicron", # π(5 → r)
                       "01_mild_non_isolated_omicron", "01_recovered_omicron", # π(6 → r)
                       "01_hospitalized_omicron", "01_recovered_omicron", # π(7 → r)
                       "01_hospitalized_ICU_omicron", "01_recovered_omicron", # π(8 → r)

                       "01_hospitalized_omicron", "01_dead_omicron", # π(7 → d)
                       "01_hospitalized_ICU_omicron", "01_dead_omicron"), # π(8 → d)
             directed = TRUE)

    # From Latent
    E(disease_model, path = c("01_latent_infections_not_isolated_omicron", "01_pre_symptomatic_non_isolated_omicron") )$weight <- CONST_CHI_omicron
    E(disease_model, path = c("01_latent_infections_not_isolated_omicron", "01_pre_symptomatic_non_isolated_omicron") )$label <- paste0("(1 → 3) = ", CONST_CHI_omicron)

    E(disease_model, path = c("01_latent_infections_isolated_omicron", "01_pre_symptomatic_isolated_omicron") )$weight <- CONST_CHI_omicron
    E(disease_model, path = c("01_latent_infections_isolated_omicron", "01_pre_symptomatic_isolated_omicron") )$label <- paste0("(2 → 4) = ", CONST_CHI_omicron)


    # From Pre-symptomatic
    E(disease_model, path = c("01_pre_symptomatic_non_isolated_omicron", "01_mild_non_isolated_omicron") )$weight <- ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_01 * (1 - CONST_nu_m_omicron)  )* (1 - CONST_p_omicron) * CONST_KAPPA_omicron
    E(disease_model, path = c("01_pre_symptomatic_non_isolated_omicron", "01_mild_non_isolated_omicron") )$label  <- paste0("(3 → 5) = ", ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_01 * (1 - CONST_nu_m_omicron)  )* (1 - CONST_p_omicron) * CONST_KAPPA_omicron)

    E(disease_model, path = c("01_pre_symptomatic_non_isolated_omicron", "01_mild_isolated_omicron") )$weight <- ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_01 * (1 - CONST_nu_m_omicron)  ) * CONST_p_omicron * CONST_KAPPA_omicron
    E(disease_model, path = c("01_pre_symptomatic_non_isolated_omicron", "01_mild_isolated_omicron") )$label <- paste0("(3 → 6) = ", ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_01 * (1 - CONST_nu_m_omicron)  ) * CONST_p_omicron * CONST_KAPPA_omicron)

    E(disease_model, path = c("01_pre_symptomatic_non_isolated_omicron", "01_hospitalized_omicron") )$weight <- CONST_nu_h_omicron * (1 - CONST_epsilon_serious_omicron_01) * CONST_KAPPA_omicron
    E(disease_model, path = c("01_pre_symptomatic_non_isolated_omicron", "01_hospitalized_omicron") )$label <- paste0("(3 → 7) = ", CONST_nu_h_omicron * (1 - CONST_epsilon_serious_omicron_01)  * CONST_KAPPA_omicron)

    E(disease_model, path = c("01_pre_symptomatic_non_isolated_omicron", "01_hospitalized_ICU_omicron") )$weight <- CONST_nu_c * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
    E(disease_model, path = c("01_pre_symptomatic_non_isolated_omicron", "01_hospitalized_ICU_omicron") )$label <- paste0("(3 → 8) = ", CONST_nu_c_omicron  * (1 - CONST_epsilon_serious_omicron_01) * CONST_KAPPA_omicron)



    E(disease_model, path = c("01_pre_symptomatic_isolated_omicron", "01_mild_isolated_omicron") )$weight <- ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_01 * (1 - CONST_nu_m_omicron)  ) * CONST_KAPPA_omicron
    E(disease_model, path = c("01_pre_symptomatic_isolated_omicron", "01_mild_isolated_omicron") )$label <- paste0("(4 → 6) = ", ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_01 * (1 - CONST_nu_m_omicron)  ) * CONST_KAPPA_omicron)

    E(disease_model, path = c("01_pre_symptomatic_isolated_omicron", "01_hospitalized_omicron") )$weight <- CONST_nu_h_omicron * (1 - CONST_epsilon_serious_omicron_01) * CONST_KAPPA_omicron
    E(disease_model, path = c("01_pre_symptomatic_isolated_omicron", "01_hospitalized_omicron") )$label <- paste0("(4 → 7) = ", CONST_nu_h_omicron  * (1 - CONST_epsilon_serious_omicron_01) * CONST_KAPPA_omicron)

    E(disease_model, path = c("01_pre_symptomatic_isolated_omicron", "01_hospitalized_ICU_omicron") )$weight <- CONST_nu_c_omicron * (1 - CONST_epsilon_serious_omicron_01) * CONST_KAPPA_omicron
    E(disease_model, path = c("01_pre_symptomatic_isolated_omicron", "01_hospitalized_ICU_omicron") )$label <- paste0("(4 → 8) = ", CONST_nu_c_omicron * (1 - CONST_epsilon_serious_omicron_01) * CONST_KAPPA_omicron)




    # From mild
    E(disease_model, path = c("01_mild_isolated_omicron", "01_recovered_omicron") )$weight <- CONST_eta_omicron
    E(disease_model, path = c("01_mild_isolated_omicron", "01_recovered_omicron") )$label <- paste0("(6 → r) = ", CONST_eta_omicron)

    E(disease_model, path = c("01_mild_non_isolated_omicron", "01_recovered_omicron") )$weight <- CONST_eta_omicron
    E(disease_model, path = c("01_mild_non_isolated_omicron", "01_recovered_omicron") )$label <- paste0("(5 → r) = ", CONST_eta_omicron)



    # From hospitalized
    E(disease_model, path = c("01_hospitalized_omicron", "01_recovered_omicron") )$weight <- CONST_rho_omicron * (1 - CONST_f_h_omicron)
    E(disease_model, path = c("01_hospitalized_omicron", "01_recovered_omicron") )$label <- paste0("(7 → r) = ", CONST_rho_omicron * (1 - CONST_f_h_omicron))

    E(disease_model, path = c("01_hospitalized_omicron", "01_dead_omicron") )$weight <- CONST_rho_omicron * CONST_f_h_omicron
    E(disease_model, path = c("01_hospitalized_omicron", "01_dead_omicron") )$label <- paste0("(7 → d) = ", CONST_rho_omicron * CONST_f_h_omicron)


    # From hospitalized with ICU usage
    E(disease_model, path = c("01_hospitalized_ICU_omicron", "01_recovered_omicron") )$weight <- CONST_sigma_omicron * (1 - CONST_f_c_omicron)
    E(disease_model, path = c("01_hospitalized_ICU_omicron", "01_recovered_omicron") )$label <- paste0("(8 → r) = ", CONST_sigma_omicron * (1 - CONST_f_c_omicron))

    E(disease_model, path = c("01_hospitalized_ICU_omicron", "01_dead_omicron") )$weight <- CONST_sigma_omicron * CONST_f_c_omicron
    E(disease_model, path = c("01_hospitalized_ICU_omicron", "01_dead_omicron") )$label <- paste0("(8 → d) = ", CONST_sigma_omicron * CONST_f_c_omicron)


    # For all the states of Omicron infection with 1 dose of vaccine
    # Connect to main transition graph
    disease_model <- disease_model + edge("received_dose1", "01_latent_infections_isolated_omicron",
                                          weight = "CALCULATE_FROM_CONTACT_MATRIX",
                                          label = paste0("if infected, then q = ", CONST_q_omicron))

    disease_model <- disease_model + edge("received_dose1", "01_latent_infections_not_isolated_omicron",
                                          weight = "CALCULATE_FROM_CONTACT_MATRIX",
                                          label = paste0("if infected, then (1 - q) = ", (1 - CONST_q_omicron)))

  }

  # Infection parameters for people with 2 dose of vaccination w.r.t Omicron
  {
    disease_model <- disease_model+
      graph( edges = c("02_latent_infections_not_isolated_omicron", "02_pre_symptomatic_non_isolated_omicron", # π(1 → 3)
                       "02_latent_infections_isolated_omicron", "02_pre_symptomatic_isolated_omicron", # π(2 → 4)

                       "02_pre_symptomatic_non_isolated_omicron", "02_mild_non_isolated_omicron", # π(3 → 5)
                       "02_pre_symptomatic_non_isolated_omicron", "02_mild_isolated_omicron", # π(3 → 6)
                       "02_pre_symptomatic_non_isolated_omicron", "02_hospitalized_omicron", # π(3 → 7)
                       "02_pre_symptomatic_non_isolated_omicron", "02_hospitalized_ICU_omicron", # π(3 → 8)

                       "02_pre_symptomatic_isolated_omicron", "02_mild_isolated_omicron", # π(4 → 6)
                       "02_pre_symptomatic_isolated_omicron", "02_hospitalized_omicron", # π(4 → 7)
                       "02_pre_symptomatic_isolated_omicron", "02_hospitalized_ICU_omicron", # π(4 → 8)

                       "02_mild_isolated_omicron", "02_recovered_omicron", # π(5 → r)
                       "02_mild_non_isolated_omicron", "02_recovered_omicron", # π(6 → r)
                       "02_hospitalized_omicron", "02_recovered_omicron", # π(7 → r)
                       "02_hospitalized_ICU_omicron", "02_recovered_omicron", # π(8 → r)

                       "02_hospitalized_omicron", "02_dead_omicron", # π(7 → d)
                       "02_hospitalized_ICU_omicron", "02_dead_omicron"), # π(8 → d)
             directed = TRUE)

    # From Latent
    E(disease_model, path = c("02_latent_infections_not_isolated_omicron", "02_pre_symptomatic_non_isolated_omicron") )$weight <- CONST_CHI_omicron
    E(disease_model, path = c("02_latent_infections_not_isolated_omicron", "02_pre_symptomatic_non_isolated_omicron") )$label <- paste0("(1 → 3) = ", CONST_CHI_omicron)

    E(disease_model, path = c("02_latent_infections_isolated_omicron", "02_pre_symptomatic_isolated_omicron") )$weight <- CONST_CHI_omicron
    E(disease_model, path = c("02_latent_infections_isolated_omicron", "02_pre_symptomatic_isolated_omicron") )$label <- paste0("(2 → 4) = ", CONST_CHI_omicron)


    # From Pre-symptomatic
    E(disease_model, path = c("02_pre_symptomatic_non_isolated_omicron", "02_mild_non_isolated_omicron") )$weight <- ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_02 * (1 - CONST_nu_m_omicron)  )* (1 - CONST_p_omicron) * CONST_KAPPA_omicron
    E(disease_model, path = c("02_pre_symptomatic_non_isolated_omicron", "02_mild_non_isolated_omicron") )$label  <- paste0("(3 → 5) = ", ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_02 * (1 - CONST_nu_m_omicron)  )* (1 - CONST_p_omicron) * CONST_KAPPA_omicron)

    E(disease_model, path = c("02_pre_symptomatic_non_isolated_omicron", "02_mild_isolated_omicron") )$weight <- ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_02 * (1 - CONST_nu_m_omicron)  ) * CONST_p_omicron * CONST_KAPPA_omicron
    E(disease_model, path = c("02_pre_symptomatic_non_isolated_omicron", "02_mild_isolated_omicron") )$label <- paste0("(3 → 6) = ", ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_02 * (1 - CONST_nu_m_omicron)  ) * CONST_p_omicron * CONST_KAPPA_omicron)

    E(disease_model, path = c("02_pre_symptomatic_non_isolated_omicron", "02_hospitalized_omicron") )$weight <- CONST_nu_h_omicron * (1 - CONST_epsilon_serious_omicron_02) * CONST_KAPPA_omicron
    E(disease_model, path = c("02_pre_symptomatic_non_isolated_omicron", "02_hospitalized_omicron") )$label <- paste0("(3 → 7) = ", CONST_nu_h_omicron * (1 - CONST_epsilon_serious_omicron_02)  * CONST_KAPPA_omicron)

    E(disease_model, path = c("02_pre_symptomatic_non_isolated_omicron", "02_hospitalized_ICU_omicron") )$weight <- CONST_nu_c * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
    E(disease_model, path = c("02_pre_symptomatic_non_isolated_omicron", "02_hospitalized_ICU_omicron") )$label <- paste0("(3 → 8) = ", CONST_nu_c_omicron  * (1 - CONST_epsilon_serious_omicron_02) * CONST_KAPPA_omicron)



    E(disease_model, path = c("02_pre_symptomatic_isolated_omicron", "02_mild_isolated_omicron") )$weight <- ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_02 * (1 - CONST_nu_m_omicron)  ) * CONST_KAPPA_omicron
    E(disease_model, path = c("02_pre_symptomatic_isolated_omicron", "02_mild_isolated_omicron") )$label <- paste0("(4 → 6) = ", ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_02 * (1 - CONST_nu_m_omicron)  ) * CONST_KAPPA_omicron)

    E(disease_model, path = c("02_pre_symptomatic_isolated_omicron", "02_hospitalized_omicron") )$weight <- CONST_nu_h_omicron * (1 - CONST_epsilon_serious_omicron_02) * CONST_KAPPA_omicron
    E(disease_model, path = c("02_pre_symptomatic_isolated_omicron", "02_hospitalized_omicron") )$label <- paste0("(4 → 7) = ", CONST_nu_h_omicron  * (1 - CONST_epsilon_serious_omicron_02) * CONST_KAPPA_omicron)

    E(disease_model, path = c("02_pre_symptomatic_isolated_omicron", "02_hospitalized_ICU_omicron") )$weight <- CONST_nu_c_omicron * (1 - CONST_epsilon_serious_omicron_02) * CONST_KAPPA_omicron
    E(disease_model, path = c("02_pre_symptomatic_isolated_omicron", "02_hospitalized_ICU_omicron") )$label <- paste0("(4 → 8) = ", CONST_nu_c_omicron * (1 - CONST_epsilon_serious_omicron_02) * CONST_KAPPA_omicron)




    # From mild
    E(disease_model, path = c("02_mild_isolated_omicron", "02_recovered_omicron") )$weight <- CONST_eta_omicron
    E(disease_model, path = c("02_mild_isolated_omicron", "02_recovered_omicron") )$label <- paste0("(6 → r) = ", CONST_eta_omicron)

    E(disease_model, path = c("02_mild_non_isolated_omicron", "02_recovered_omicron") )$weight <- CONST_eta_omicron
    E(disease_model, path = c("02_mild_non_isolated_omicron", "02_recovered_omicron") )$label <- paste0("(5 → r) = ", CONST_eta_omicron)



    # From hospitalized
    E(disease_model, path = c("02_hospitalized_omicron", "02_recovered_omicron") )$weight <- CONST_rho_omicron * (1 - CONST_f_h_omicron)
    E(disease_model, path = c("02_hospitalized_omicron", "02_recovered_omicron") )$label <- paste0("(7 → r) = ", CONST_rho_omicron * (1 - CONST_f_h_omicron))

    E(disease_model, path = c("02_hospitalized_omicron", "02_dead_omicron") )$weight <- CONST_rho_omicron * CONST_f_h_omicron
    E(disease_model, path = c("02_hospitalized_omicron", "02_dead_omicron") )$label <- paste0("(7 → d) = ", CONST_rho_omicron * CONST_f_h_omicron)


    # From hospitalized with ICU usage
    E(disease_model, path = c("02_hospitalized_ICU_omicron", "02_recovered_omicron") )$weight <- CONST_sigma_omicron * (1 - CONST_f_c_omicron)
    E(disease_model, path = c("02_hospitalized_ICU_omicron", "02_recovered_omicron") )$label <- paste0("(8 → r) = ", CONST_sigma_omicron * (1 - CONST_f_c_omicron))

    E(disease_model, path = c("02_hospitalized_ICU_omicron", "02_dead_omicron") )$weight <- CONST_sigma_omicron * CONST_f_c_omicron
    E(disease_model, path = c("02_hospitalized_ICU_omicron", "02_dead_omicron") )$label <- paste0("(8 → d) = ", CONST_sigma_omicron * CONST_f_c_omicron)


    # For all the states of Omicron infection with 2 doses of vaccine
    # Connect to main transition graph
    disease_model <- disease_model + edge("received_dose2", "02_latent_infections_isolated_omicron",
                                          weight = "CALCULATE_FROM_CONTACT_MATRIX",
                                          label = paste0("if infected, then q = ", CONST_q_omicron))

    disease_model <- disease_model + edge("received_dose2", "02_latent_infections_not_isolated_omicron",
                                          weight = "CALCULATE_FROM_CONTACT_MATRIX",
                                          label = paste0("if infected, then (1 - q) = ", (1 - CONST_q_omicron)))

  }


  # Infection parameters for people with 3 dose of vaccination w.r.t Omicron
  {
    disease_model <- disease_model+
      graph( edges = c("03_latent_infections_not_isolated_omicron", "03_pre_symptomatic_non_isolated_omicron", # π(1 → 3)
                       "03_latent_infections_isolated_omicron", "03_pre_symptomatic_isolated_omicron", # π(2 → 4)

                       "03_pre_symptomatic_non_isolated_omicron", "03_mild_non_isolated_omicron", # π(3 → 5)
                       "03_pre_symptomatic_non_isolated_omicron", "03_mild_isolated_omicron", # π(3 → 6)
                       "03_pre_symptomatic_non_isolated_omicron", "03_hospitalized_omicron", # π(3 → 7)
                       "03_pre_symptomatic_non_isolated_omicron", "03_hospitalized_ICU_omicron", # π(3 → 8)

                       "03_pre_symptomatic_isolated_omicron", "03_mild_isolated_omicron", # π(4 → 6)
                       "03_pre_symptomatic_isolated_omicron", "03_hospitalized_omicron", # π(4 → 7)
                       "03_pre_symptomatic_isolated_omicron", "03_hospitalized_ICU_omicron", # π(4 → 8)

                       "03_mild_isolated_omicron", "03_recovered_omicron", # π(5 → r)
                       "03_mild_non_isolated_omicron", "03_recovered_omicron", # π(6 → r)
                       "03_hospitalized_omicron", "03_recovered_omicron", # π(7 → r)
                       "03_hospitalized_ICU_omicron", "03_recovered_omicron", # π(8 → r)

                       "03_hospitalized_omicron", "03_dead_omicron", # π(7 → d)
                       "03_hospitalized_ICU_omicron", "03_dead_omicron"), # π(8 → d)
             directed = TRUE)

    # From Latent
    E(disease_model, path = c("03_latent_infections_not_isolated_omicron", "03_pre_symptomatic_non_isolated_omicron") )$weight <- CONST_CHI_omicron
    E(disease_model, path = c("03_latent_infections_not_isolated_omicron", "03_pre_symptomatic_non_isolated_omicron") )$label <- paste0("(1 → 3) = ", CONST_CHI_omicron)

    E(disease_model, path = c("03_latent_infections_isolated_omicron", "03_pre_symptomatic_isolated_omicron") )$weight <- CONST_CHI_omicron
    E(disease_model, path = c("03_latent_infections_isolated_omicron", "03_pre_symptomatic_isolated_omicron") )$label <- paste0("(2 → 4) = ", CONST_CHI_omicron)


    # From Pre-symptomatic
    E(disease_model, path = c("03_pre_symptomatic_non_isolated_omicron", "03_mild_non_isolated_omicron") )$weight <- ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_03 * (1 - CONST_nu_m_omicron)  )* (1 - CONST_p_omicron) * CONST_KAPPA_omicron
    E(disease_model, path = c("03_pre_symptomatic_non_isolated_omicron", "03_mild_non_isolated_omicron") )$label  <- paste0("(3 → 5) = ", ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_03 * (1 - CONST_nu_m_omicron)  )* (1 - CONST_p_omicron) * CONST_KAPPA_omicron)

    E(disease_model, path = c("03_pre_symptomatic_non_isolated_omicron", "03_mild_isolated_omicron") )$weight <- ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_03 * (1 - CONST_nu_m_omicron)  ) * CONST_p_omicron * CONST_KAPPA_omicron
    E(disease_model, path = c("03_pre_symptomatic_non_isolated_omicron", "03_mild_isolated_omicron") )$label <- paste0("(3 → 6) = ", ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_03 * (1 - CONST_nu_m_omicron)  ) * CONST_p_omicron * CONST_KAPPA_omicron)

    E(disease_model, path = c("03_pre_symptomatic_non_isolated_omicron", "03_hospitalized_omicron") )$weight <- CONST_nu_h_omicron * (1 - CONST_epsilon_serious_omicron_03) * CONST_KAPPA_omicron
    E(disease_model, path = c("03_pre_symptomatic_non_isolated_omicron", "03_hospitalized_omicron") )$label <- paste0("(3 → 7) = ", CONST_nu_h_omicron * (1 - CONST_epsilon_serious_omicron_03)  * CONST_KAPPA_omicron)

    E(disease_model, path = c("03_pre_symptomatic_non_isolated_omicron", "03_hospitalized_ICU_omicron") )$weight <- CONST_nu_c * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
    E(disease_model, path = c("03_pre_symptomatic_non_isolated_omicron", "03_hospitalized_ICU_omicron") )$label <- paste0("(3 → 8) = ", CONST_nu_c_omicron  * (1 - CONST_epsilon_serious_omicron_03) * CONST_KAPPA_omicron)



    E(disease_model, path = c("03_pre_symptomatic_isolated_omicron", "03_mild_isolated_omicron") )$weight <- ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_03 * (1 - CONST_nu_m_omicron)  ) * CONST_KAPPA_omicron
    E(disease_model, path = c("03_pre_symptomatic_isolated_omicron", "03_mild_isolated_omicron") )$label <- paste0("(4 → 6) = ", ( CONST_nu_m_omicron + CONST_epsilon_serious_omicron_03 * (1 - CONST_nu_m_omicron)  ) * CONST_KAPPA_omicron)

    E(disease_model, path = c("03_pre_symptomatic_isolated_omicron", "03_hospitalized_omicron") )$weight <- CONST_nu_h_omicron * (1 - CONST_epsilon_serious_omicron_03) * CONST_KAPPA_omicron
    E(disease_model, path = c("03_pre_symptomatic_isolated_omicron", "03_hospitalized_omicron") )$label <- paste0("(4 → 7) = ", CONST_nu_h_omicron  * (1 - CONST_epsilon_serious_omicron_03) * CONST_KAPPA_omicron)

    E(disease_model, path = c("03_pre_symptomatic_isolated_omicron", "03_hospitalized_ICU_omicron") )$weight <- CONST_nu_c_omicron * (1 - CONST_epsilon_serious_omicron_03) * CONST_KAPPA_omicron
    E(disease_model, path = c("03_pre_symptomatic_isolated_omicron", "03_hospitalized_ICU_omicron") )$label <- paste0("(4 → 8) = ", CONST_nu_c_omicron * (1 - CONST_epsilon_serious_omicron_03) * CONST_KAPPA_omicron)




    # From mild
    E(disease_model, path = c("03_mild_isolated_omicron", "03_recovered_omicron") )$weight <- CONST_eta_omicron
    E(disease_model, path = c("03_mild_isolated_omicron", "03_recovered_omicron") )$label <- paste0("(6 → r) = ", CONST_eta_omicron)

    E(disease_model, path = c("03_mild_non_isolated_omicron", "03_recovered_omicron") )$weight <- CONST_eta_omicron
    E(disease_model, path = c("03_mild_non_isolated_omicron", "03_recovered_omicron") )$label <- paste0("(5 → r) = ", CONST_eta_omicron)



    # From hospitalized
    E(disease_model, path = c("03_hospitalized_omicron", "03_recovered_omicron") )$weight <- CONST_rho_omicron * (1 - CONST_f_h_omicron)
    E(disease_model, path = c("03_hospitalized_omicron", "03_recovered_omicron") )$label <- paste0("(7 → r) = ", CONST_rho_omicron * (1 - CONST_f_h_omicron))

    E(disease_model, path = c("03_hospitalized_omicron", "03_dead_omicron") )$weight <- CONST_rho_omicron * CONST_f_h_omicron
    E(disease_model, path = c("03_hospitalized_omicron", "03_dead_omicron") )$label <- paste0("(7 → d) = ", CONST_rho_omicron * CONST_f_h_omicron)


    # From hospitalized with ICU usage
    E(disease_model, path = c("03_hospitalized_ICU_omicron", "03_recovered_omicron") )$weight <- CONST_sigma_omicron * (1 - CONST_f_c_omicron)
    E(disease_model, path = c("03_hospitalized_ICU_omicron", "03_recovered_omicron") )$label <- paste0("(8 → r) = ", CONST_sigma_omicron * (1 - CONST_f_c_omicron))

    E(disease_model, path = c("03_hospitalized_ICU_omicron", "03_dead_omicron") )$weight <- CONST_sigma_omicron * CONST_f_c_omicron
    E(disease_model, path = c("03_hospitalized_ICU_omicron", "03_dead_omicron") )$label <- paste0("(8 → d) = ", CONST_sigma_omicron * CONST_f_c_omicron)


    # For all the states of Omicron infection with 2 doses of vaccine
    # Connect to main transition graph
    disease_model <- disease_model + edge("received_dose3", "03_latent_infections_isolated_omicron",
                                          weight = "CALCULATE_FROM_CONTACT_MATRIX",
                                          label = paste0("if infected, then q = ", CONST_q_omicron))

    disease_model <- disease_model + edge("received_dose3", "03_latent_infections_not_isolated_omicron",
                                          weight = "CALCULATE_FROM_CONTACT_MATRIX",
                                          label = paste0("if infected, then (1 - q) = ", (1 - CONST_q_omicron)))

  }

  # Generate transition matrix
  transition_matrix <- as_adjacency_matrix(disease_model, attr="weight", sparse = FALSE)

  STATE_NAMES = colnames(transition_matrix)

  # Book keeping

  # Calibration mode:
  STATELIST_SUSCEPTIBLE <- which(colnames(transition_matrix) == "susceptible" |
                                   colnames(transition_matrix) == "received_dose1" |
                                   colnames(transition_matrix) == "received_dose2" |
                                   colnames(transition_matrix) == "received_dose3")

  STATELIST_INFECTIOUS <- which(colnames(transition_matrix) == "00_pre_symptomatic_non_isolated" |
                                  colnames(transition_matrix) == "00_mild_non_isolated" |
                                  colnames(transition_matrix) == "01_pre_symptomatic_non_isolated" |
                                  colnames(transition_matrix) == "01_mild_non_isolated" |
                                  colnames(transition_matrix) == "02_pre_symptomatic_non_isolated" |
                                  colnames(transition_matrix) == "02_mild_non_isolated" |
                                  colnames(transition_matrix) == "03_pre_symptomatic_non_isolated" |
                                  colnames(transition_matrix) == "03_mild_non_isolated" )

  # Separated to setup proportions of Omicron and Non-omicron infection events
  STATELIST_INFECTIOUS_OMICRON <- which(colnames(transition_matrix) == "00_pre_symptomatic_non_isolated_omicron" |
                                          colnames(transition_matrix) == "00_mild_non_isolated_omicron" |
                                          colnames(transition_matrix) == "01_pre_symptomatic_non_isolated_omicron" |
                                          colnames(transition_matrix) == "01_mild_non_isolated_omicron" |
                                          colnames(transition_matrix) == "02_pre_symptomatic_non_isolated_omicron" |
                                          colnames(transition_matrix) == "02_mild_non_isolated_omicron" |
                                          colnames(transition_matrix) == "03_pre_symptomatic_non_isolated_omicron" |
                                          colnames(transition_matrix) == "03_mild_non_isolated_omicron")
  # New observed cases
  STATELIST_NEW_CASES <- which(colnames(transition_matrix) == "00_pre_symptomatic_isolated" |
                                 colnames(transition_matrix) == "00_mild_isolated" |
                                 colnames(transition_matrix) == "01_pre_symptomatic_isolated" |
                                 colnames(transition_matrix) == "01_mild_isolated" |
                                 colnames(transition_matrix) == "02_pre_symptomatic_isolated" |
                                 colnames(transition_matrix) == "02_mild_isolated" |
                                 colnames(transition_matrix) == "03_pre_symptomatic_isolated" |
                                 colnames(transition_matrix) == "03_mild_isolated" |

                                 colnames(transition_matrix) == "00_pre_symptomatic_isolated_omicron" |
                                 colnames(transition_matrix) == "00_mild_isolated_omicron" |
                                 colnames(transition_matrix) == "01_pre_symptomatic_isolated_omicron" |
                                 colnames(transition_matrix) == "01_mild_isolated_omicron" |
                                 colnames(transition_matrix) == "02_pre_symptomatic_isolated_omicron" |
                                 colnames(transition_matrix) == "02_mild_isolated_omicron" |
                                 colnames(transition_matrix) == "03_pre_symptomatic_isolated_omicron" |
                                 colnames(transition_matrix) == "03_mild_isolated_omicron" )


  # Observed active cases
  STATELIST_ACTIVE_CASES <- which(colnames(transition_matrix) == "00_pre_symptomatic_isolated" |
                                    colnames(transition_matrix) == "00_mild_isolated" |
                                    colnames(transition_matrix) == "00_hospitalized" |
                                    colnames(transition_matrix) == "00_hospitalized_ICU" |
                                    colnames(transition_matrix) == "01_pre_symptomatic_isolated" |
                                    colnames(transition_matrix) == "01_mild_isolated" |
                                    colnames(transition_matrix) == "01_hospitalized" |
                                    colnames(transition_matrix) == "01_hospitalized_ICU" |
                                    colnames(transition_matrix) == "02_pre_symptomatic_isolated" |
                                    colnames(transition_matrix) == "02_mild_isolated" |
                                    colnames(transition_matrix) == "02_hospitalized" |
                                    colnames(transition_matrix) == "02_hospitalized_ICU" |
                                    colnames(transition_matrix) == "03_pre_symptomatic_isolated" |
                                    colnames(transition_matrix) == "03_mild_isolated" |
                                    colnames(transition_matrix) == "03_hospitalized" |
                                    colnames(transition_matrix) == "03_hospitalized_ICU" |

                                    colnames(transition_matrix) == "00_pre_symptomatic_isolated_omicron" |
                                    colnames(transition_matrix) == "00_mild_isolated_omicron" |
                                    colnames(transition_matrix) == "00_hospitalized_omicron" |
                                    colnames(transition_matrix) == "00_hospitalized_ICU_omicron" |
                                    colnames(transition_matrix) == "01_pre_symptomatic_isolated_omicron" |
                                    colnames(transition_matrix) == "01_mild_isolated_omicron" |
                                    colnames(transition_matrix) == "01_hospitalized_omicron" |
                                    colnames(transition_matrix) == "01_hospitalized_ICU_omicron" |
                                    colnames(transition_matrix) == "02_pre_symptomatic_isolated_omicron" |
                                    colnames(transition_matrix) == "02_mild_isolated_omicron" |
                                    colnames(transition_matrix) == "02_hospitalized_omicron" |
                                    colnames(transition_matrix) == "02_hospitalized_ICU_omicron" |
                                    colnames(transition_matrix) == "03_pre_symptomatic_isolated_omicron" |
                                    colnames(transition_matrix) == "03_mild_isolated_omicron" |
                                    colnames(transition_matrix) == "03_hospitalized_omicron" |
                                    colnames(transition_matrix) == "03_hospitalized_ICU_omicron")



  # Terminal states (non-transitioning & non-probabilistic):
  # Note: May be programatically deduced but just manually plugging it in now
  STATELIST_TERMINAL = which(colnames(transition_matrix) == "00_recovered" |
                               colnames(transition_matrix) == "00_dead" |
                               colnames(transition_matrix) == "01_recovered" |
                               colnames(transition_matrix) == "01_dead" |
                               colnames(transition_matrix) == "02_recovered" |
                               colnames(transition_matrix) == "02_dead" |
                               colnames(transition_matrix) == "03_recovered" |
                               colnames(transition_matrix) == "03_dead" |

                               colnames(transition_matrix) == "00_recovered_omicron" |
                               colnames(transition_matrix) == "00_dead_omicron" |
                               colnames(transition_matrix) == "01_recovered_omicron" |
                               colnames(transition_matrix) == "01_dead_omicron" |
                               colnames(transition_matrix) == "02_recovered_omicron" |
                               colnames(transition_matrix) == "02_dead_omicron" |
                               colnames(transition_matrix) == "03_recovered_omicron" |
                               colnames(transition_matrix) == "03_dead_omicron")

  # End construction of complex disease model
  # ----------------------------

} else if(CONST_CHOSEN_DISEASE_MODEL == "SIR"){
  # Begin construction of SIR disease model
  # ----------------------------

  cat("\nConstructing disease model: ", CONST_CHOSEN_DISEASE_MODEL, sep = "")

  # Infection states for 0 doses:
  disease_model <- graph( edges = c("susceptible","infected",
                                    "infected", "removed"), directed = TRUE)

  # For people who are not vaccinated: Infection transition/day parameters
  CONSTANT_REMOVAL_RATE <- 0.1 # gamma
  # CONSTANT_REMOVAL_RATE <- 1/9 # gamma, maybe this offsets for evolving first and checking for infection next? Was useful in calibration

  E(disease_model, path = c("susceptible","infected") )$weight <- "CALCULATE_FROM_CONTACT_MATRIX"
  E(disease_model, path = c("susceptible","infected") )$label <- paste0("CALCULATE_FROM_CONTACT_MATRIX")

  E(disease_model, path = c("infected", "removed") )$weight <- CONSTANT_REMOVAL_RATE
  E(disease_model, path = c("infected", "removed") )$label <- paste0(CONSTANT_REMOVAL_RATE)




  # Generate transition matrix
  transition_matrix <- as_adjacency_matrix(disease_model, attr="weight", sparse = FALSE)

  STATE_NAMES = colnames(transition_matrix)

  # Book keeping

  # Calibration mode:
  STATELIST_SUSCEPTIBLE <- which(colnames(transition_matrix) == "susceptible")

  STATELIST_INFECTIOUS <- which(colnames(transition_matrix) == "infected")

  # Separated to setup proportions of Omicron and Non-omicron infection events
  STATELIST_INFECTIOUS_OMICRON <- c()

  STATELIST_NEW_CASES <- which(colnames(transition_matrix) == "infected")

  STATELIST_ACTIVE_CASES <- which(colnames(transition_matrix) == "infected")

  STATELIST_TERMINAL <- which(colnames(transition_matrix) == "removed")

} else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){
  # Tag: @calib10
  # Begin construction of SIR disease model with extra compartments for Testing, Isolation
  # ----------------------------

  cat("\nConstructing disease model: ", CONST_CHOSEN_DISEASE_MODEL, sep = "")
  cat("\n '-CONST_CHOSEN_TEST_ALLOCATION_STRATEGY: ", CONST_CHOSEN_TEST_ALLOCATION_STRATEGY, sep = "")

  # Infection states for 0 doses:
  disease_model <- graph( edges = c("susceptible", "(unconfirmed) infected",
                                    "susceptible", "(isolated) susceptible",

                                    "(isolated) susceptible", "susceptible",
                                    "(isolated) susceptible", "(isolated + false positive) susceptible",

                                    "(isolated + false positive) susceptible", "susceptible",

                                    "(unconfirmed) infected", "(isolated + unconfirmed) infected",
                                    "(unconfirmed) infected", "removed",

                                    "(isolated + unconfirmed) infected", "(isolated + confirmed) infected",
                                    "(isolated + unconfirmed) infected", "(unconfirmed) infected",
                                    "(isolated + unconfirmed) infected", "removed",

                                    "(isolated + confirmed) infected", "removed"), directed = TRUE)

  # For people who are not vaccinated: Infection transition/day parameters
  CONSTANT_REMOVAL_RATE <- 0.1 # gamma
  CONSTANT_FALSE_POSITIVE_ISOLATION_PERIOD <- 1/12 # 1/5 # infectious period (10 days) + 2 days

  # Graph visualization parameters
  CONST_CURVE_LEVEL_1 = 0.3

  # beta derived from contact matrix
  E(disease_model, path = c("susceptible", "(unconfirmed) infected") )$weight <- "CALCULATE_FROM_CONTACT_MATRIX"
  E(disease_model, path = c("susceptible", "(unconfirmed) infected") )$label <- paste0("CALCULATE_FROM_CONTACT_MATRIX")
  E(disease_model, path = c("susceptible", "(unconfirmed) infected") )$curved <- 0.0

  # Susceptible requesting tests, voluntarily or via tracing, deterministic state change with constant time
  E(disease_model, path = c("susceptible", "(isolated) susceptible") )$weight <- 0.0
  E(disease_model, path = c("susceptible", "(isolated) susceptible") )$label <- paste0("Request for test")
  E(disease_model, path = c("susceptible", "(isolated) susceptible") )$curved <- CONST_CURVE_LEVEL_1

  # Isolated susceptible receiving negative test result and joining back the susceptible population, stochastic state change with constant time
  E(disease_model, path = c("(isolated) susceptible", "susceptible") )$weight <- 0.0
  E(disease_model, path = c("(isolated) susceptible", "susceptible") )$label <- paste0("Test Result (TNR)")
  E(disease_model, path = c("(isolated) susceptible", "susceptible") )$curved <- CONST_CURVE_LEVEL_1

  # Isolated susceptible receiving positive test result, moving to a separate compartment for book-keeping of "observed" active cases and "Test positivity rate"
  E(disease_model, path = c("(isolated) susceptible", "(isolated + false positive) susceptible") )$weight <- 0.0
  E(disease_model, path = c("(isolated) susceptible", "(isolated + false positive) susceptible") )$label <- paste0("Test Result (FPR)")
  E(disease_model, path = c("(isolated) susceptible", "(isolated + false positive) susceptible") )$curved <- 0.0

  # Isolated susceptible who received (false) positive test result, moving back to susceptible post a predetermined isolation time
  E(disease_model, path = c("(isolated + false positive) susceptible", "susceptible") )$weight <- CONSTANT_FALSE_POSITIVE_ISOLATION_PERIOD
  E(disease_model, path = c("(isolated + false positive) susceptible", "susceptible") )$label <- paste0(CONSTANT_FALSE_POSITIVE_ISOLATION_PERIOD)
  E(disease_model, path = c("(isolated + false positive) susceptible", "susceptible") )$curved <- 3 * CONST_CURVE_LEVEL_1


  # Unconfirmed infectious (for SIR model) requesting tests, voluntarily or via tracing, deterministic state change with constant time
  E(disease_model, path = c("(unconfirmed) infected", "(isolated + unconfirmed) infected") )$weight <- 0.0
  E(disease_model, path = c("(unconfirmed) infected", "(isolated + unconfirmed) infected") )$label <- paste0("Request for test")
  E(disease_model, path = c("(unconfirmed) infected", "(isolated + unconfirmed) infected") )$curved <- CONST_CURVE_LEVEL_1

  # Unconfirmed infectious moving to removed, stochastic gamma
  E(disease_model, path = c("(unconfirmed) infected", "removed") )$weight <- CONSTANT_REMOVAL_RATE
  E(disease_model, path = c("(unconfirmed) infected", "removed") )$label <- paste0(CONSTANT_REMOVAL_RATE)
  E(disease_model, path = c("(unconfirmed) infected", "removed") )$curved <- 0.0


  # Unconfirmed and Isolated infectious receiving positive test result and joining confirmed infectious population, stochastic state change with constant time
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(isolated + confirmed) infected") )$weight <- 0.0
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(isolated + confirmed) infected") )$label <- paste0("Test Result (TPR)")
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(isolated + confirmed) infected") )$curved <- 0.0

  # Unconfirmed and Isolated infectious receiving negative test result and moving back to unconfirmed infectious population, stochastic state change with constant time
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(unconfirmed) infected") )$weight <- 0.0
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(unconfirmed) infected") )$label <- paste0("Test Result (FNR)")
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(unconfirmed) infected") )$curved <- CONST_CURVE_LEVEL_1


  # Unconfirmed and Isolated infectious moving to removed, stochastic gamma
  E(disease_model, path = c("(isolated + unconfirmed) infected", "removed") )$weight <- CONSTANT_REMOVAL_RATE
  E(disease_model, path = c("(isolated + unconfirmed) infected", "removed") )$label <- paste0(CONSTANT_REMOVAL_RATE)
  E(disease_model, path = c("(isolated + unconfirmed) infected", "removed") )$curved <- 0.0


  # Confirmed and Isolated infectious moving to removed, stochastic gamma
  E(disease_model, path = c("(isolated + confirmed) infected", "removed") )$weight <- CONSTANT_REMOVAL_RATE
  E(disease_model, path = c("(isolated + confirmed) infected", "removed") )$label <- paste0(CONSTANT_REMOVAL_RATE)
  E(disease_model, path = c("(isolated + confirmed) infected", "removed") )$curved <- 0.0




  # Generate transition matrix
  transition_matrix <- as_adjacency_matrix(disease_model, attr="weight", sparse = FALSE)

  STATE_NAMES = colnames(transition_matrix)

  # Book keeping

  # Calibration mode:
  STATELIST_SUSCEPTIBLE <- which(colnames(transition_matrix) == "susceptible") # isolated agents do not participate in receiving infection

  STATELIST_INFECTIOUS <- which(colnames(transition_matrix) == "(unconfirmed) infected" ) # only participating infectious state in propagating infection

  # Separated to setup proportions of Omicron and Non-omicron infection events
  STATELIST_INFECTIOUS_OMICRON <- c()

  STATELIST_NEW_CASES <- which(colnames(transition_matrix) == "(isolated + confirmed) infected" |
                                 colnames(transition_matrix) == "(isolated + false positive) susceptible")

  STATELIST_ACTIVE_CASES <- which(colnames(transition_matrix) == "(isolated + confirmed) infected" |
                                    colnames(transition_matrix) == "(isolated + false positive) susceptible")

  STATELIST_TERMINAL <- which(colnames(transition_matrix) == "removed")


  # For testing
  # Infectious and non-infected voluntary test seekers
  STATELIST_SYMPTOMATIC_INFECTIOUS_TEST_SEEKER <- which(colnames(transition_matrix) == "(unconfirmed) infected" )
  STATELIST_ASYMPTOMATIC_INFECTIOUS_TEST_SEEKER <- c()
  STATELIST_NOT_INFECTED_TEST_SEEKER <- which(colnames(transition_matrix) == "susceptible" )

  # Infectious and non-infected result seekers
  STATELIST_SYMPTOMATIC_INFECTIOUS_RESULT_SEEKER <- which(colnames(transition_matrix) == "(isolated + unconfirmed) infected" )
  STATELIST_ASYMPTOMATIC_INFECTIOUS_RESULT_SEEKER <- c()
  STATELIST_NOT_INFECTED_RESULT_SEEKER <- which(colnames(transition_matrix) == "(isolated) susceptible" )

} else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){
  # Tag: @calib11
  # Begin construction of SIR disease model with extra compartments for Testing, Isolation and Contact tracing
  # ----------------------------

  cat("\nConstructing disease model: ", CONST_CHOSEN_DISEASE_MODEL, sep = "")
  cat("\n '-CONST_CHOSEN_TEST_ALLOCATION_STRATEGY: ", CONST_CHOSEN_TEST_ALLOCATION_STRATEGY, sep = "")

  # Infection states for 0 doses:
  disease_model <- graph( edges = c("susceptible", "(unconfirmed) infected",
                                    "susceptible", "(isolated) susceptible",

                                    "(isolated) susceptible", "susceptible",
                                    "(isolated) susceptible", "(isolated + false positive) susceptible",

                                    "(isolated + false positive) susceptible", "susceptible",

                                    "(unconfirmed) infected", "(isolated + unconfirmed) infected",
                                    "(unconfirmed) infected", "removed",

                                    "(isolated + unconfirmed) infected", "(isolated + confirmed) infected",
                                    "(isolated + unconfirmed) infected", "(unconfirmed) infected",
                                    "(isolated + unconfirmed) infected", "removed",

                                    "(isolated + confirmed) infected", "removed",

                                    "removed", "(isolated) removed",

                                    "(isolated) removed", "removed",
                                    "(isolated) removed", "(isolated + false positive) removed",

                                    "(isolated + false positive) removed", "removed"), directed = TRUE)

  # For people who are not vaccinated: Infection transition/day parameters
  CONSTANT_REMOVAL_RATE <- 0.1 # gamma
  CONSTANT_FALSE_POSITIVE_ISOLATION_PERIOD <- 1/12 # infectious period (10 days) + 2 days

  # Graph visualization parameters
  CONST_CURVE_LEVEL_1 = 0.3

  # beta derived from contact matrix
  E(disease_model, path = c("susceptible", "(unconfirmed) infected") )$weight <- "CALCULATE_FROM_CONTACT_MATRIX"
  E(disease_model, path = c("susceptible", "(unconfirmed) infected") )$label <- paste0("CALCULATE_FROM_CONTACT_MATRIX")
  E(disease_model, path = c("susceptible", "(unconfirmed) infected") )$curved <- 0.0

  # Susceptible requesting tests, voluntarily or via tracing, deterministic state change with constant time
  E(disease_model, path = c("susceptible", "(isolated) susceptible") )$weight <- 0.0
  E(disease_model, path = c("susceptible", "(isolated) susceptible") )$label <- paste0("Request for test")
  E(disease_model, path = c("susceptible", "(isolated) susceptible") )$curved <- CONST_CURVE_LEVEL_1

  # Isolated susceptible receiving negative test result and joining back the susceptible population, stochastic state change with constant time
  E(disease_model, path = c("(isolated) susceptible", "susceptible") )$weight <- 0.0
  E(disease_model, path = c("(isolated) susceptible", "susceptible") )$label <- paste0("Test Result (TNR)")
  E(disease_model, path = c("(isolated) susceptible", "susceptible") )$curved <- CONST_CURVE_LEVEL_1

  # Isolated susceptible receiving (false) positive test result, moving to a separate compartment for book-keeping of "observed" active cases and "Test positivity rate"
  E(disease_model, path = c("(isolated) susceptible", "(isolated + false positive) susceptible") )$weight <- 0.0
  E(disease_model, path = c("(isolated) susceptible", "(isolated + false positive) susceptible") )$label <- paste0("Test Result (FPR)")
  E(disease_model, path = c("(isolated) susceptible", "(isolated + false positive) susceptible") )$curved <- 0.0

  # Isolated susceptible who received (false) positive test result, moving back to susceptible post a predetermined isolation time
  E(disease_model, path = c("(isolated + false positive) susceptible", "susceptible") )$weight <- CONSTANT_FALSE_POSITIVE_ISOLATION_PERIOD
  E(disease_model, path = c("(isolated + false positive) susceptible", "susceptible") )$label <- paste0(CONSTANT_FALSE_POSITIVE_ISOLATION_PERIOD)
  E(disease_model, path = c("(isolated + false positive) susceptible", "susceptible") )$curved <- 3 * CONST_CURVE_LEVEL_1


  # Unconfirmed infectious (for SIR model) requesting tests, voluntarily or via tracing, deterministic state change with constant time
  E(disease_model, path = c("(unconfirmed) infected", "(isolated + unconfirmed) infected") )$weight <- 0.0
  E(disease_model, path = c("(unconfirmed) infected", "(isolated + unconfirmed) infected") )$label <- paste0("Request for test")
  E(disease_model, path = c("(unconfirmed) infected", "(isolated + unconfirmed) infected") )$curved <- CONST_CURVE_LEVEL_1

  # Unconfirmed infectious moving to removed, stochastic gamma
  E(disease_model, path = c("(unconfirmed) infected", "removed") )$weight <- CONSTANT_REMOVAL_RATE
  E(disease_model, path = c("(unconfirmed) infected", "removed") )$label <- paste0(CONSTANT_REMOVAL_RATE)
  E(disease_model, path = c("(unconfirmed) infected", "removed") )$curved <- 0.0


  # Unconfirmed and Isolated infectious receiving positive test result and joining confirmed infectious population, stochastic state change with constant time
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(isolated + confirmed) infected") )$weight <- 0.0
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(isolated + confirmed) infected") )$label <- paste0("Test Result (TPR)")
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(isolated + confirmed) infected") )$curved <- 0.0

  # Unconfirmed and Isolated infectious receiving negative test result and moving back to unconfirmed infectious population, stochastic state change with constant time
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(unconfirmed) infected") )$weight <- 0.0
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(unconfirmed) infected") )$label <- paste0("Test Result (FNR)")
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(unconfirmed) infected") )$curved <- CONST_CURVE_LEVEL_1


  # Unconfirmed and Isolated infectious moving to removed, stochastic gamma
  E(disease_model, path = c("(isolated + unconfirmed) infected", "removed") )$weight <- CONSTANT_REMOVAL_RATE
  E(disease_model, path = c("(isolated + unconfirmed) infected", "removed") )$label <- paste0(CONSTANT_REMOVAL_RATE)
  E(disease_model, path = c("(isolated + unconfirmed) infected", "removed") )$curved <- 0.0


  # Confirmed and Isolated infectious moving to removed, stochastic gamma
  E(disease_model, path = c("(isolated + confirmed) infected", "removed") )$weight <- CONSTANT_REMOVAL_RATE
  E(disease_model, path = c("(isolated + confirmed) infected", "removed") )$label <- paste0(CONSTANT_REMOVAL_RATE)
  E(disease_model, path = c("(isolated + confirmed) infected", "removed") )$curved <- 0.0


  # Removed requesting tests, due to tracing, deterministic state change with constant time
  E(disease_model, path = c("removed", "(isolated) removed") )$weight <- 0.0
  E(disease_model, path = c("removed", "(isolated) removed") )$label <- paste0("Request for test")
  E(disease_model, path = c("removed", "(isolated) removed") )$curved <- CONST_CURVE_LEVEL_1

  # Isolated removed receiving negative test result and joining back the removed population, stochastic state change with constant time
  E(disease_model, path = c("(isolated) removed", "removed") )$weight <- 0.0
  E(disease_model, path = c("(isolated) removed", "removed") )$label <- paste0("Test Result (TNR)")
  E(disease_model, path = c("(isolated) removed", "removed") )$curved <- CONST_CURVE_LEVEL_1


  # Isolated removed receiving (false) positive test result, moving to a separate compartment for book-keeping of "observed" active cases and "Test positivity rate"
  E(disease_model, path = c("(isolated) removed", "(isolated + false positive) removed") )$weight <- 0.0
  E(disease_model, path = c("(isolated) removed", "(isolated + false positive) removed") )$label <- paste0("Test Result (FPR)")
  E(disease_model, path = c("(isolated) removed", "(isolated + false positive) removed") )$curved <- 0.0

  # Isolated removed who received (false) positive test result, moving back to susceptible post a predetermined isolation time
  E(disease_model, path = c("(isolated + false positive) removed", "removed") )$weight <- CONSTANT_FALSE_POSITIVE_ISOLATION_PERIOD
  E(disease_model, path = c("(isolated + false positive) removed", "removed") )$label <- paste0(CONSTANT_FALSE_POSITIVE_ISOLATION_PERIOD)
  E(disease_model, path = c("(isolated + false positive) removed", "removed") )$curved <- 3 * CONST_CURVE_LEVEL_1




  # Generate transition matrix
  transition_matrix <- as_adjacency_matrix(disease_model, attr="weight", sparse = FALSE)

  STATE_NAMES = colnames(transition_matrix)

  # Book keeping

  # Calibration mode:
  STATELIST_SUSCEPTIBLE <- which(colnames(transition_matrix) == "susceptible") # isolated agents do not participate in receiving infection

  STATELIST_INFECTIOUS <- which(colnames(transition_matrix) == "(unconfirmed) infected" ) # only participating infectious state in propagating infection

  # Separated to setup proportions of Omicron and Non-omicron infection events
  STATELIST_INFECTIOUS_OMICRON <- c()

  STATELIST_NEW_CASES <- which(colnames(transition_matrix) == "(isolated + confirmed) infected" |
                                 colnames(transition_matrix) == "(isolated + false positive) susceptible" |
                                 colnames(transition_matrix) == "(isolated + false positive) removed")

  STATELIST_ACTIVE_CASES <- which(colnames(transition_matrix) == "(isolated + confirmed) infected" |
                                    colnames(transition_matrix) == "(isolated + false positive) susceptible" |
                                    colnames(transition_matrix) == "(isolated + false positive) removed")

  STATELIST_TERMINAL <- NULL # which(colnames(transition_matrix) == "removed")


  # For testing
  # Infectious and non-infected voluntary test seekers
  STATELIST_SYMPTOMATIC_INFECTIOUS_TEST_SEEKER <- which(colnames(transition_matrix) == "(unconfirmed) infected" )
  STATELIST_ASYMPTOMATIC_INFECTIOUS_TEST_SEEKER <- c()
  STATELIST_NOT_INFECTED_TEST_SEEKER <- which( colnames(transition_matrix) == "susceptible" |
                                                 colnames(transition_matrix) == "removed"  ) # Non-voluntary, contact-tracing based test seeker

  # Infectious and non-infected result seekers
  STATELIST_SYMPTOMATIC_INFECTIOUS_RESULT_SEEKER <- which(colnames(transition_matrix) == "(isolated + unconfirmed) infected" )
  STATELIST_ASYMPTOMATIC_INFECTIOUS_RESULT_SEEKER <- c()
  STATELIST_NOT_INFECTED_RESULT_SEEKER <- which( colnames(transition_matrix) == "(isolated) susceptible" |
                                                   colnames(transition_matrix) == "(isolated) removed") # Non-voluntary contact-tracing based test-result seeker

} else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {

  # Begin construction of SIR disease model with extra compartments for Hospitalisation with and without ICU, Recovered, Dead Testing, Isolation and Contact tracing
  # ----------------------------

  cat("\nConstructing disease model: ", CONST_CHOSEN_DISEASE_MODEL, sep = "")
  cat("\n '-CONST_CHOSEN_TEST_ALLOCATION_STRATEGY: ", CONST_CHOSEN_TEST_ALLOCATION_STRATEGY, sep = "")

  # Infection states for 0 doses:
  disease_model <- graph( edges = c("susceptible", "(unconfirmed) infected",
                                    "susceptible", "(isolated) susceptible",

                                    "(isolated) susceptible", "susceptible",
                                    "(isolated) susceptible", "(isolated + false positive) susceptible",

                                    "(isolated + false positive) susceptible", "susceptible",

                                    "(unconfirmed) infected", "(isolated + unconfirmed) infected",
                                    "(unconfirmed) infected", "recovered",
                                    "(unconfirmed) infected", "dead",

                                    "(isolated + unconfirmed) infected", "(isolated + confirmed) infected",
                                    "(isolated + unconfirmed) infected", "(unconfirmed) infected",

                                    
                                    "(isolated + unconfirmed) infected", "recovered",
                                    "(isolated + confirmed) infected", "recovered",
                                    
                                    
                                    "(isolated + unconfirmed) infected", "hospitalised",
                                    "(isolated + confirmed) infected", "hospitalised",
                                    

                                    "hospitalised", "hospitalised with ICU",
                                    

                                    "hospitalised", "recovered",
                                    "hospitalised", "dead",

                                    "hospitalised with ICU", "recovered",
                                    "hospitalised with ICU", "dead",



                                    "recovered", "(isolated) recovered",

                                    "(isolated) recovered", "recovered",
                                    "(isolated) recovered", "(isolated + false positive) recovered",

                                    "(isolated + false positive) recovered", "recovered"), directed = TRUE)

  # For people who are did not get tested or get hospital care
  CONSTANT_FALSE_POSITIVE_ISOLATION_PERIOD <- 1/12 # infectious period (10 days) + 2 days

  CONSTANT_REMOVAL_RATE <- 1/10 # inverse of the period a person may carry the contagion


  CONST_KAPPA <- 2/5       # inverse of the period after getting tested and before hospitalisation
  CONST_rho <- 1/10        # inverse of the period for hospitalization without ICU
  CONST_sigma <- 1/10      # inverse of the period for hospitalization with ICU

  # Calibrated with Robert's help
  CONST_nu_m <- 0.9812     # fraction of cases never hospitalized (-> mild)
  CONST_nu_c <- 0.0028      # fraction of cases hospitalized in ICU for part of stay
  CONST_nu_h <- 0.0160       # fraction of cases hospitalized but never in ICU

  # CONST_nu_m + CONST_nu_c + CONST_nu_h must be 1 
  # Assuming a mean period of 1 day of stay in Hospital, let the mean of 
  # a Negative Binomial distribution of at least one success in r trials to transition to ICU be C = CONST_nu_c / (CONST_nu_h + CONST_nu_c)
  # which is equal to r * p / (1 - p), where p is the probability of transmission, re-arranging 
  #
  # p = 1/(r/C + 1)
  # For example if r was the length of a sim, then per-day chance: p = 1/(210/C + 1) = 0.0007087172
  # 
  # Setting r to mean period of hospitalisation ~ 10
  
  CONST_h_to_icu <- 1/( (10 * (CONST_nu_h + CONST_nu_c))/CONST_nu_c + 1)

  CONST_f_gen <- 1/100000     # fraction of non-hospitalised resulting in death
  CONST_f_c <- 1/2            # fraction of ICU hospitalization resulting in death
  CONST_f_h <- 1/10          # fraction of non-ICU hospitalization resulting in death


  # Graph visualization parameters
  CONST_CURVE_LEVEL_1 = 0.3

  # beta derived from contact matrix
  E(disease_model, path = c("susceptible", "(unconfirmed) infected") )$weight <- "CALCULATE_FROM_CONTACT_MATRIX"
  E(disease_model, path = c("susceptible", "(unconfirmed) infected") )$label <- paste0("CALCULATE_FROM_CONTACT_MATRIX")
  E(disease_model, path = c("susceptible", "(unconfirmed) infected") )$curved <- 0.0

  # Susceptible requesting tests, voluntarily or via tracing, deterministic state change with constant time
  E(disease_model, path = c("susceptible", "(isolated) susceptible") )$weight <- 0.0
  E(disease_model, path = c("susceptible", "(isolated) susceptible") )$label <- paste0("Request for test")
  E(disease_model, path = c("susceptible", "(isolated) susceptible") )$curved <- CONST_CURVE_LEVEL_1

  # Isolated susceptible receiving negative test result and joining back the susceptible population, stochastic state change with constant time
  E(disease_model, path = c("(isolated) susceptible", "susceptible") )$weight <- 0.0
  E(disease_model, path = c("(isolated) susceptible", "susceptible") )$label <- paste0("Test Result (TNR)")
  E(disease_model, path = c("(isolated) susceptible", "susceptible") )$curved <- CONST_CURVE_LEVEL_1

  # Isolated susceptible receiving (false) positive test result, moving to a separate compartment for book-keeping of "observed" active cases and "Test positivity rate"
  E(disease_model, path = c("(isolated) susceptible", "(isolated + false positive) susceptible") )$weight <- 0.0
  E(disease_model, path = c("(isolated) susceptible", "(isolated + false positive) susceptible") )$label <- paste0("Test Result (FPR)")
  E(disease_model, path = c("(isolated) susceptible", "(isolated + false positive) susceptible") )$curved <- 0.0

  # Isolated susceptible who received (false) positive test result, moving back to susceptible post a predetermined isolation time
  E(disease_model, path = c("(isolated + false positive) susceptible", "susceptible") )$weight <- CONSTANT_FALSE_POSITIVE_ISOLATION_PERIOD
  E(disease_model, path = c("(isolated + false positive) susceptible", "susceptible") )$label <- paste0(CONSTANT_FALSE_POSITIVE_ISOLATION_PERIOD)
  E(disease_model, path = c("(isolated + false positive) susceptible", "susceptible") )$curved <- 3 * CONST_CURVE_LEVEL_1



  # Unconfirmed infectious (for SIR model) requesting tests, voluntarily or via tracing, deterministic state change with constant time
  E(disease_model, path = c("(unconfirmed) infected", "(isolated + unconfirmed) infected") )$weight <- 0.0
  E(disease_model, path = c("(unconfirmed) infected", "(isolated + unconfirmed) infected") )$label <- paste0("Request for test")
  E(disease_model, path = c("(unconfirmed) infected", "(isolated + unconfirmed) infected") )$curved <- CONST_CURVE_LEVEL_1

  # Unconfirmed infectious moving to recovered, stochastic gamma
  E(disease_model, path = c("(unconfirmed) infected", "recovered") )$weight <- CONSTANT_REMOVAL_RATE * (1 - CONST_f_gen)
  E(disease_model, path = c("(unconfirmed) infected", "recovered") )$label <- paste0(CONSTANT_REMOVAL_RATE * (1 - CONST_f_gen))
  E(disease_model, path = c("(unconfirmed) infected", "recovered") )$curved <- 0.0

  # Unconfirmed infectious moving to dead, stochastic gamma
  E(disease_model, path = c("(unconfirmed) infected", "dead") )$weight <- CONSTANT_REMOVAL_RATE * CONST_f_gen
  E(disease_model, path = c("(unconfirmed) infected", "dead") )$label <- paste0(CONSTANT_REMOVAL_RATE * CONST_f_gen)
  E(disease_model, path = c("(unconfirmed) infected", "dead") )$curved <- 0.0





  # Unconfirmed and Isolated infectious receiving positive test result and joining confirmed infectious population, stochastic state change with constant time
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(isolated + confirmed) infected") )$weight <- 0.0
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(isolated + confirmed) infected") )$label <- paste0("Test Result (TPR)")
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(isolated + confirmed) infected") )$curved <- 0.0

  # Unconfirmed and Isolated infectious receiving negative test result and moving back to unconfirmed infectious population, stochastic state change with constant time
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(unconfirmed) infected") )$weight <- 0.0
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(unconfirmed) infected") )$label <- paste0("Test Result (FNR)")
  E(disease_model, path = c("(isolated + unconfirmed) infected", "(unconfirmed) infected") )$curved <- CONST_CURVE_LEVEL_1


  # Unconfirmed and Isolated infectious hospitalising before receiving test result
  E(disease_model, path = c("(isolated + unconfirmed) infected", "hospitalised") )$weight <- CONST_KAPPA * CONST_nu_h
  E(disease_model, path = c("(isolated + unconfirmed) infected", "hospitalised") )$label <- paste0(CONST_KAPPA * CONST_nu_h)
  E(disease_model, path = c("(isolated + unconfirmed) infected", "hospitalised") )$curved <- 0.0


  # Confirmed and Isolated infectious hospitalising after receiving test result
  E(disease_model, path = c("(isolated + confirmed) infected", "hospitalised") )$weight <- CONST_KAPPA * CONST_nu_h
  E(disease_model, path = c("(isolated + confirmed) infected", "hospitalised") )$label <- paste0(CONST_KAPPA * CONST_nu_h)
  E(disease_model, path = c("(isolated + confirmed) infected", "hospitalised") )$curved <- 0.0
  
  
  
  
  # Unconfirmed and Isolated infectious recovering before receiving test result
  E(disease_model, path = c("(isolated + unconfirmed) infected", "recovered") )$weight <- CONSTANT_REMOVAL_RATE * CONST_nu_m
  E(disease_model, path = c("(isolated + unconfirmed) infected", "recovered") )$label <- paste0(CONSTANT_REMOVAL_RATE * CONST_nu_m)
  E(disease_model, path = c("(isolated + unconfirmed) infected", "recovered") )$curved <- 0.0
  
  
  # Confirmed and Isolated infectious recovering after receiving test result
  E(disease_model, path = c("(isolated + confirmed) infected", "recovered") )$weight <- CONSTANT_REMOVAL_RATE * CONST_nu_m
  E(disease_model, path = c("(isolated + confirmed) infected", "recovered") )$label <- paste0(CONSTANT_REMOVAL_RATE * CONST_nu_m)
  E(disease_model, path = c("(isolated + confirmed) infected", "recovered") )$curved <- 0.0


  
  
  
  # Hospitalised population seeking ICU
  E(disease_model, path = c("hospitalised", "hospitalised with ICU") )$weight <- (CONST_h_to_icu)
  E(disease_model, path = c("hospitalised", "hospitalised with ICU") )$label <- paste0(CONST_h_to_icu)
  E(disease_model, path = c("hospitalised", "hospitalised with ICU") )$curved <- 0.0



  # Hospitalised population recovering
  E(disease_model, path = c("hospitalised", "recovered") )$weight <- CONST_rho * (1 - CONST_f_h)
  E(disease_model, path = c("hospitalised", "recovered") )$label <- paste0(CONST_rho * (1 - CONST_f_h))
  E(disease_model, path = c("hospitalised", "recovered") )$curved <- 0.0

  # Hospitalised population dying
  E(disease_model, path = c("hospitalised", "dead") )$weight <- CONST_rho * CONST_f_h
  E(disease_model, path = c("hospitalised", "dead") )$label <- paste0(CONST_rho * CONST_f_h)
  E(disease_model, path = c("hospitalised", "dead") )$curved <- 0.0




  # Hospitalised with ICU care population recovering
  E(disease_model, path = c("hospitalised with ICU", "recovered") )$weight <- CONST_sigma * (1 - CONST_f_c)
  E(disease_model, path = c("hospitalised with ICU", "recovered") )$label <- paste0(CONST_sigma * (1 - CONST_f_c))
  E(disease_model, path = c("hospitalised with ICU", "recovered") )$curved <- 0.0

  # Hospitalised population dying
  E(disease_model, path = c("hospitalised with ICU", "dead") )$weight <- CONST_sigma * CONST_f_c
  E(disease_model, path = c("hospitalised with ICU", "dead") )$label <- paste0(CONST_sigma * CONST_f_c)
  E(disease_model, path = c("hospitalised with ICU", "dead") )$curved <- 0.0



  # Recovered requesting tests, due to tracing, deterministic state change with constant time
  E(disease_model, path = c("recovered", "(isolated) recovered") )$weight <- 0.0
  E(disease_model, path = c("recovered", "(isolated) recovered") )$label <- paste0("Request for test")
  E(disease_model, path = c("recovered", "(isolated) recovered") )$curved <- CONST_CURVE_LEVEL_1

  # Isolated Recovered receiving negative test result and joining back the recovered population, stochastic state change with constant time
  E(disease_model, path = c("(isolated) recovered", "recovered") )$weight <- 0.0
  E(disease_model, path = c("(isolated) recovered", "recovered") )$label <- paste0("Test Result (TNR)")
  E(disease_model, path = c("(isolated) recovered", "recovered") )$curved <- CONST_CURVE_LEVEL_1


  # Isolated Recovered receiving (false) positive test result, moving to a separate compartment for book-keeping of "observed" active cases and "Test positivity rate"
  E(disease_model, path = c("(isolated) recovered", "(isolated + false positive) recovered") )$weight <- 0.0
  E(disease_model, path = c("(isolated) recovered", "(isolated + false positive) recovered") )$label <- paste0("Test Result (FPR)")
  E(disease_model, path = c("(isolated) recovered", "(isolated + false positive) recovered") )$curved <- 0.0

  # Isolated Recovered who received (false) positive test result, moving back to Recovered post a predetermined isolation time
  E(disease_model, path = c("(isolated + false positive) recovered", "recovered") )$weight <- CONSTANT_FALSE_POSITIVE_ISOLATION_PERIOD
  E(disease_model, path = c("(isolated + false positive) recovered", "recovered") )$label <- paste0(CONSTANT_FALSE_POSITIVE_ISOLATION_PERIOD)
  E(disease_model, path = c("(isolated + false positive) recovered", "recovered") )$curved <- 3 * CONST_CURVE_LEVEL_1




  # Generate transition matrix
  transition_matrix <- as_adjacency_matrix(disease_model, attr="weight", sparse = FALSE)

  STATE_NAMES = colnames(transition_matrix)

  # Book keeping

  # Calibration mode:
  STATELIST_SUSCEPTIBLE <- which(colnames(transition_matrix) == "susceptible") # isolated agents do not participate in receiving infection

  STATELIST_INFECTIOUS <- which(colnames(transition_matrix) == "(unconfirmed) infected" ) # only participating infectious state in propagating infection

  # Separated to setup proportions of Omicron and Non-omicron infection events
  STATELIST_INFECTIOUS_OMICRON <- c()

  STATELIST_NEW_CASES <- which(colnames(transition_matrix) == "(isolated + confirmed) infected" |
                                 colnames(transition_matrix) == "(isolated + false positive) susceptible" |
                                 colnames(transition_matrix) == "(isolated + false positive) recovered")

  STATELIST_ACTIVE_CASES <- which(colnames(transition_matrix) == "(isolated + confirmed) infected" |
                                    colnames(transition_matrix) == "(isolated + false positive) susceptible" |
                                    colnames(transition_matrix) == "hospitalised" |
                                    colnames(transition_matrix) == "hospitalised with ICU" |
                                    colnames(transition_matrix) == "(isolated + false positive) recovered")

  STATELIST_TERMINAL <- which(colnames(transition_matrix) == "dead")


  # For testing
  # Infectious and non-infected voluntary test seekers
  STATELIST_SYMPTOMATIC_INFECTIOUS_TEST_SEEKER <- which(colnames(transition_matrix) == "(unconfirmed) infected" )
  STATELIST_ASYMPTOMATIC_INFECTIOUS_TEST_SEEKER <- c()
  STATELIST_NOT_INFECTED_TEST_SEEKER <- which( colnames(transition_matrix) == "susceptible" |
                                                 colnames(transition_matrix) == "recovered"  ) # Non-voluntary, contact-tracing based test seeker

  # Infectious and non-infected result seekers
  STATELIST_SYMPTOMATIC_INFECTIOUS_RESULT_SEEKER <- which(colnames(transition_matrix) == "(isolated + unconfirmed) infected" )
  STATELIST_ASYMPTOMATIC_INFECTIOUS_RESULT_SEEKER <- c()
  STATELIST_NOT_INFECTED_RESULT_SEEKER <- which( colnames(transition_matrix) == "(isolated) susceptible" |
                                                   colnames(transition_matrix) == "(isolated) recovered") # Non-voluntary contact-tracing based test-result seeker

}




# If-else ladder of disease model construction ends
# *************************************************************

plot(disease_model, layout=layout_as_tree)

# Interactive plot
# tkplot(disease_model, layout=layout_as_tree,
#        canvas.height = 900, canvas.width = 1600,
#        vertex.color="#77c98d", edge.arrow.size=0.75)

# Debug output graph
E(disease_model)
edge_attr(disease_model)



{
  # Demographic data pre-processing
  demographic_data_json <- read_json(DEMOGRAPHIC_DATA_FILEPATH)

  # Setup demographic data as "Person id", "Age", "Occupation type", "Household income"
  COUNT_PERSON_WITH_DEMOGRAPHIC_DATA <- length(demographic_data_json)

  demographic_data_mat <- matrix (ncol = 4, nrow = COUNT_PERSON_WITH_DEMOGRAPHIC_DATA)
  demographic_data_mat_colnames <- c("person_id", "age", "occupation_type", "household_income")
  colnames(demographic_data_mat) <- demographic_data_mat_colnames

  # Populate demographic data as matrix
  for(i in 1:COUNT_PERSON_WITH_DEMOGRAPHIC_DATA){
    temp_json_element <- demographic_data_json[[i]]

    demographic_data_mat[i, "person_id"] <- strtoi(i) # converting all Json person_ids to person_id + 1

    demographic_data_mat[i, "age"] <- strtoi(temp_json_element$age)
    demographic_data_mat[i, "occupation_type"] <- temp_json_element$job
    demographic_data_mat[i, "household_income"] <- strtoi(temp_json_element$hhIncome)
  }
}

# Population distribution for a performance arts event
{
  # For a performance arts event (including and not limited to live music)
  # attendee's age distribution is as follows:
  # Ref: https://hillstrategies.com/wp-content/uploads/1970/01/Perfarts_report.pdf
  #
  # For Atlantic Canada:
  #
  # 15-29: 34.1%
  # 30-44: 27.1%
  # 45-59: 25.3%
  # 60+  : 18.2%

  performance_arts_age_dist_mat <- matrix ( c(15, 29, 34.1,
                                              30, 44, 27.1,
                                              45, 59, 25.3,
                                              60, 100, 18.2),
                                            ncol = 3, nrow = 4, byrow = TRUE)

  performance_arts_age_dist_mat_colnames <- c("person_age_start", "person_age_end", "percentage")
  colnames(performance_arts_age_dist_mat) <- performance_arts_age_dist_mat_colnames
}
# Extract event attendee list (of person ids with schedules) based on the age distribution
attendee_list <- c() # Empty list



cat("\nReading contact matrix of type: ", CONST_CHOSEN_CONTACT_MATRIX_TYPE, sep ="")
# Memory Expensive approach to reduce total I/O

# To Read all the matrices in a list
list_of_contact_matrix <- list()
list_of_contact_matrix_w_event <- list()
matrix_index <- 1

# Read all matrices and find out total simulated persons and total available (contact) matries
if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Aggregated") {


  # Upper bound of simulated persons
  TOTAL_SIMULATED_PERSONS <- 0

  # Find the max id of simulated agent with overlap
  for(CONTACT_MATRIX_AS_PAIRLIST_FILENAME in LIST_OF_CONTACT_MATRIX_FILEPATH){
    # Debug:
    # cat(CONTACT_MATRIX_AS_PAIRLIST_FILENAME, "\n")

    # Read contact matrix
    contact_matrix_as_pairlist <- read.table(CONTACT_MATRIX_AS_PAIRLIST_FILENAME, sep=",")

    # Adding readable column names
    colnames(contact_matrix_as_pairlist) <- c("person_id_1", "person_id_2", "contact_in_seconds")

    COUNT_MAX_PERSON_ID <- max( max(contact_matrix_as_pairlist$person_id_1), max(contact_matrix_as_pairlist$person_id_2))

    TOTAL_SIMULATED_PERSONS <- max(TOTAL_SIMULATED_PERSONS, COUNT_MAX_PERSON_ID)

  }

} else if (CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Location based"){

  # Upper bound of simulated persons
  TOTAL_SIMULATED_PERSONS <- 0

  # Find the max id of simulated agent with overlap
  for(CONTACT_MATRIX_AS_PAIRLIST_FILENAME in LIST_OF_CONTACT_MATRIX_FILEPATH){
    # Debug:
    # cat(CONTACT_MATRIX_AS_PAIRLIST_FILENAME, "\n")

    # Read contact matrix
    contact_matrix_as_pairlist <- read.table(CONTACT_MATRIX_AS_PAIRLIST_FILENAME, sep=",", header = TRUE)

    # CSV Column names: person_1,	person_2,	contact_time,	location_id,	citisketch_location_type,	citisketch_location_id <- Note location_id and citisketch_location_id indices are swapped in the CSV

    # Overriding column names: person_id_1, person_id_2, contact_in_seconds ...
    colnames(contact_matrix_as_pairlist) <- c("person_id_1", "person_id_2", "contact_in_seconds", "citisketch_location_id",	"citisketch_location_type", "internal_id")

    COUNT_MAX_PERSON_ID <- max( max(contact_matrix_as_pairlist$person_id_1), max(contact_matrix_as_pairlist$person_id_2) )

    TOTAL_SIMULATED_PERSONS <- max(TOTAL_SIMULATED_PERSONS, COUNT_MAX_PERSON_ID)

  }

}


# Generate attendee list
# iterate over summary stats to construct the full attendee list from available agents
for (row_index in 1:nrow(performance_arts_age_dist_mat)){
  possible_agent_list <- intersect(demographic_data_mat[which(
    strtoi( demographic_data_mat[ , "age"] ) >=  performance_arts_age_dist_mat[[row_index, "person_age_start"]] &
      strtoi(demographic_data_mat[ , "age"]) <= performance_arts_age_dist_mat[[row_index, "person_age_end"]] )],
    seq(1:COUNT_MAX_PERSON_ID))

  temp_attendee_list <- sample(possible_agent_list,
                               min(length(possible_agent_list),
                                   floor((performance_arts_age_dist_mat[[row_index, "percentage"]]/100)*COUNT_EVENT_ATTENDANCE)))

  attendee_list <- c(attendee_list, temp_attendee_list)

}

# Tag: @calib08
# Generate event contacts
if(GENERATE_EVENT_CONTACT_PAIRS){
  COUNT_ATTENDEES <- length(attendee_list)


  # Convert to contact pairlist and corresponding matrix
  event_contact_pairs <- combn(attendee_list, 2)

  # Changing from constant time spent to a gaussian/normal distribution around half the duration of the event with 30 minutes of sd
  # contact_time <- rep(EVENT_DURATION_IN_SECONDS, ncol(event_contact_pairs))
  contact_time <- rnorm(ncol(event_contact_pairs), mean = (EVENT_DURATION_IN_SECONDS)/2, sd = 0.5 * 60 * 60)

  event_contact_pairs <- rbind(event_contact_pairs, contact_time)

  event_contact_pairs <- t(event_contact_pairs)
  colnames(event_contact_pairs) <- c("person_id_1", "person_id_2", "contact_in_seconds")

  # Generate the symmetric pairs
  symmetric_event_contact_pairs <- event_contact_pairs
  symmetric_event_contact_pairs[ , c(1,2)]  <- symmetric_event_contact_pairs[ , c(2,1)]

  event_contact_pairs <- rbind(event_contact_pairs, symmetric_event_contact_pairs)


  # Tag: @calib08
  # Format for appropriate contact matrix types
  if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Aggregated") {
    # Generate the matrix augment
    sparse_event_contact_matrix <- sparseMatrix(i = event_contact_pairs[ , "person_id_1"],
                                                j = event_contact_pairs[ , "person_id_2"],
                                                x = event_contact_pairs[ , "contact_in_seconds"],
                                                dims = c(TOTAL_SIMULATED_PERSONS, TOTAL_SIMULATED_PERSONS))

    event_contact_matrix_augment <- array( data = sparse_event_contact_matrix,
                                           dim = c(TOTAL_SIMULATED_PERSONS, TOTAL_SIMULATED_PERSONS))

  } else if (CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Location based"){
    # Add appropriate columns
    citisketch_location_id <- rep(-2, nrow(event_contact_pairs)) # Special escape value, Note: may enumerate for multiple events
    citisketch_location_type <- rep("Simulated Event", nrow(event_contact_pairs)) # Human readable venue name

    internal_id <- rep(-2, nrow(event_contact_pairs)) # Special escape value

    event_contact_pairs <- cbind(event_contact_pairs, citisketch_location_id)
    event_contact_pairs <- cbind(event_contact_pairs, citisketch_location_type)
    event_contact_pairs <- cbind(event_contact_pairs, internal_id)

    # Convert to data frame
    event_contact_pairs <- as.data.frame(event_contact_pairs)

  }
}

# Create a dual for each contact matrix with simulated event augment during pre-process

# Pre-process contact-matrices
if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Aggregated") {




  COUNT_CONTACT_MATRIX <- length(LIST_OF_CONTACT_MATRIX_FILEPATH)

  # List of list of vectors of contacts

  # Initialising as empty lists
  list_day_x_person_x_contact <- list()
  list_day_x_person_x_contact_w_event <- list()

  for(i in 1:COUNT_CONTACT_MATRIX){
    temp_list_person_x_contact <- list()
    for(j in 1:TOTAL_SIMULATED_PERSONS){
      temp_list_person_x_contact[[j]] <- list()
    }
    list_day_x_person_x_contact[[i]] <- temp_list_person_x_contact
    list_day_x_person_x_contact_w_event[[i]] <- temp_list_person_x_contact
  }


  # Pre-load all contact matrices
  for(CONTACT_MATRIX_AS_PAIRLIST_FILENAME in LIST_OF_CONTACT_MATRIX_FILEPATH){
    # Debug:
    # cat(CONTACT_MATRIX_AS_PAIRLIST_FILENAME, "\n")

    # Read contact matrix
    contact_matrix_as_pairlist <- read.table(CONTACT_MATRIX_AS_PAIRLIST_FILENAME, sep=",")

    # Adding readable column names
    colnames(contact_matrix_as_pairlist) <- c("person_id_1", "person_id_2", "contact_in_seconds")

    # COUNT_MAX_PERSON_ID <- max( max(contact_matrix_as_pairlist$person_id_1), max(contact_matrix_as_pairlist$person_id_2))

    # Converting to sparse matrices
    sparse_contact_matrix <- sparseMatrix(i = contact_matrix_as_pairlist$person_id_1,
                                          j = contact_matrix_as_pairlist$person_id_2,
                                          # x = getInfection_probability(contact_matrix_as_pairlist$contact_in_seconds)), infection prorbability is now w.r.t viral strain and vaccination status
                                          x = contact_matrix_as_pairlist$contact_in_seconds,
                                          dims = c(TOTAL_SIMULATED_PERSONS, TOTAL_SIMULATED_PERSONS))

    # convert to array
    contact_matrix <- array( data = sparse_contact_matrix,
                             dim = c(TOTAL_SIMULATED_PERSONS, TOTAL_SIMULATED_PERSONS))

    # Converting to dataframe
    # contact_matrix_as_pairlist <- data.frame(contact_matrix_as_pairlist)

    # contact_matrix_as_pairlist$infection_prob <- getInfection_probability(contact_matrix_as_pairlist$contact_in_seconds)



    # list_of_contact_matrix[[matrix_index]] <- contact_matrix_as_pairlist


    list_of_contact_matrix[[matrix_index]] <- contact_matrix

    # Construct bit vectors @bitvector
    # for(person_id in 1:TOTAL_SIMULATED_PERSONS){
    #   list_day_x_person_x_contact[[matrix_index]][[person_id]] <- as.bit(contact_matrix[person_id, ])
    # }

    # Trying with list of list of vectors of contacts
    for(person_id in 1:TOTAL_SIMULATED_PERSONS){
      list_day_x_person_x_contact[[matrix_index]][[person_id]] <- which(contact_matrix[person_id, ] > 0)
    }

    # Tag: @calib08
    # Create event augmented contact matrix
    if(GENERATE_EVENT_CONTACT_PAIRS){
      list_of_contact_matrix_w_event[[matrix_index]] <- contact_matrix + event_contact_matrix_augment

      # Neighbour list including event based contact
      for (person_id in 1:TOTAL_SIMULATED_PERSONS){
        list_day_x_person_x_contact_w_event[[matrix_index]][[person_id]] <- which(list_of_contact_matrix_w_event[[matrix_index]][person_id, ] > 0)
      }
    }

    matrix_index <- matrix_index + 1
  }
  COUNT_CONTACT_MATRIX <- length(list_of_contact_matrix)

} else if (CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Location based"){


  COUNT_CONTACT_MATRIX <- length(LIST_OF_CONTACT_MATRIX_FILEPATH)

  # List of list of vectors of contacts and their details
  # Note: Using multiple vectors for possible speedup instead of an encapsulating data structure
  # Initialising as empty lists
  list_day_x_person_x_contact <- list()
  list_day_x_person_x_contact_time <- list()
  list_day_x_person_x_contact_location_type <- list()
  list_day_x_person_x_contact_location_id <- list()

  list_day_x_person_x_contact_w_event <- list()
  list_day_x_person_x_contact_time_w_event <- list()
  list_day_x_person_x_contact_location_type_w_event <- list()
  list_day_x_person_x_contact_location_id_w_event <- list()

  for(i in 1:COUNT_CONTACT_MATRIX){
    temp_list_person_x_contact <- list()
    for(j in 1:TOTAL_SIMULATED_PERSONS){
      temp_list_person_x_contact[[j]] <- list()
    }
    list_day_x_person_x_contact[[i]] <- temp_list_person_x_contact
    list_day_x_person_x_contact_time[[i]] <- temp_list_person_x_contact
    list_day_x_person_x_contact_location_type[[i]] <- temp_list_person_x_contact
    list_day_x_person_x_contact_location_id[[i]] <- temp_list_person_x_contact

    list_day_x_person_x_contact_w_event[[i]] <- temp_list_person_x_contact
    list_day_x_person_x_contact_time_w_event[[i]] <- temp_list_person_x_contact
    list_day_x_person_x_contact_location_type_w_event[[i]] <- temp_list_person_x_contact
    list_day_x_person_x_contact_location_id_w_event[[i]] <- temp_list_person_x_contact
  }

  # Pre-load all contact matrices
  for(CONTACT_MATRIX_AS_PAIRLIST_FILENAME in LIST_OF_CONTACT_MATRIX_FILEPATH){
    # Debug:
    # cat(CONTACT_MATRIX_AS_PAIRLIST_FILENAME, "\n")

    # Read contact matrix
    contact_matrix_as_pairlist <- read.table(CONTACT_MATRIX_AS_PAIRLIST_FILENAME, sep=",", header = TRUE)

    # Overriding column names
    colnames(contact_matrix_as_pairlist) <- c("person_id_1", "person_id_2", "contact_in_seconds", "citisketch_location_id", "citisketch_location_type",  "internal_id")

    list_of_contact_matrix[[matrix_index]] <- contact_matrix_as_pairlist

    # Trying with list of list of vectors of contacts
    for(person_id in 1:TOTAL_SIMULATED_PERSONS){
      list_day_x_person_x_contact[[matrix_index]][[person_id]] <- contact_matrix_as_pairlist[which(contact_matrix_as_pairlist$person_id_1 == person_id), ]$person_id_2
      list_day_x_person_x_contact_time[[matrix_index]][[person_id]] <- contact_matrix_as_pairlist[which(contact_matrix_as_pairlist$person_id_1 == person_id), ]$contact_in_seconds
      list_day_x_person_x_contact_location_type[[matrix_index]][[person_id]] <- contact_matrix_as_pairlist[which(contact_matrix_as_pairlist$person_id_1 == person_id), ]$citisketch_location_type
      list_day_x_person_x_contact_location_id[[matrix_index]][[person_id]] <- contact_matrix_as_pairlist[which(contact_matrix_as_pairlist$person_id_1 == person_id), ]$citisketch_location_id
    }

    # Tag: @calib08
    # Create event augmented contact matrix
    if(GENERATE_EVENT_CONTACT_PAIRS){
      contact_matrix_as_pairlist_w_event <- rbind(contact_matrix_as_pairlist, event_contact_pairs)
      list_of_contact_matrix_w_event[[matrix_index]] <- contact_matrix_as_pairlist_w_event

      # Neighbour list including event based contact
      for(person_id in 1:TOTAL_SIMULATED_PERSONS){
        list_day_x_person_x_contact_w_event[[matrix_index]][[person_id]] <- as.numeric( contact_matrix_as_pairlist_w_event[which(contact_matrix_as_pairlist_w_event$person_id_1 == person_id), ]$person_id_2 )
        list_day_x_person_x_contact_time_w_event[[matrix_index]][[person_id]] <- as.numeric( contact_matrix_as_pairlist_w_event[which(contact_matrix_as_pairlist_w_event$person_id_1 == person_id), ]$contact_in_seconds )
        list_day_x_person_x_contact_location_type_w_event[[matrix_index]][[person_id]] <- contact_matrix_as_pairlist_w_event[which(contact_matrix_as_pairlist_w_event$person_id_1 == person_id), ]$citisketch_location_type
        list_day_x_person_x_contact_location_id_w_event[[matrix_index]][[person_id]] <- as.numeric( contact_matrix_as_pairlist_w_event[which(contact_matrix_as_pairlist_w_event$person_id_1 == person_id), ]$citisketch_location_id )
      }
    }


    matrix_index <- matrix_index + 1
  }
  COUNT_CONTACT_MATRIX <- length(list_of_contact_matrix)
}

# Create iterator that works till end of list
contact_matrix_iterator <- iter(list_of_contact_matrix)

# Get first contact matrix
# contact_matrix_as_pairlist <- nextElem(contact_matrix_iterator)
contact_matrix <- nextElem(contact_matrix_iterator)

# Simulation assumption:
# Repeating the condition of available contact matrix by cycling through them
# if no. of days to simulate > available contact matrices
#
#
#  |----COUNT_CONTACT_MATRIX------>|----COUNT_CONTACT_MATRIX------>|
#  +----TOTAL_SIMULATION_DAYS----------------------------------- - - - >



# Initialize state
TOTAL_STATES <- length(colnames(transition_matrix))

TOTAL_SIMULATED_PERSONS <- max(contact_matrix_as_pairlist[1:2])

STATE <- array(rep(NA, TOTAL_SIMULATED_PERSONS * TOTAL_SIMULATION_DAYS),
               dim = c(TOTAL_SIMULATED_PERSONS, TOTAL_SIMULATION_DAYS) )



# Set up simulation population

# Set every person to susceptible for the first day
STATE[ , 1] <- which( colnames(transition_matrix) == "susceptible" )


# Setup infection history for each person in a matrix, where each row is a person id
if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Aggregated"){
  infection_hist_mat <- matrix (ncol = 6, nrow = TOTAL_SIMULATED_PERSONS)
  infection_hist_mat_colnames <- c("variant", "infected on", "infected by", "dose 1 on", "dose 2 on", "dose 3 on")
  colnames(infection_hist_mat) <- infection_hist_mat_colnames
} else if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Location based"){
  # Tag: @calib07 adding infection venue details i.e citisketch_location_type, citisketch_location_id
  infection_hist_mat <- matrix (ncol = 8, nrow = TOTAL_SIMULATED_PERSONS)
  infection_hist_mat_colnames <- c("variant", "infected on", "infected by", "dose 1 on", "dose 2 on", "dose 3 on", "citisketch_location_type", "citisketch_location_id")
  colnames(infection_hist_mat) <- infection_hist_mat_colnames
}





# agent ids of population > 5 years and are present in the schedules
possible_vaccinated_agent_list <- intersect(demographic_data_mat[ which( strtoi( demographic_data_mat[ , "age"] ) >=  5)], seq(1:COUNT_MAX_PERSON_ID))

# Set vaccination state for day 1
if (CONST_CHOSEN_DISEASE_MODEL == "Complex"){
  for(person_id in possible_vaccinated_agent_list){
    vaccination_state <- sample(c( which(colnames(transition_matrix) == "susceptible"),
                                    which(colnames(transition_matrix) ==  "received_dose1"),
                                    which(colnames(transition_matrix) ==  "received_dose2"),
                                    which(colnames(transition_matrix) ==  "received_dose3")
                                  ),
                                  1,
                                  prob = c(CONST_y0, CONST_y1, CONST_y2, CONST_y3))

    STATE[person_id, 1] <- vaccination_state

    # Extra book-keeping (To eventually incorporate waning)
    if(vaccination_state == which(colnames(transition_matrix) ==  "received_dose3")) {
      # Negative value to state vaccination event occurred before simulation
      infection_hist_mat[person_id, "dose 3 on"] <- -1
      infection_hist_mat[person_id, "dose 2 on"] <- -1
      infection_hist_mat[person_id, "dose 1 on"] <- -1
    }
    else if(vaccination_state == which(colnames(transition_matrix) ==  "received_dose2")) {
      # Negative value to state vaccination event occurred before simulation
      infection_hist_mat[person_id, "dose 2 on"] <- -1
      infection_hist_mat[person_id, "dose 1 on"] <- -1
    }
    else if(vaccination_state == which(colnames(transition_matrix) ==  "received_dose1")) {
      # Negative value to state vaccination event occurred before simulation
      infection_hist_mat[person_id, "dose 1 on"] <- -1
    }
  }
} else if (CONST_CHOSEN_DISEASE_MODEL == "SIR") {

  # No vaccination in simple SIR
} else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation") {

  # No vaccination implemented in SIR + Testing + Isolation + Contact Tracing
}

# Initial populations
init_list_of_person_with_0_dose <- which(STATE[ , 1] == which(colnames(transition_matrix) == "susceptible"))
init_list_of_person_with_1_dose <- which(STATE[ , 1] == which(colnames(transition_matrix) == "received_dose1"))
init_list_of_person_with_2_dose <- which(STATE[ , 1] == which(colnames(transition_matrix) == "received_dose2"))
init_list_of_person_with_3_dose <- which(STATE[ , 1] == which(colnames(transition_matrix) == "received_dose3"))

sampled_y0 <- length(init_list_of_person_with_0_dose) / length(possible_vaccinated_agent_list)
sampled_y1 <- length(init_list_of_person_with_1_dose) / length(possible_vaccinated_agent_list)
sampled_y2 <- length(init_list_of_person_with_2_dose) / length(possible_vaccinated_agent_list)
sampled_y3 <-  length(init_list_of_person_with_3_dose) / length(possible_vaccinated_agent_list)

# Debug
cat("\n")
cat("Sampled Y0: ", sampled_y0, "\n")
cat("Sampled Y1: ", sampled_y1, "\n")
cat("Sampled Y2: ", sampled_y2, "\n")
cat("Sampled Y3: ", sampled_y2, "\n")





DAILY_NEW_CASES <- array( rep(0, TOTAL_SIMULATION_DAYS), dim = TOTAL_SIMULATION_DAYS )
DAILY_ACTIVE_CASES <- array( rep(0, TOTAL_SIMULATION_DAYS), dim = TOTAL_SIMULATION_DAYS )

# Tracks un-observed infectious
DAILY_UNOBSERVED_INFECTIOUS_CASES <- array( rep(0, TOTAL_SIMULATION_DAYS), dim = TOTAL_SIMULATION_DAYS )

# Set first day new cases to COUNT_FIRST_DAY_DISEASE_IMPORT for bookkeeping
DAILY_NEW_CASES[1] <- COUNT_FIRST_DAY_DISEASE_IMPORT

# Randomly distribute the infection import
first_disease_import <- sample(seq_len(length(STATE[ , 1])), COUNT_FIRST_DAY_DISEASE_IMPORT) # Worked when there were 0 vaccinated on the 1st day
# Freezing disease import to agent 1, 2, 3 and 4 as decided with Robert and Jarod
# first_disease_import <- c(1, 2, 3, 4)


# first_disease_import <- sample(init_list_of_person_with_0_dose, COUNT_FIRST_DAY_DISEASE_IMPORT)
# first_disease_import <- sample(init_list_of_person_with_1_dose, COUNT_FIRST_DAY_DISEASE_IMPORT)
# first_disease_import <- sample(init_list_of_person_with_2_dose, COUNT_FIRST_DAY_DISEASE_IMPORT)
# first_disease_import <- sample(init_list_of_person_with_3_dose, COUNT_FIRST_DAY_DISEASE_IMPORT)


# active_voc_df <- filter(voc_df, Proportion > 0)
# active_voc_mat <- as.matrix.data.frame(active_voc_df)

active_voc_mat <- as.matrix.data.frame(filter(voc_df, Proportion > 0))


day_1_varaint_list <- list()

cat("Setting first day disease import to: ", COUNT_FIRST_DAY_DISEASE_IMPORT, "\n")
cat("Infected agent ids: ")
cat(print(first_disease_import), "\n")

for(person_id in first_disease_import){


  # Sample based on proportions
  # infecting_variant <- sample(active_voc_df$variant, 1, prob = active_voc_df$Proportion/100)
  # [DONE] Migrate to matrix
  infecting_variant <- sample(active_voc_mat[ , "variant"], 1, prob = as.numeric(active_voc_mat[ , "Proportion"])/100)

  # Setting infection history
  infection_hist_mat[person_id, "variant"] <- toString(infecting_variant)
  infection_hist_mat[person_id, "infected on"] <- 1 # first day disease import

  if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Location based"){
    # Special values for first day disease import
    infection_hist_mat[person_id, "citisketch_location_type"] <- "UNKNOWN"
    infection_hist_mat[person_id, "citisketch_location_id"] <- -1
  }

  present_state = STATE[person_id , 1]
  infected_state = "UNKNOWN"

  # Tag: @cailb03, the infeceted states are foced to be "XX_pre_symptomatic_non_isolated_YY
  if (CONST_CHOSEN_DISEASE_MODEL == "Complex") {
    # If 0 vaccination dose
    if(present_state == which(colnames(transition_matrix) == "susceptible")){

      #Check if Omicron
      if(toString(infecting_variant) == "B.1.1.529"){
        # infected_state <- sample( c( which(colnames(transition_matrix) == "00_pre_symptomatic_non_isolated_omicron"),
        #                              which(colnames(transition_matrix) == "00_mild_non_isolated_omicron") ), 1)

        infected_state <- which(colnames(transition_matrix) == "00_pre_symptomatic_non_isolated_omicron")
      }
      # For all other Variants of Concern
      else {
        # infected_state <- sample( c( which(colnames(transition_matrix) == "00_pre_symptomatic_non_isolated"),
        #                              which(colnames(transition_matrix) == "00_mild_non_isolated") ), 1)

        infected_state <- which(colnames(transition_matrix) == "00_pre_symptomatic_non_isolated")
      }
    }

    # If 1 vaccination dose
    else if(present_state == which(colnames(transition_matrix) == "received_dose1")){

      #Check if Omicron
      if(toString(infecting_variant) == "B.1.1.529"){
        # infected_state <- sample( c( which(colnames(transition_matrix) == "01_pre_symptomatic_non_isolated_omicron"),
        #                              which(colnames(transition_matrix) == "01_mild_non_isolated_omicron") ), 1)

        infected_state <- which(colnames(transition_matrix) == "01_pre_symptomatic_non_isolated_omicron")
      }
      # For all other Variants of Concern
      else {
        # infected_state <- sample( c( which(colnames(transition_matrix) == "01_pre_symptomatic_non_isolated"),
        #                              which(colnames(transition_matrix) == "01_mild_non_isolated") ), 1)

        infected_state <- which(colnames(transition_matrix) == "01_pre_symptomatic_non_isolated")
      }
    }

    # If 2 vaccination dose
    else if(present_state == which(colnames(transition_matrix) == "received_dose2")){

      #Check if Omicron
      if(toString(infecting_variant) == "B.1.1.529"){
        # infected_state <- sample( c( which(colnames(transition_matrix) == "02_pre_symptomatic_non_isolated_omicron"),
        #                              which(colnames(transition_matrix) == "02_mild_non_isolated_omicron") ), 1)

        infected_state <- which(colnames(transition_matrix) == "02_pre_symptomatic_non_isolated_omicron")
      }
      # For all other Variants of Concern
      else {
        # infected_state <- sample( c( which(colnames(transition_matrix) == "02_pre_symptomatic_non_isolated"),
        #                              which(colnames(transition_matrix) == "02_mild_non_isolated") ), 1)

        infected_state <- which(colnames(transition_matrix) == "02_pre_symptomatic_non_isolated")
      }
    }

    # If 3 vaccination dose
    else if(present_state == which(colnames(transition_matrix) == "received_dose3")){

      #Check if Omicron
      if(toString(infecting_variant) == "B.1.1.529"){
        # infected_state <- sample( c( which(colnames(transition_matrix) == "03_pre_symptomatic_non_isolated_omicron"),
        #                              which(colnames(transition_matrix) == "03_mild_non_isolated_omicron") ), 1)

        infected_state <- which(colnames(transition_matrix) == "03_pre_symptomatic_non_isolated_omicron")
      }
      # For all other Variants of Concern
      else {
      #   infected_state <- sample( c( which(colnames(transition_matrix) == "03_pre_symptomatic_non_isolated"),
      #                                which(colnames(transition_matrix) == "03_mild_non_isolated") ), 1)

        infected_state <- which(colnames(transition_matrix) == "03_pre_symptomatic_non_isolated")
      }
    }
  } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR"){

    if(present_state == which(colnames(transition_matrix) == "susceptible")){
      infected_state <- which(colnames(transition_matrix) == "infected")

    }
  } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){

    if(present_state == which(colnames(transition_matrix) == "susceptible")){
      infected_state <- which(colnames(transition_matrix) == "(unconfirmed) infected")

    }
  } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){

    if(present_state == which(colnames(transition_matrix) == "susceptible")){
      infected_state <- which(colnames(transition_matrix) == "(unconfirmed) infected")

    }
  } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){

    if(present_state == which(colnames(transition_matrix) == "susceptible")){
      infected_state <- which(colnames(transition_matrix) == "(unconfirmed) infected")

    }
  } else {

    cat("\n [WARNING] Initial disease import has not been implemented for: ", CONST_CHOSEN_DISEASE_MODEL)
  }


  # Debug
  # cat("person id - to infect: ", person_id,
  #     "with variant: ", infection_hist_mat[person_id, "variant"],
  #     "present state: ", colnames(transition_matrix)[present_state], " -> ",
  #     "infected state: ", colnames(transition_matrix)[infected_state],
  #     "\n")

  # Check which variant to infect with
  STATE[person_id , 1] <- infected_state

 # Book keeping
  # day_1_variants_mat[toString(infecting_variant), "count"] <- day_1_variants_mat[toString(infecting_variant), "count"] + 1
  day_1_varaint_list <- append(day_1_varaint_list, toString(infecting_variant))


}

# print("Debug here")



# Getting vaccination data, @Vaccination Rates
CONST_NB_POP = 779993
CONST_MIN_DAYS_TILL_SECOND_DOSE = 28 #

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
    # TODO: DEBUG DEBUG!!!
    vaccination_perday_mat[i, "dose2"] <- vaccination_data_table_list[[4]][i] - vaccination_data_table_list[[4]][i -1]

    vaccination_perday_mat[i, "dose1"] <- (vaccination_data_table_list[[2]][i] - vaccination_data_table_list[[2]][i -1]) +
      vaccination_perday_mat[i, "dose2"]

    # Debug mode:
    # vaccination_perday_mat[i, "dose2"] <- CONST_NB_POP/10
    # vaccination_perday_mat[i, "dose1"] <- CONST_NB_POP/10
  }
}



STATELIST_ALL_INFECTIOUS <- union(STATELIST_INFECTIOUS, STATELIST_INFECTIOUS_OMICRON)

# Helper function to check if a particular state is infectious or not
isInfectiousState <- function(state_id){
  if(sum(sapply(STATELIST_ALL_INFECTIOUS, FUN = function(x) x == state_id)) == 0) { return (FALSE) }

  else { return (TRUE) }
}

# Helper function to check if a particular state is considered active case or not
isActiveCaseState <- function(state_id){
  if(sum(sapply(STATELIST_ACTIVE_CASES, FUN = function(x) x == state_id)) == 0) { return (FALSE) }

  else { return (TRUE) }
}

# Helper function to check if a particular state terminal active case or not
isTerminalState <- function(state_id){
  if(sum(sapply(STATELIST_TERMINAL, FUN = function(x) x == state_id)) == 0) { return (FALSE) }

  else { return (TRUE) }
}

# Tag: @calib10
# Test seeking states
# Helper function to check if a particular state is considered a symptomatic test seeker
isInfectiousSymptomaticTestSeeker <- function(state_id){
  if(sum(sapply(STATELIST_SYMPTOMATIC_INFECTIOUS_TEST_SEEKER, FUN = function(x) x == state_id)) == 0) { return (FALSE) }

  else { return (TRUE) }
}

# Helper function to check if a particular state is considered a asymptomatic test seeker
isInfectiousAsymptomaticTestSeeker <- function(state_id){
  if(sum(sapply(STATELIST_ASYMPTOMATIC_INFECTIOUS_TEST_SEEKER, FUN = function(x) x == state_id)) == 0) { return (FALSE) }

  else { return (TRUE) }
}

# Helper function to check if a particular state is considered a not infectious test seeker
isNotInfectedTestSeeker <- function(state_id){
  if(sum(sapply(STATELIST_NOT_INFECTED_TEST_SEEKER, FUN = function(x) x == state_id)) == 0) { return (FALSE) }

  else { return (TRUE) }
}

# Result seeking states
# Helper function to check if a particular state is considered a symptomatic result seeker
isInfectiousSymptomaticResulttSeeker <- function(state_id){
  if(sum(sapply(STATELIST_SYMPTOMATIC_INFECTIOUS_RESULT_SEEKER, FUN = function(x) x == state_id)) == 0) { return (FALSE) }

  else { return (TRUE) }
}

# Helper function to check if a particular state is considered a asymptomatic result seeker
isInfectiousAsymptomaticResulttSeeker <- function(state_id){
  if(sum(sapply(STATELIST_ASYMPTOMATIC_INFECTIOUS_RESULT_SEEKER, FUN = function(x) x == state_id)) == 0) { return (FALSE) }

  else { return (TRUE) }
}

# Helper function to check if a particular state is considered a not infectious result seeker
isNotInfectedResultSeeker <- function(state_id){
  if(sum(sapply(STATELIST_NOT_INFECTED_RESULT_SEEKER, FUN = function(x) x == state_id)) == 0) { return (FALSE) }

  else { return (TRUE) }
}

# Tag: @calib05
previous_day_susceptible_person_ids <- setdiff(seq(1:TOTAL_SIMULATED_PERSONS), first_disease_import)
previous_day_non_susceptible_person_ids <- first_disease_import

# Helper function to evolve non-susceptibles
evolveNonSusceptible <- function(person_id){

  returnState = NA # Default error value

  # Debug:
  # cat("\n      Beginning call of 'evolveNonSusceptible' with, person id: ", person_id, ", day_index: ", day_index, sep = "")
  # Get state of this person from previous day
  PREV_STATE_INDEX <- as.numeric(STATE[person_id, day_index - 1])
  PREV_STATE_NAME <- STATE_NAMES[PREV_STATE_INDEX] # Note: At this point STATE_NAME is a matrix, by the end of execution it's converted to df for CSV IO


  # Debug:
  # cat("\n          State id: ", PREV_STATE_INDEX, ", name: ", PREV_STATE_NAME, sep = "")

  # for no transitions
  next_state_id <- PREV_STATE_INDEX
  next_state_name <- PREV_STATE_NAME

  # Get possible transitions
  POSSIBLE_TRANSITIONS_mat_colnames <- c("state_id", "transition_weights")
  # POSSIBLE_TRANSITIONS_mat <- matrix( c(which(transition_matrix[PREV_STATE_INDEX, ] != ""),
  #                                       transition_matrix[PREV_STATE_INDEX, which(transition_matrix[PREV_STATE_INDEX, ] != "")]),
  #                                     ncol = 2)
  POSSIBLE_TRANSITIONS_mat <- matrix( c(which(transition_matrix[PREV_STATE_INDEX, ] > "0.0"),
                                        transition_matrix[PREV_STATE_INDEX, which(transition_matrix[PREV_STATE_INDEX, ] > "0.0")]),
                                      ncol = 2)

  colnames(POSSIBLE_TRANSITIONS_mat) <- POSSIBLE_TRANSITIONS_mat_colnames

  if(nrow(POSSIBLE_TRANSITIONS_mat) == 0){
    # Terminal state, don't sample for probabilities
    # cat("\n Terminal state w.r.t disease model: :", PREV_STATE_NAME)
  } else {

    # cat("\n Parametrised transitions for person: ", person_id, "in state: ", PREV_STATE_NAME, "\n")

    # There should not be any non numeric transition weights for this state
    # Coerce into double type

    STAY_IN_STATE_WEIGHT <- min(1.0, abs(1.0 - sum( as.double(POSSIBLE_TRANSITIONS_mat[ , "transition_weights"]) )))

    # Create temp loop-back state

    complete_state_ids <- append(as.numeric(POSSIBLE_TRANSITIONS_mat[ , "state_id"]), PREV_STATE_INDEX)
    complete_state_weights <- append(POSSIBLE_TRANSITIONS_mat[ , "transition_weights"], STAY_IN_STATE_WEIGHT)

    # Debug:
    # cat("\n            Possible transitions: ", complete_state_ids, sep = ", ")
    # cat("\n                           names: ", STATE_NAMES[complete_state_ids], sep = ", ")
    # cat("\n               and their weights: ", complete_state_weights, sep = ", ")


    # Debug:
    # cat("\n    About to sample a non-susceptible evolution::::...")
    # Choose a state w.r.t weight
    next_state_id <- sample(complete_state_ids, 1, prob = complete_state_weights)


    # cat("\n                 Sampled state: ", next_state_id, sep = "")
    # cat("\n                          name: ", STATE_NAMES[next_state_id], sep = "")

  }

  returnState = next_state_id

  return (returnState)
}

# Tag: @calib10
# Setting up a helper data structure for scheduling and reporting tests
# for both voluntary requests and via contact tracing
# Note: Row number is interpreted as person id
test_schedule_mat <- matrix (ncol = 4, nrow = TOTAL_SIMULATED_PERSONS)
test_schedule_mat_colnames <- c("test type", "test scheduled on", "result day", "result")
colnames(test_schedule_mat) <- test_schedule_mat_colnames

# Setting up (subset) of available tests
available_covid_tests_df <- filter(covid_test_df, `Max test capacity` > 0)
available_covid_tests_indices <- 1:nrow(available_covid_tests_df)
available_covid_test_types <- available_covid_tests_df$Type
available_covid_test_result_delays <- available_covid_tests_df$`Result delay`

# Assuming only two types of test
# Note: Need to change when introducing Covid test brand specific sensitivity and specificity (confusion matrices)
available_antigen_test_index <- which(available_covid_test_types == "antigen")
available_molecular_test_index <- which(available_covid_test_types == "rapid molecular")

symptomatic_antigen_TPR <- available_covid_tests_df$`Symptomatic TPR`[ available_antigen_test_index ]
symptomatic_antigen_FNR <- available_covid_tests_df$`Symptomatic FNR`[ available_antigen_test_index ]
asymptomatic_antigen_TPR <- available_covid_tests_df$`Asymptomatic TPR`[ available_antigen_test_index ]
asymptomatic_antigen_FNR <- available_covid_tests_df$`Asymptomatic FNR`[ available_antigen_test_index ]

molecular_TPR <- available_covid_tests_df$`Symptomatic TPR`[ available_molecular_test_index ]
molecular_FNR <- available_covid_tests_df$`Symptomatic FNR`[ available_molecular_test_index ]


asymptomatic_antigen_TNR <- available_covid_tests_df$`Asymptomatic TNR`[ available_antigen_test_index ]
asymptomatic_antigen_FPR <- available_covid_tests_df$`Asymptomatic FPR`[ available_antigen_test_index ]

molecular_TNR <- available_covid_tests_df$`Symptomatic TNR`[ available_molecular_test_index ]
molecular_FPR <- available_covid_tests_df$`Symptomatic FPR`[ available_molecular_test_index ]

# Setup test positivity logging for models with testing
if( (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation") ||
    (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") ||
    (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing")){

  COVID_TEST_STATS_mat <- matrix (ncol = 4, nrow = TOTAL_SIMULATION_DAYS)
  COVID_TEST_STATS_mat_colnames <- c("total result received", "positive result", "negative result", "test positivity")
  colnames(COVID_TEST_STATS_mat) <- COVID_TEST_STATS_mat_colnames

}

# Tag: @calib12
# Standardising simulation day to contact matrix id mapping
# Note this may utilised to shuffle the order
getContactMatrixIndexForSimDay <- function(simulation_day_index){
  if( simulation_day_index <= 1 ){
    cat("\n[FATAL ERROR] Requested simulation day index: ", simulation_day_index, " is out of bounds.",
        "\n              Expected range (2:", TOTAL_SIMULATION_DAYS, ").",
        "\n Terminating simulation")
    stop()
  } else if( (simulation_day_index) <= COUNT_CONTACT_MATRIX + 1){
    return (simulation_day_index - 1)
  }

  # else
  return( ( (simulation_day_index - 2) %% COUNT_CONTACT_MATRIX) + 1)
}

contact_matrix_index_lookup_list = sapply(2:TOTAL_SIMULATION_DAYS, getContactMatrixIndexForSimDay)
contact_matrix_index_lookup_list <- c(NA, contact_matrix_index_lookup_list)
# Tag: @calib11

# Tracing helper function, dependent on pre-processing of contacts and
# contact tracing parameters
contactTrace <- function( traced_agent_id, tracing_day_index ){
  
  # Guard clause 
  # Note: We do not know the contacts for day index 0, 
  #       And day_index 1 contacts may only be available after computing infections for day_index 1,
  #       which completes at the end of day_index 2
  if(tracing_day_index <= 1){
    return(c())
  }

  # tracing window
  tracing_end_day <- min(tracing_day_index, TOTAL_SIMULATION_DAYS)
  tracing_beg_day <- min( max(2, tracing_day_index - (CONST_CONTACT_TRACING_TIME_WINDOW - 1)), TOTAL_SIMULATION_DAYS)
  tracing_window_days <- tracing_beg_day:tracing_end_day
  
  # Debug: 
  # cat(" Tracing window: ", tracing_window_days, "\n")
  
  # select contact matrix indices
  contact_matrix_index_list <- contact_matrix_index_lookup_list[tracing_window_days]

  # traced_aggregated_contacts_mat <- matrix(ncol = tracing_end_day - tracing_beg_day + 1, nrow = TOTAL_SIMULATED_PERSONS)
  # traced_aggregated_contacts_mat_colnames <- paste(tracing_beg_day:tracing_end_day)
  # colnames(traced_aggregated_contacts_mat) <- traced_aggregated_contacts_mat_colnames

  traced_list_of_contact_ids <- c()
  traced_list_of_contact_times <- c()
  traced_list_of_contact_location_types <- c()
  traced_list_of_contact_location_ids <- c()

  # Debug:
  # cat("\n Tracing agent: ", traced_agent_id, sep = "")
  # cat("\n Tracing window: ", tracing_window_days)
  # cat("\n Contact matrix indices: ", contact_matrix_index_list)


  # get all contact ids & # get all contact times
  for(trace_index in 1:length(tracing_window_days) ){
    tracing_sim_day <- tracing_window_days[trace_index]
    contact_matrix_index <- contact_matrix_index_list[trace_index]

    # cat("\n  |- Tracing day: ", tracing_sim_day, sep = "")


    if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Aggregated"){


      # Load contact time including time spent in event with events during event duration
      if(GENERATE_EVENT_CONTACT_PAIRS && tracing_sim_day >= EVENT_START_DATE && tracing_sim_day <= EVENT_END_DATE){
        # Get all contact ids
        traced_contact_ids_for_this_day <-  list_day_x_person_x_contact_w_event[[contact_matrix_index]][[traced_agent_id]]

        # Get all person-specific contact times
        traced_list_of_contact_times_for_this_day <- list_of_contact_matrix_w_event[[contact_matrix_index]][traced_agent_id, traced_contact_ids_for_this_day]

        # Aggregate
        traced_list_of_contact_ids <- c(traced_list_of_contact_ids, traced_contact_ids_for_this_day)
        traced_list_of_contact_times <- c(traced_list_of_contact_times, traced_list_of_contact_times_for_this_day)


      } else {
        # Get all contact ids
        traced_contact_ids_for_this_day <-  list_day_x_person_x_contact[[contact_matrix_index]][[traced_agent_id]]

        # Get all person-specific contact times
        traced_list_of_contact_times_for_this_day <- list_of_contact_matrix[[contact_matrix_index]][traced_agent_id, traced_contact_ids_for_this_day]

        # Aggregate
        traced_list_of_contact_ids <- c(traced_list_of_contact_ids, traced_contact_ids_for_this_day)
        traced_list_of_contact_times <- c(traced_list_of_contact_times, traced_list_of_contact_times_for_this_day)

      }
      # cat("\n  |- Contacts found on this day: ", length(list_day_x_person_x_contact[[contact_matrix_index]][[traced_agent_id]]) , sep = "")
      # cat("\n  '- Total contacts found: ", length(traced_list_of_contact_ids), "\n", sep = "" )


    } else if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Location based"){

      # Note, this is where may access the location type as well
      # list_of_contact_times <- contact_matrix_as_pairlist[
      #   which(contact_matrix_as_pairlist[
      #     which(contact_matrix_as_pairlist$person_id_1 == person_id), ]$person_id_2 == contact_index), ]$contact_in_seconds

      # Tag: @calib08
      # Load contact time including time spent in event with events during event duration
      if( GENERATE_EVENT_CONTACT_PAIRS && tracing_sim_day >= EVENT_START_DATE && tracing_sim_day <= EVENT_END_DATE){

        # Get all contact ids
        traced_list_of_contact_ids <- c(traced_list_of_contact_ids, list_day_x_person_x_contact_w_event[[contact_matrix_index]][[traced_agent_id]])


        # Get all person-specific contact times
        traced_list_of_contact_times <- c(traced_list_of_contact_times, list_day_x_person_x_contact_time_w_event[[contact_matrix_index]][[traced_agent_id]])


        # Get all person-specific contact location types
        traced_list_of_contact_location_types <- c(traced_list_of_contact_location_types, list_day_x_person_x_contact_location_type_w_event[[contact_matrix_index]][[traced_agent_id]])


        # Get all person-specific contact location ids
        traced_list_of_contact_location_ids <- c(traced_list_of_contact_location_ids, list_day_x_person_x_contact_location_id_w_event[[contact_matrix_index]][[traced_agent_id]])


      } else {

        # Get all contact ids
        traced_list_of_contact_ids <- c(traced_list_of_contact_ids, list_day_x_person_x_contact[[contact_matrix_index]][[traced_agent_id]])

        # Get all person-specific contact times
        traced_list_of_contact_times <- c(traced_list_of_contact_times, list_day_x_person_x_contact_time[[contact_matrix_index]][[traced_agent_id]])


        # Get all person-specific contact location types
        traced_list_of_contact_location_types <- c(traced_list_of_contact_location_types, list_day_x_person_x_contact_location_type[[contact_matrix_index]][[traced_agent_id]])


        # Get all person-specific contact location ids
        traced_list_of_contact_location_ids <- c(traced_list_of_contact_location_ids, list_day_x_person_x_contact_location_id[[contact_matrix_index]][[traced_agent_id]])

      }
    }
  }

  # split remaining contacts into close or casual contacts
  # Naive split based on a arbitrary constant
  #  - any shared contact time less than or equal to 4 hours is considered "casual contact"
  CONST_ARBITRARY_CASUAL_SPLIT_CONTACT_TIME = 4 * 60 * 60

  all_casual_contact_indexes <- which(traced_list_of_contact_times <= CONST_ARBITRARY_CASUAL_SPLIT_CONTACT_TIME)
  all_close_contact_indexes <- which(traced_list_of_contact_times > CONST_ARBITRARY_CASUAL_SPLIT_CONTACT_TIME)

  # list_of_all_contact_types <- rep(NA,length(traced_list_of_contact_ids))
  # list_of_all_contact_types[ all_casual_contact_indexes ] <- "casual"
  # list_of_all_contact_types[ all_close_contact_indexes ] <- "close"

  traced_close_contacts_df <- data.frame(agent_id = traced_list_of_contact_ids[all_close_contact_indexes], contact_time = traced_list_of_contact_times[all_close_contact_indexes])
  traced_casual_contacts_df <- data.frame(agent_id = traced_list_of_contact_ids[all_casual_contact_indexes], contact_time = traced_list_of_contact_times[all_casual_contact_indexes])

  # filter out non-viable contacts
  # - Yet to implement

  # order close contact by contact_time
  if( length(traced_close_contacts_df) > 0){
    # Guard clause
    traced_close_contacts_df <- traced_close_contacts_df[order(traced_close_contacts_df$contact_time, decreasing = TRUE),]
  }

  # get unique contacts
  unique_close_contact_ids <- unique(traced_close_contacts_df$agent_id)
  unique_casual_contact_ids <- unique(traced_casual_contacts_df$agent_id)

  count_close_contacts <- length(unique_close_contact_ids)
  count_casual_contacts <- length(unique_casual_contact_ids)

  traced_close_contact_ids <- c()
  traced_casual_contact_ids <- c()

  # Reduce to magic numbers
  if(count_close_contacts >= CONST_CHOSEN_CLOSE_CONTACT_NUMBER){
    # Get the Close contacts as top CONST_CHOSEN_CLOSE_CONTACT_NUMBER
    traced_close_contact_ids <- unique_close_contact_ids[1:CONST_CHOSEN_CLOSE_CONTACT_NUMBER]
  } else {
    # Get as many close contacts as possible
    traced_close_contact_ids <- unique_close_contact_ids
  }

  # Randomly sample CONST_CHOSEN_CASUAL_CONTACT_NUMBER of casual contact
  if(count_casual_contacts >= CONST_CHOSEN_CASUAL_CONTACT_NUMBER){
    traced_casual_contact_ids <- sample(unique_casual_contact_ids, CONST_CHOSEN_CASUAL_CONTACT_NUMBER)
  } else {
    # Get as many casual contacts as possible
    traced_casual_contact_ids <- unique_casual_contact_ids
  }

  sample_count_traced_close_contacts <- floor(length(traced_close_contact_ids) * CONST_CONTACT_TRACING_COVERAGE_CLOSE_CONTACTS)
  sample_count_traced_casual_contacts <- floor(length(traced_casual_contact_ids) * CONST_CONTACT_TRACING_COVERAGE_CASUAL_CONTACTS)

  sampled_traced_close_contact_ids <- c()
  # Apply tracing coverage
  if(sample_count_traced_close_contacts > 0){
    sampled_traced_close_contact_ids <- sample(traced_close_contact_ids, sample_count_traced_close_contacts )
  } else if (sample_count_traced_close_contacts == 0){
    # Do nothing
  } else {
    cat("\n [WARNING] Attempting to sample negative count of traced close contacts: ", sample_count_traced_close_contacts)
  }

  sampled_traced_casual_contact_ids <- c()
  if (sample_count_traced_casual_contacts > 0){
    sampled_traced_casual_contact_ids <- sample(traced_casual_contact_ids, sample_count_traced_casual_contacts)
  } else if (sample_count_traced_casual_contacts == 0){
    # Do nothing
  } else {
    cat("\n [WARNING] Attempting to sample negative count of traced casual contacts: ", sample_count_traced_casual_contacts)
  }


  return_contact_id_list <- c(sampled_traced_close_contact_ids, sampled_traced_casual_contact_ids)

  return(return_contact_id_list)

  # return a table
  # return_contact_type_list <- c( rep("close", length(sampled_traced_close_contact_ids)), rep("casual", length(sampled_traced_casual_contact_ids)))
  # contact agent id | contact type: <close | casual>
  # return_traced_contacts_df <- data.frame(agent_id = return_contact_id_list, contact_type = return_contact_type_list)
  # return(return_traced_contacts_df)
}

# Debug:
# traced_contact_df <- contactTrace(1, 10)
# traced_contact_ids <- contactTrace(1, 10)

# Tag: @calib13
# Helper function for batch contact tracing
batchContactTrace <- function(all_positive_test_result_ids, day_index){
  # Count all positive results
  count_of_all_positive_test_result_ids <- length(all_positive_test_result_ids)

  # Get all neighbours
  all_contact_traced_neighbour_agent_ids <- c()

  if(count_of_all_positive_test_result_ids > 1){

    # Complex call for multiple neighbours
    temp_result <- mapply(contactTrace, all_positive_test_result_ids, rep( day_index,  count_of_all_positive_test_result_ids) )

    # Linearise to a 1-D vector
    if(is.matrix(temp_result)) {
      all_contact_traced_neighbour_agent_ids <- unique(as.vector( temp_result ) )
    } else if(is.list(temp_result)){

      # Check if non-empty list
      if(length(temp_result) > 0){

        # Check if list of list
        if(is.list(temp_result[1])){

          # Linearise to a 1-D vector
          for (mini_list in temp_result) { all_contact_traced_neighbour_agent_ids <- c(all_contact_traced_neighbour_agent_ids, mini_list)  }
        }
      }
    } else {
      cat("\n [WARNING] Contact tracing call is returning an unknown data structure: ", class(temp_result))
    }

  } else if(count_of_all_positive_test_result_ids == 1){

    # Simplified call for single neighbour
    all_contact_traced_neighbour_agent_ids <- contactTrace(all_positive_test_result_ids, day_index)

  }

  return(unique(all_contact_traced_neighbour_agent_ids))
}

sim_setup_time <- proc.time() - start_time
cat("\n Sim-setup time for preloading no. of. contact-matrices:  ", COUNT_CONTACT_MATRIX, " and observed vaccination rates for: ", TOTAL_DAYS_OF_OBSERVATION, " days", sep = "")
cat("\n", sim_setup_time)
cat("\n")

for(run_index in 1:TOTAL_SIMULATION_RUNS){
  # Re-Start the clock!
  sim_time <- proc.time()

  # Set up infected list for 2nd simulation
  previous_day_infectious_person_ids <- first_disease_import
  # Alternatively, we could also use
  # previous_day_infectious_person_ids <- which(sapply(STATE[ , 1], FUN = isInfectiousState) == TRUE)

  # Simulate from day 2 to TOTAL_SIMULATION_DAYS
  for(day_index in 2:TOTAL_SIMULATION_DAYS){

    moved_to_non_susceptible_today <- c()
    moved_to_susceptible_today <- c()

    # Debug:
    # cat("\n")
    # cat("  Sim day: ", day_index, sep = "")
    # cat("\n")

    # previous_day_infectious_person_ids <- which(sapply(STATE[ , day_index - 1], FUN = isInfectiousState) == TRUE)

    COUNT_NEW_INFECTIONS_TODAY <- 0 # flush the counter
    COUNT_NEW_INFECTIONS_YESTERDAY <- 0 # flush the counter (needed for testing based positive results)

    # Tag: @calib10
    # Reset available test's count and chances at the start of day
    # TODO: Decide whether tracing reduces the test capacity for this day here

    previous_day_df <- data.frame(STATE[ , day_index - 1])
    colnames(previous_day_df) <- c("state_id")

    {

      available_covid_tests_count <- available_covid_tests_df$`Max test capacity`
      sum_available_covid_tests_count <- sum(available_covid_tests_count)

      available_covid_test_chances <- available_covid_tests_count/sum_available_covid_tests_count
    }

    # Transitions for both susceptibles and non-susceptibles
    {
      ITERATE_OVER_SUSCEPTIBLE <- FALSE
      # ITERATE_OVER_SUSCEPTIBLE <- TRUE
      # ITERATE_OVER_SUSCEPTIBLE <- length(previous_day_infectious_person_ids) > CONST_INFECTIOUS_COUNT_SWITCH
      # Check neighbours of susceptibles for infectious contacts
      if(ITERATE_OVER_SUSCEPTIBLE){

        # cat("   \n Iterating on Susceptibles")

        # Tag: @Vaccination Rates "Did anyone get vaccinated yesterday?"
        if (CONST_CHOSEN_DISEASE_MODEL == "Complex"){
          for (person_id in previous_day_susceptible_person_ids){

            # Get state of this person from previous day
            PREV_STATE_INDEX <- as.numeric(STATE[person_id, day_index - 1])
            PREV_STATE_NAME <- STATE_NAMES[PREV_STATE_INDEX] # Note: At this point STATE_NAME is a matrix, by the end of execution it's converted to df for CSV IO

            # Vaccination state changes during simulation time
            # -------------------
            # Check if the person may have been vaccinated last simulation day
            # i.e. is in succeptible or received_dose_1 state
            # This breaks new evolution function: NO_OF_VACCINATION_DATA_BASED_TRANSITION <- length(which( POSSIBLE_TRANSITIONS_mat[ , "transition_weights"] == "CALCULATE_FROM_VACCINATION_DATA"))

            this_vaccination_chance = runif(1)

            if(PREV_STATE_NAME == "susceptible" || PREV_STATE_NAME == "received_dose1"){

              # Get vaccination data and roll dice for possible vaccination last day
              vaccination_day_index <- day_index - 1

              # Re-use last available vaccination rates
              if(vaccination_day_index > nrow(vaccination_perday_mat)) {
                vaccination_day_index <- nrow(vaccination_perday_mat)
              }

              if(PREV_STATE_NAME == "susceptible" &&
                 vaccination_perday_mat[vaccination_day_index , "dose1"] > 0){

                dose1_prob <- vaccination_perday_mat[vaccination_day_index , "dose1"] / CONST_NB_POP

                # Force negative fractions to 0, to counteract artifacts in the data
                if(dose1_prob < 0) { dose1_prob = 0 }

                else if( this_vaccination_chance <= dose1_prob){
                  prev_state_id <- which( colnames(transition_matrix) == "received_dose1" )

                  # Set state
                  STATE[person_id, day_index - 1] <- prev_state_id

                  # Book keeping
                  infection_hist_mat[person_id, "dose 1 on"] <- day_index - 1
                }
              }

              if(PREV_STATE_NAME == "received_dose1" &&
                 vaccination_perday_mat[vaccination_day_index , "dose2"] > 0 &&
                 (day_index - strtoi(infection_hist_mat[[person_id, "dose 1 on"]])) >= CONST_MIN_DAYS_TILL_SECOND_DOSE){

                dose2_prob <- vaccination_perday_mat[vaccination_day_index , "dose2"] / CONST_NB_POP

                # Force negative fractions to 0, to counteract artifacts in the data
                if(dose2_prob < 0) { dose2_prob = 0 }

                else if( this_vaccination_chance <= dose2_prob){
                  prev_state_id <- which( colnames(transition_matrix) == "received_dose2" )

                  # Set state
                  STATE[person_id, day_index - 1] <- prev_state_id

                  # Book keeping
                  infection_hist_mat[person_id, "dose 2 on"] <- day_index - 1
                }
              }


              # Reload states
              # PREV_STATE_INDEX <- as.numeric(STATE[person_id, day_index - 1])
              # PREV_STATE_NAME <- STATE_NAMES[PREV_STATE_INDEX]

            } # End of last days vaccination prob
          }
        }


        # Tag: @calib10
        # For models: "SIR + Testing + Isolation", "SIR + Testing + Isolation + Contact Tracing"
        # Queue voluntary tests: "Did anyone request test yesterday?"
        if ( (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation") ||
             (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") ||
             (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing")){
          
          # Generate voluntary test request from: Infectious and symptomatic agents
          # Note: for other models need to implement statelist (STATELIST_SYMPTOMATIC_INFECTIOUS_TEST_SEEKER) based helper functions
          
          list_symptomatic_infectious_test_seeker_ids <- previous_day_infectious_person_ids
          count_total_seeker <- length(list_symptomatic_infectious_test_seeker_ids)
          # Draw which of the agents requests for test
          sampled_indices_of_test_seeker_ids <- which(runif(count_total_seeker) < rep(CONST_SYMPTOMATIC_INFECTIOUS_TEST_CHANCE, count_total_seeker))
          # Set as empty list
          list_symptomatic_infectious_test_seeker_ids_yesterday <- c()
          
          # Populate based on sampling
          if(length(sampled_indices_of_test_seeker_ids) > 0){
            list_symptomatic_infectious_test_seeker_ids_yesterday <- list_symptomatic_infectious_test_seeker_ids[sampled_indices_of_test_seeker_ids]
            
            # Debug: 
            # cat("\n Sampled symptomatic test sekers: ", length(sampled_indices_of_test_seeker_ids), "/", length(previous_day_infectious_person_ids), " : ", length(sampled_indices_of_test_seeker_ids) / length(previous_day_infectious_person_ids))
          }
          
          
          # Generate voluntary test request from: Asymptomatic agents
          # Note: for other models need to implement statelist (STATELIST_ASYMPTOMATIC_INFECTIOUS_TEST_SEEKER, STATELIST_NOT_INFECTED_TEST_SEEKER) based helper functions
          # Note: for vaccination based model need to exclude agents who were vaccinated yesterday from STATELIST_NOT_INFECTED_TEST_SEEKER
          list_asymptomatic_test_seeker_ids <- previous_day_susceptible_person_ids
          count_total_seeker <- length(list_asymptomatic_test_seeker_ids)
          # Draw which of the agents requests for test
          sampled_indices_of_test_seeker_ids <- which(runif(count_total_seeker) < rep(CONST_ASYMPTOMATIC_TEST_CHANCE, count_total_seeker))
          # Set as empty list
          list_asymptomatic_test_seeker_ids_yesterday <- c()
          
          # Populate based on sampling
          if(length(sampled_indices_of_test_seeker_ids) > 0){
            list_asymptomatic_test_seeker_ids_yesterday <- list_asymptomatic_test_seeker_ids[sampled_indices_of_test_seeker_ids]
          }
          
          if(CONST_CHOSEN_TEST_ALLOCATION_STRATEGY == "Antigen test first"){
            
            # Default value:
            count_available_antigen_test <- 0
            count_available_molecular_test <- 0
            # cat("\n Debug here!!!")
            
            if(!identical(available_antigen_test_index, integer(0))){
              count_available_antigen_test <- available_covid_tests_count[ available_antigen_test_index ]
            }
            
            if(!identical(available_molecular_test_index, integer(0))){
              count_available_molecular_test <- available_covid_tests_count[ available_molecular_test_index  ]
            }
            
            all_covid_test_requesting_ids_yesterday <- union(list_symptomatic_infectious_test_seeker_ids_yesterday, list_asymptomatic_test_seeker_ids_yesterday)
            total_requested_covid_test <- length(all_covid_test_requesting_ids_yesterday)
            
            # Case 1, 2: No. of antigen tests are equal or greater than requested no. of covid test
            if(count_available_antigen_test >= total_requested_covid_test){
              test_schedule_mat[all_covid_test_requesting_ids_yesterday, "test type"] <- "antigen"
              test_schedule_mat[all_covid_test_requesting_ids_yesterday, "test scheduled on"] <- day_index - 1
              test_schedule_mat[all_covid_test_requesting_ids_yesterday, "result day"] <- day_index - 1 + floor(available_covid_test_result_delays[available_antigen_test_index])
              
              # Book-keeping:
              available_covid_tests_count[ available_antigen_test_index ] <- available_covid_tests_count[ available_antigen_test_index ] - total_requested_covid_test
              
            } else{
              # Case 3: No. of antigen tests are less than requested no. of covid test
              
              # Case 3.1, 3.2: No. of molecular test are equal or greater than difference
              count_overflow_antigent_test <- total_requested_covid_test - count_available_antigen_test
              if(count_available_molecular_test >= (count_overflow_antigent_test)){
                
                if(count_available_antigen_test >= 1){
                  # Schedule as many antigen test possible
                  test_schedule_mat[all_covid_test_requesting_ids_yesterday[1:count_available_antigen_test], "test type"] <- "antigen"
                  test_schedule_mat[all_covid_test_requesting_ids_yesterday[1:count_available_antigen_test], "test scheduled on"] <- day_index -1
                  test_schedule_mat[all_covid_test_requesting_ids_yesterday[1:count_available_antigen_test], "result day"] <- day_index - 1 + floor(available_covid_test_result_delays[available_antigen_test_index])
                  # Book-keeping:
                  available_covid_tests_count[ available_antigen_test_index ] <- 0 #available_covid_tests_count[ available_antigen_test_index ] - count_available_antigen_test
                }
                
                # Schedule the rest as molecular test
                test_schedule_mat[all_covid_test_requesting_ids_yesterday[(count_available_antigen_test + 1):length(all_covid_test_requesting_ids_yesterday)], "test type"] <- "rapid molecular"
                test_schedule_mat[all_covid_test_requesting_ids_yesterday[(count_available_antigen_test + 1):length(all_covid_test_requesting_ids_yesterday)], "test scheduled on"] <- day_index - 1
                test_schedule_mat[all_covid_test_requesting_ids_yesterday[(count_available_antigen_test + 1):length(all_covid_test_requesting_ids_yesterday)], "result day"] <- day_index - 1 + floor(available_covid_test_result_delays[available_molecular_test_index])
                # Book-keeping:
                available_covid_tests_count[ available_molecular_test_index ] <- available_covid_tests_count[ available_molecular_test_index ] - count_overflow_antigent_test
                
              } else{
                # Case 3.3: No. of molecular tests is less than the difference
                # Randomly choose from test requests up to cumulative available total no. tests
                # i.e. assuming voluntary testing capacity is capped, in other words some agents may not be able to test due to lack of availability
                # Not scheduling for the next day or subsequent days
                sampled_covid_test_requesting_ids_yesterday <- sample(all_covid_test_requesting_ids_yesterday, sum_available_covid_tests_count)
                
                # Schedule tests
                if(count_available_antigen_test >= 1){
                  # Schedule as many antigen test possible
                  test_schedule_mat[sampled_covid_test_requesting_ids_yesterday[1:count_available_antigen_test], "test type"] <- "antigen"
                  test_schedule_mat[sampled_covid_test_requesting_ids_yesterday[1:count_available_antigen_test], "test scheduled on"] <- day_index - 1
                  test_schedule_mat[sampled_covid_test_requesting_ids_yesterday[1:count_available_antigen_test], "result day"] <- day_index - 1 + floor(available_covid_test_result_delays[available_antigen_test_index])
                  # Book-keeping:
                  available_covid_tests_count[ available_antigen_test_index ] <- 0 #available_covid_tests_count[ available_antigen_test_index ] - count_available_antigen_test
                }
                # Schedule the rest as molecular test
                test_schedule_mat[sampled_covid_test_requesting_ids_yesterday[(count_available_antigen_test + 1):length(sampled_covid_test_requesting_ids_yesterday)], "test type"] <- "rapid molecular"
                test_schedule_mat[sampled_covid_test_requesting_ids_yesterday[(count_available_antigen_test + 1):length(sampled_covid_test_requesting_ids_yesterday)], "test scheduled on"] <- day_index - 1
                test_schedule_mat[sampled_covid_test_requesting_ids_yesterday[(count_available_antigen_test + 1):length(sampled_covid_test_requesting_ids_yesterday)], "result day"] <- day_index - 1 + floor(available_covid_test_result_delays[available_molecular_test_index])
                # Book-keeping:
                available_covid_tests_count[ available_molecular_test_index ] <- 0 #available_covid_tests_count[ available_molecular_test_index ] - count_overflow_antigent_test
                
              }
              
            }
            
          }
          
          # Apply "Waiting for result" State changes:
          test_requested_yesterday_ids <- which(test_schedule_mat[ , "test scheduled on"] == day_index - 1)
          
          # Filter states only for test requesting ids
          previous_day_test_requested_df <- subset(previous_day_df, as.numeric(rownames(previous_day_df)) %in% test_requested_yesterday_ids)
          
          # Filter out not infected test seekers
          test_requested_yesterday_not_infected_ids <- as.numeric(rownames(subset(previous_day_test_requested_df, state_id %in% STATELIST_NOT_INFECTED_TEST_SEEKER)))
          
          if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){
            # Nothing to do
          } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") {
            # For SIR+++ process multiple non-infected test-seeker states, namely: susceptible and removed
            
            test_requested_yesterday_susceptible_ids <- as.numeric(rownames(subset(previous_day_test_requested_df, state_id == which(STATE_NAMES == "susceptible"))))
            test_requested_yesterday_removed_ids <- as.numeric(rownames(subset(previous_day_test_requested_df, state_id == which(STATE_NAMES == "removed"))))
          } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {
            # For augmented SIR with separate recovered and dead compartments,
            # process multiple non-infected test-seeker states, namely: susceptible and recovered
            
            test_requested_yesterday_susceptible_ids <- as.numeric(rownames(subset(previous_day_test_requested_df, state_id == which(STATE_NAMES == "susceptible"))))
            test_requested_yesterday_recovered_ids <- as.numeric(rownames(subset(previous_day_test_requested_df, state_id == which(STATE_NAMES == "recovered"))))
          }
          # Filter out symptomatic infectious test seekers
          test_requested_yesterday_symptomatic_infectious_ids <- as.numeric(rownames(subset(previous_day_test_requested_df, state_id %in% STATELIST_SYMPTOMATIC_INFECTIOUS_TEST_SEEKER)))
          # Filter out asymptomatic infectious test seekers
          test_requested_yesterday_asymptomatic_infectious_ids <- as.numeric(rownames(subset(previous_day_test_requested_df, state_id %in% STATELIST_ASYMPTOMATIC_INFECTIOUS_TEST_SEEKER)))
          
          
          # inconsistent_tesk_seeker_ids <- setdiff(test_requested_yesterday_ids, test_requested_yesterday_not_infected_ids)
          # inconsistent_tesk_seeker_ids <- setdiff(inconsistent_tesk_seeker_ids, test_requested_yesterday_symptomatic_infectious_ids)
          # inconsistent_tesk_seeker_ids <- setdiff(inconsistent_tesk_seeker_ids, test_requested_yesterday_asymptomatic_infectious_ids)
          #
          # if(length(inconsistent_tesk_seeker_ids) > 0) {
          #   cat("\n [WARNING] The following test seeking agents were dropped: ", inconsistent_tesk_seeker_ids)
          # }
          
          
          
          # Set "waiting for result" state retroactively
          if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){
            # For SIR++, the only non infected test seeker state is "susceptible"
            STATE[ test_requested_yesterday_not_infected_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) susceptible")
            # For SIR++, the infected test seeker state is "infected"
            STATE[ test_requested_yesterday_symptomatic_infectious_ids , day_index - 1] <- which(STATE_NAMES == "(isolated + unconfirmed) infected")
            # For SIR++, there are no asymptomatic test seekers only non infected test seeker state is "susceptible"
          } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){
            
            # For SIR+++ process multiple non-infected test-seeker states, namely: susceptible and removed
            STATE[ test_requested_yesterday_susceptible_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) susceptible")
            STATE[ test_requested_yesterday_removed_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) removed")
            
            # For SIR+++, the infected test seeker state is "infected"
            STATE[ test_requested_yesterday_symptomatic_infectious_ids , day_index - 1] <- which(STATE_NAMES == "(isolated + unconfirmed) infected")
            
            # For SIR+++, there are no asymptomatic and infectious test seekers
          } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){
            
            # For augmented SIR with separate recovered and dead compartments,
            # multiple non-infected test-seeker states, namely: susceptible and recovered
            STATE[ test_requested_yesterday_susceptible_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) susceptible")
            STATE[ test_requested_yesterday_recovered_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) recovered")
            
            # For SIR+++, the infected test seeker state is "infected"
            STATE[ test_requested_yesterday_symptomatic_infectious_ids , day_index - 1] <- which(STATE_NAMES == "(isolated + unconfirmed) infected")
            
            # For SIR+++, there are no asymptomatic and infectious test seekers
          }
          
          
          
          # Change infectious and susceptible sets (as per disease propagation model)
          
          if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){
            # Debug:
            # cat("\n----------Moving from susceptible to non-susceptible at Test Request: ", length(test_requested_yesterday_not_infected_ids) )
            # "Susceptible agents waiting for covid test result isolate themselves (and do not participate in disease propagation)
            previous_day_susceptible_person_ids <- setdiff(previous_day_susceptible_person_ids, test_requested_yesterday_not_infected_ids)
            previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, test_requested_yesterday_not_infected_ids)
            
            
          } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){
            previous_day_susceptible_person_ids <- setdiff(previous_day_susceptible_person_ids, test_requested_yesterday_susceptible_ids)
            previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, test_requested_yesterday_susceptible_ids)
            
            
          } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){
            previous_day_susceptible_person_ids <- setdiff(previous_day_susceptible_person_ids, test_requested_yesterday_susceptible_ids)
            previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, test_requested_yesterday_susceptible_ids)
          }
          
          # "Symptomatic Infectious agents waiting for covid test result isolate themselves (and do not participate in disease propagation)
          previous_day_infectious_person_ids <- setdiff(previous_day_infectious_person_ids, test_requested_yesterday_symptomatic_infectious_ids)
          
          # "Asymptomatic Infectious agents waiting for covid test result isolate themselves (and do not participate in disease propagation)
          previous_day_infectious_person_ids <- setdiff(previous_day_infectious_person_ids, test_requested_yesterday_asymptomatic_infectious_ids)
          
          
          
          # Update previous_day_df
          previous_day_df <- data.frame(STATE[ , day_index - 1])
          colnames(previous_day_df) <- c("state_id")
        }
        
        
        
        
        
        
        
        
        # Tag: @calib10
        # For models: "SIR + Testing + Isolation", "SIR + Testing + Isolation + Contact Tracing"
        # Generate test result scheduled: "Did anyone receive test result yesterday?"
        if ( (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation") ||
             (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") ||
             (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing")){
          result_requested_yesterday_ids <- which(test_schedule_mat[ , "result day"] == day_index - 1)
          
          COUNT_TOTAL_TEST_RESULTS_REQUESTED = length(result_requested_yesterday_ids)
          
          # Filter states only for result requesting ids
          previous_day_result_requested_df <- subset(previous_day_df, as.numeric(rownames(previous_day_df)) %in% result_requested_yesterday_ids)
          
          
          # Filter out not infected result seekers
          result_requested_yesterday_not_infected_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id %in% STATELIST_NOT_INFECTED_RESULT_SEEKER)))
          antigen_result_requested_yesterday_not_infected_ids <- result_requested_yesterday_not_infected_ids[ test_schedule_mat[result_requested_yesterday_not_infected_ids, "test type"] == "antigen" ]
          molecular_result_requested_yesterday_not_infected_ids <- result_requested_yesterday_not_infected_ids[ test_schedule_mat[result_requested_yesterday_not_infected_ids, "test type"] == "rapid molecular" ]
          if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){
            # Nothing to do
          } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){
            # Filter out "(isolated) susceptible"
            result_requested_yesterday_susceptible_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id == which(STATE_NAMES == "(isolated) susceptible"))))
            antigen_result_requested_yesterday_susceptible_ids <- result_requested_yesterday_susceptible_ids[ test_schedule_mat[result_requested_yesterday_susceptible_ids, "test type"] == "antigen" ]
            molecular_result_requested_yesterday_susceptible_ids <- result_requested_yesterday_susceptible_ids[ test_schedule_mat[result_requested_yesterday_susceptible_ids, "test type"] == "rapid molecular" ]
            
            # Filter out "(isolated) removed"
            result_requested_yesterday_removed_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id == which(STATE_NAMES == "(isolated) removed"))))
            antigen_result_requested_yesterday_removed_ids <- result_requested_yesterday_removed_ids[ test_schedule_mat[result_requested_yesterday_removed_ids, "test type"] == "antigen" ]
            molecular_result_requested_yesterday_removed_ids <- result_requested_yesterday_removed_ids[ test_schedule_mat[result_requested_yesterday_removed_ids, "test type"] == "rapid molecular" ]
            
            
          } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){
            # Filter out "(isolated) susceptible"
            result_requested_yesterday_susceptible_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id == which(STATE_NAMES == "(isolated) susceptible"))))
            antigen_result_requested_yesterday_susceptible_ids <- result_requested_yesterday_susceptible_ids[ test_schedule_mat[result_requested_yesterday_susceptible_ids, "test type"] == "antigen" ]
            molecular_result_requested_yesterday_susceptible_ids <- result_requested_yesterday_susceptible_ids[ test_schedule_mat[result_requested_yesterday_susceptible_ids, "test type"] == "rapid molecular" ]
            
            # Filter out "(isolated) recovered"
            result_requested_yesterday_recovered_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id == which(STATE_NAMES == "(isolated) recovered"))))
            antigen_result_requested_yesterday_recovered_ids <- result_requested_yesterday_recovered_ids[ test_schedule_mat[result_requested_yesterday_recovered_ids, "test type"] == "antigen" ]
            molecular_result_requested_yesterday_recovered_ids <- result_requested_yesterday_recovered_ids[ test_schedule_mat[result_requested_yesterday_recovered_ids, "test type"] == "rapid molecular" ]
          }
          
          # Filter out symptomatic infectious result seeker
          result_requested_yesterday_symptomatic_infectious_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id %in% STATELIST_SYMPTOMATIC_INFECTIOUS_RESULT_SEEKER)))
          antigen_result_requested_yesterday_symptomatic_infectious_ids <- result_requested_yesterday_symptomatic_infectious_ids[ test_schedule_mat[result_requested_yesterday_symptomatic_infectious_ids, "test type"] == "antigen" ]
          molecular_result_requested_yesterday_symptomatic_infectious_ids <- result_requested_yesterday_symptomatic_infectious_ids[ test_schedule_mat[result_requested_yesterday_symptomatic_infectious_ids, "test type"] == "rapid molecular" ]
          
          # Filter out asymptomatic infectious result seeker
          result_requested_yesterday_asymptomatic_infectious_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id %in% STATELIST_ASYMPTOMATIC_INFECTIOUS_RESULT_SEEKER)))
          antigen_result_requested_yesterday_asymptomatic_infectious_ids <- result_requested_yesterday_asymptomatic_infectious_ids[ test_schedule_mat[result_requested_yesterday_asymptomatic_infectious_ids, "test type"] == "antigen" ]
          molecular_result_requested_yesterday_asymptomatic_infectious_ids <- result_requested_yesterday_asymptomatic_infectious_ids[ test_schedule_mat[result_requested_yesterday_asymptomatic_infectious_ids, "test type"] == "rapid molecular" ]
          
          
          
          
          # Generate covid test "result"
          
          result_state_set_not_infected <- c()
          result_state_set_symptomatic_infectious <- c()
          result_state_set_asymptomatic_infectious <- c()
          
          # Construct covid test result set for each model
          if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){
            result_state_set_not_infected <- c( which(STATE_NAMES == "susceptible"), which(STATE_NAMES == "(isolated + false positive) susceptible") )
            result_state_set_symptomatic_infectious <- c( which(STATE_NAMES == "(isolated + confirmed) infected"), which(STATE_NAMES == "(unconfirmed) infected") )
            
            result_state_positive <- c( which(STATE_NAMES == "(isolated + false positive) susceptible"), which(STATE_NAMES == "(isolated + confirmed) infected") )
            result_state_negative <- c( which(STATE_NAMES == "susceptible"), which(STATE_NAMES == "(unconfirmed) infected") )
            
          } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){
            
            result_state_set_susceptible <- c( which(STATE_NAMES == "susceptible"), which(STATE_NAMES == "(isolated + false positive) susceptible") )
            result_state_set_removed <- c( which(STATE_NAMES == "removed"), which(STATE_NAMES == "(isolated + false positive) removed") )
            
            result_state_set_symptomatic_infectious <- c( which(STATE_NAMES == "(isolated + confirmed) infected"), which(STATE_NAMES == "(unconfirmed) infected") )
            
            result_state_positive <- c( which(STATE_NAMES == "(isolated + false positive) susceptible"),
                                        which(STATE_NAMES == "(isolated + confirmed) infected"),
                                        which(STATE_NAMES == "(isolated + false positive) removed"))
            
            result_state_negative <- c( which(STATE_NAMES == "susceptible"),
                                        which(STATE_NAMES == "(unconfirmed) infected"),
                                        which(STATE_NAMES == "removed"))
            
            
          } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){
            
            result_state_set_susceptible <- c( which(STATE_NAMES == "susceptible"), which(STATE_NAMES == "(isolated + false positive) susceptible") )
            result_state_set_recovered <- c( which(STATE_NAMES == "recovered"), which(STATE_NAMES == "(isolated + false positive) recovered") )
            
            result_state_set_symptomatic_infectious <- c( which(STATE_NAMES == "(isolated + confirmed) infected"), which(STATE_NAMES == "(unconfirmed) infected") )
            
            result_state_positive <- c( which(STATE_NAMES == "(isolated + false positive) susceptible"),
                                        which(STATE_NAMES == "(isolated + confirmed) infected"),
                                        which(STATE_NAMES == "(isolated + false positive) recovered"))
            
            result_state_negative <- c( which(STATE_NAMES == "susceptible"),
                                        which(STATE_NAMES == "(unconfirmed) infected"),
                                        which(STATE_NAMES == "recovered"))
          }
          
          # Get state change probability for not infected
          # Antigen - Asymptomatic: True Negative rate, Assuming the same parameters as asymptomatic selectivity
          asymptomatic_antigen_state_change_for_not_infected_prob <- c(asymptomatic_antigen_TNR, asymptomatic_antigen_FPR)
          
          # Rapid Molecular: True Negative rate
          molecular_state_change_for_not_infected_prob <- c(molecular_TNR, molecular_FPR)
          
          
          
          # Get test state change probability for infected
          # Antigen - Symptomatic: True positive rate
          symptomatic_antigen_state_change_for_infected_prob <- c(symptomatic_antigen_TPR, symptomatic_antigen_FNR)
          # Antigen - Asymptomatic: True positive rate
          asymptomatic_antigen_state_change_for_infected_prob <- c(asymptomatic_antigen_TPR, asymptomatic_antigen_FNR)
          
          # Rapid Molecular (Assuming same for symptomatic and asymptomatic): True positive Rate
          molecular_state_change_for_infected_prob <- c(molecular_TPR, molecular_FNR)
          
          
          # Book-keeping
          true_negative_sus_test_result_ids <- c()
          false_negative_test_result_ids <- c()
          
          # Tag: @calib11 - for contact tracing
          all_positive_test_result_ids <- c()
          
          # Calculate covid test results for each model
          # or process each Covid test based state transition
          # --------------------------
          
          # Antigen - Not infected : True Negative rate
          count_antigen_result_for_not_infected <- length(antigen_result_requested_yesterday_not_infected_ids)
          output_state_antigen_result_for_not_infected <- c()
          
          if(count_antigen_result_for_not_infected > 0){
            
            
            if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation"){
              # For SIR++, the only not_infected result seeking state is susceptible
              output_state_antigen_result_for_not_infected <- replicate(count_antigen_result_for_not_infected,
                                                                        sample(result_state_set_not_infected, 1,
                                                                               prob = asymptomatic_antigen_state_change_for_not_infected_prob) )
              
              # Change state
              STATE[antigen_result_requested_yesterday_not_infected_ids, day_index - 1] <- output_state_antigen_result_for_not_infected
              
              # Aggregate agent ids receiving positive result
              positive_result_ids <- antigen_result_requested_yesterday_not_infected_ids[output_state_antigen_result_for_not_infected %in% result_state_positive]
              
              # Aggregate agent ids receiving negative result
              negative_result_ids <- antigen_result_requested_yesterday_not_infected_ids[output_state_antigen_result_for_not_infected %in% result_state_negative]
              
              
            } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Contact Tracing"){
              
              susceptible_positive_result_ids <- c()
              susceptible_negative_result_ids <- c()
              # Count "(isolated) susceptible" result seeker
              count_antigen_result_for_susceptible <- length(antigen_result_requested_yesterday_susceptible_ids)
              output_state_antigen_result_for_susceptible <- c()
              if(count_antigen_result_for_susceptible > 0){
                
                # Calculate output
                output_state_antigen_result_for_susceptible <- replicate(count_antigen_result_for_susceptible,
                                                                         sample(result_state_set_susceptible, 1,
                                                                                prob = asymptomatic_antigen_state_change_for_not_infected_prob))
                
                # Change state
                STATE[antigen_result_requested_yesterday_susceptible_ids, day_index - 1] <- output_state_antigen_result_for_susceptible
                
                # Aggregate agent ids receiving positive result
                susceptible_positive_result_ids <- antigen_result_requested_yesterday_susceptible_ids[output_state_antigen_result_for_susceptible %in% result_state_positive]
                
                # Aggregate agent ids receiving negative result
                susceptible_negative_result_ids <- antigen_result_requested_yesterday_susceptible_ids[output_state_antigen_result_for_susceptible %in% result_state_negative]
              }
              
              removed_positive_result_ids <- c()
              removed_negative_result_ids <- c()
              # Count "(isolated) removed" result seeker
              count_antigen_result_for_removed <- length(antigen_result_requested_yesterday_removed_ids)
              output_state_antigen_result_for_removed <- c()
              if(count_antigen_result_for_removed > 0){
                
                # Calculate output
                output_state_antigen_result_for_removed <- replicate(count_antigen_result_for_removed,
                                                                     sample(result_state_set_removed, 1,
                                                                            prob = asymptomatic_antigen_state_change_for_not_infected_prob))
                
                # Change state
                STATE[antigen_result_requested_yesterday_removed_ids, day_index - 1] <- output_state_antigen_result_for_removed
                
                # Aggregate agent ids receiving positive result
                removed_positive_result_ids <- antigen_result_requested_yesterday_removed_ids[output_state_antigen_result_for_removed %in% result_state_positive]
                
                # Aggregate agent ids receiving negative result
                removed_negative_result_ids <- antigen_result_requested_yesterday_removed_ids[output_state_antigen_result_for_removed %in% result_state_negative]
              }
              
              # Aggregate positives and negative agent ids for book-keeping
              positive_result_ids <- c(susceptible_positive_result_ids, removed_positive_result_ids)
              negative_result_ids <- c(susceptible_negative_result_ids, removed_negative_result_ids)
              
            } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){
              
              susceptible_positive_result_ids <- c()
              susceptible_negative_result_ids <- c()
              # Count "(isolated) susceptible" result seeker
              count_antigen_result_for_susceptible <- length(antigen_result_requested_yesterday_susceptible_ids)
              output_state_antigen_result_for_susceptible <- c()
              if(count_antigen_result_for_susceptible > 0){
                
                # Calculate output
                output_state_antigen_result_for_susceptible <- replicate(count_antigen_result_for_susceptible,
                                                                         sample(result_state_set_susceptible, 1,
                                                                                prob = asymptomatic_antigen_state_change_for_not_infected_prob))
                
                # Change state
                STATE[antigen_result_requested_yesterday_susceptible_ids, day_index - 1] <- output_state_antigen_result_for_susceptible
                
                # Aggregate agent ids receiving positive result
                susceptible_positive_result_ids <- antigen_result_requested_yesterday_susceptible_ids[output_state_antigen_result_for_susceptible %in% result_state_positive]
                
                # Aggregate agent ids receiving negative result
                susceptible_negative_result_ids <- antigen_result_requested_yesterday_susceptible_ids[output_state_antigen_result_for_susceptible %in% result_state_negative]
              }
              
              recovered_positive_result_ids <- c()
              recovered_negative_result_ids <- c()
              # Count "(isolated) recovered" result seeker
              count_antigen_result_for_recovered <- length(antigen_result_requested_yesterday_recovered_ids)
              output_state_antigen_result_for_recovered <- c()
              if(count_antigen_result_for_recovered > 0){
                
                # Calculate output
                output_state_antigen_result_for_recovered <- replicate(count_antigen_result_for_recovered,
                                                                       sample(result_state_set_recovered, 1,
                                                                              prob = asymptomatic_antigen_state_change_for_not_infected_prob))
                
                # Change state
                STATE[antigen_result_requested_yesterday_recovered_ids, day_index - 1] <- output_state_antigen_result_for_recovered
                
                # Aggregate agent ids receiving positive result
                recovered_positive_result_ids <- antigen_result_requested_yesterday_recovered_ids[output_state_antigen_result_for_recovered %in% result_state_positive]
                
                # Aggregate agent ids receiving negative result
                recovered_negative_result_ids <- antigen_result_requested_yesterday_recovered_ids[output_state_antigen_result_for_recovered %in% result_state_negative]
              }
              
              # Aggregate positives and negative agent ids for book-keeping
              positive_result_ids <- c(susceptible_positive_result_ids, recovered_positive_result_ids)
              negative_result_ids <- c(susceptible_negative_result_ids, recovered_negative_result_ids)
              
            }
            
            
            # Update test scheduling data structure
            test_schedule_mat[positive_result_ids, "result"] <- "positive"
            test_schedule_mat[negative_result_ids, "result"] <- "negative"
            
            # Book-keeping
            if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){
              true_negative_sus_test_result_ids <- c(true_negative_sus_test_result_ids, negative_result_ids)
            } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){
              true_negative_sus_test_result_ids <- c(true_negative_sus_test_result_ids, susceptible_negative_result_ids)
            } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){
              true_negative_sus_test_result_ids <- c(true_negative_sus_test_result_ids, susceptible_negative_result_ids)
            }
            
            all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)
            
            # Edge: "(isolated) susceptible/removed" -- Test Result (FPR) --> (isolated + false positive) susceptible/removed
            COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)
          }
          
          # Rapid molecular - Not infected : True Negative rate
          count_molecular_result_requested_yesterday_not_infected <- length(molecular_result_requested_yesterday_not_infected_ids)
          output_state_molecular_result_requested_yesterday_not_infected <- c()
          
          if(count_molecular_result_requested_yesterday_not_infected > 0){
            
            if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation"){
              # For SIR++, the only not_infected result seeking state is susceptible
              output_state_molecular_result_requested_yesterday_not_infected <- replicate(count_molecular_result_requested_yesterday_not_infected,
                                                                                          sample(result_state_set_not_infected, 1,
                                                                                                 prob = molecular_state_change_for_not_infected_prob) )
              
              # Change state
              STATE[molecular_result_requested_yesterday_not_infected_ids, day_index - 1] <- output_state_molecular_result_requested_yesterday_not_infected
              
              # Aggregate agent ids receiving positive result
              positive_result_ids <- molecular_result_requested_yesterday_not_infected_ids[output_state_molecular_result_requested_yesterday_not_infected %in% result_state_positive]
              
              # Aggregate agent ids receiving negative result
              negative_result_ids <- molecular_result_requested_yesterday_not_infected_ids[output_state_molecular_result_requested_yesterday_not_infected %in% result_state_negative]
              
              
            } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Contact Tracing") {
              
              susceptible_positive_result_ids <- c()
              susceptible_negative_result_ids <- c()
              # Count "(isolated) susceptible" result seeker
              count_molecular_result_requested_yesterday_susceptible <- length(molecular_result_requested_yesterday_susceptible_ids)
              output_state_molecular_result_susceptible <- c()
              if(count_molecular_result_requested_yesterday_susceptible > 0){
                # Calculate output
                output_state_molecular_result_susceptible <- replicate(count_molecular_result_requested_yesterday_susceptible,
                                                                       sample(result_state_set_susceptible, 1,
                                                                              prob = molecular_state_change_for_not_infected_prob) )
                
                # Change state
                STATE[molecular_result_requested_yesterday_susceptible_ids, day_index - 1] <- output_state_molecular_result_susceptible
                
                
                # Aggregate agent ids receiving positive result
                susceptible_positive_result_ids <- molecular_result_requested_yesterday_susceptible_ids[output_state_molecular_result_susceptible %in% result_state_positive]
                
                # Aggregate agent ids receiving negative result
                susceptible_negative_result_ids <- molecular_result_requested_yesterday_susceptible_ids[output_state_molecular_result_susceptible %in% result_state_negative]
              }
              
              removed_positive_result_ids <- c()
              removed_negative_result_ids <- c()
              # Count "(isolated) removed" result seeker
              count_molecular_result_requested_yesterday_removed <- length(molecular_result_requested_yesterday_removed_ids)
              output_state_molecular_result_removed <- c()
              if(count_molecular_result_requested_yesterday_removed > 0){
                # Calculate output
                output_state_molecular_result_removed <- replicate(count_molecular_result_requested_yesterday_removed,
                                                                   sample(result_state_set_removed, 1,
                                                                          prob = molecular_state_change_for_not_infected_prob) )
                
                # Change state
                STATE[output_state_molecular_result_removed, day_index - 1] <- output_state_molecular_result_removed
                
                
                # Aggregate agent ids receiving positive result
                removed_positive_result_ids <- molecular_result_requested_yesterday_removed_ids[output_state_molecular_result_removed %in% result_state_positive]
                
                # Aggregate agent ids receiving negative result
                removed_negative_result_ids <- molecular_result_requested_yesterday_removed_ids[output_state_molecular_result_removed %in% result_state_negative]
              }
              
              
              # Aggregate positives and negative agent ids for book-keeping
              positive_result_ids <- c(susceptible_positive_result_ids, removed_positive_result_ids)
              negative_result_ids <- c(susceptible_negative_result_ids, removed_negative_result_ids)
              
            } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {
              
              susceptible_positive_result_ids <- c()
              susceptible_negative_result_ids <- c()
              # Count "(isolated) susceptible" result seeker
              count_molecular_result_requested_yesterday_susceptible <- length(molecular_result_requested_yesterday_susceptible_ids)
              output_state_molecular_result_susceptible <- c()
              if(count_molecular_result_requested_yesterday_susceptible > 0){
                # Calculate output
                output_state_molecular_result_susceptible <- replicate(count_molecular_result_requested_yesterday_susceptible,
                                                                       sample(result_state_set_susceptible, 1,
                                                                              prob = molecular_state_change_for_not_infected_prob) )
                
                # Change state
                STATE[molecular_result_requested_yesterday_susceptible_ids, day_index - 1] <- output_state_molecular_result_susceptible
                
                
                # Aggregate agent ids receiving positive result
                susceptible_positive_result_ids <- molecular_result_requested_yesterday_susceptible_ids[output_state_molecular_result_susceptible %in% result_state_positive]
                
                # Aggregate agent ids receiving negative result
                susceptible_negative_result_ids <- molecular_result_requested_yesterday_susceptible_ids[output_state_molecular_result_susceptible %in% result_state_negative]
              }
              
              recovered_positive_result_ids <- c()
              recovered_negative_result_ids <- c()
              # Count "(isolated) recovered" result seeker
              count_molecular_result_requested_yesterday_recovered <- length(molecular_result_requested_yesterday_recovered_ids)
              output_state_molecular_result_recovered <- c()
              if(count_molecular_result_requested_yesterday_recovered > 0){
                # Calculate output
                output_state_molecular_result_recovered <- replicate(count_molecular_result_requested_yesterday_recovered,
                                                                     sample(result_state_set_recovered, 1,
                                                                            prob = molecular_state_change_for_not_infected_prob) )
                
                # Change state
                STATE[output_state_molecular_result_recovered, day_index - 1] <- output_state_molecular_result_recovered
                
                
                # Aggregate agent ids receiving positive result
                recovered_positive_result_ids <- molecular_result_requested_yesterday_recovered_ids[output_state_molecular_result_recovered %in% result_state_positive]
                
                # Aggregate agent ids receiving negative result
                recovered_negative_result_ids <- molecular_result_requested_yesterday_recovered_ids[output_state_molecular_result_recovered %in% result_state_negative]
              }
              
              
              # Aggregate positives and negative agent ids for book-keeping
              positive_result_ids <- c(susceptible_positive_result_ids, recovered_positive_result_ids)
              negative_result_ids <- c(susceptible_negative_result_ids, recovered_negative_result_ids)
              
            }
            
            # Update test scheduling data structure
            test_schedule_mat[positive_result_ids, "result"] <- "positive"
            test_schedule_mat[negative_result_ids, "result"] <- "negative"
            
            # Book-keeping
            if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){
              true_negative_sus_test_result_ids <- c(true_negative_sus_test_result_ids, negative_result_ids)
            } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){
              true_negative_sus_test_result_ids <- c(true_negative_sus_test_result_ids, susceptible_negative_result_ids)
            } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){
              true_negative_sus_test_result_ids <- c(true_negative_sus_test_result_ids, susceptible_negative_result_ids)
            }
            
            all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)
            
            # Edge: "(isolated) susceptible" -- Test Result (FPR) --> (isolated + false positive) susceptible
            COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)
          }
          
          
          
          
          
          
          # Antigen - Symptomatic Infected : True Positive rate
          count_antigen_result_requested_yesterday_symptomatic_infectious <- length(antigen_result_requested_yesterday_symptomatic_infectious_ids)
          if(count_antigen_result_requested_yesterday_symptomatic_infectious > 0){
            output_state_antigen_result_requested_yesterday_symptomatic_infectious <- replicate( count_antigen_result_requested_yesterday_symptomatic_infectious,
                                                                                                 sample(result_state_set_symptomatic_infectious, 1,
                                                                                                        prob = symptomatic_antigen_state_change_for_infected_prob) )
            
            # Change state
            STATE[antigen_result_requested_yesterday_symptomatic_infectious_ids, day_index - 1] <- output_state_antigen_result_requested_yesterday_symptomatic_infectious
            
            # Update test scheduling data structure
            positive_result_ids <- antigen_result_requested_yesterday_symptomatic_infectious_ids[output_state_antigen_result_requested_yesterday_symptomatic_infectious %in% result_state_positive]
            test_schedule_mat[positive_result_ids, "result"] <- "positive"
            
            negative_result_ids <- antigen_result_requested_yesterday_symptomatic_infectious_ids[output_state_antigen_result_requested_yesterday_symptomatic_infectious %in% result_state_negative]
            test_schedule_mat[negative_result_ids, "result"] <- "negative"
            
            # Book-keeping
            false_negative_test_result_ids <- c(false_negative_test_result_ids, negative_result_ids)
            all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)
            
            # Edge: "(isolated + unconfirmed) infected"--- Test Result (TPR) --> "(isolated + confirmed) infected"
            COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)
          }
          
          # Rapid Molecular - Symptomatic Infected : True Positive rate
          count_molecular_result_requested_yesterday_symptomatic_infectious <- length(molecular_result_requested_yesterday_symptomatic_infectious_ids)
          if(count_molecular_result_requested_yesterday_symptomatic_infectious){
            output_state_molecular_result_requested_yesterday_symptomatic_infectious <- replicate( count_molecular_result_requested_yesterday_symptomatic_infectious,
                                                                                                   sample(result_state_set_symptomatic_infectious, 1,
                                                                                                          prob = molecular_state_change_for_infected_prob) )
            
            # Change state
            STATE[molecular_result_requested_yesterday_symptomatic_infectious_ids, day_index - 1] <- output_state_molecular_result_requested_yesterday_symptomatic_infectious
            
            # Update test scheduling data structures
            positive_result_ids <- molecular_result_requested_yesterday_symptomatic_infectious_ids[output_state_molecular_result_requested_yesterday_symptomatic_infectious %in% result_state_positive]
            test_schedule_mat[positive_result_ids, "result"] <- "positive"
            
            negative_result_ids <- molecular_result_requested_yesterday_symptomatic_infectious_ids[output_state_molecular_result_requested_yesterday_symptomatic_infectious %in% result_state_negative]
            test_schedule_mat[positive_result_ids, "result"] <- "negative"
            
            # Book-keeping
            false_negative_test_result_ids <- c(false_negative_test_result_ids, negative_result_ids)
            all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)
            
            # Edge: "(isolated + unconfirmed) infected"--- Test Result (TPR) --> "(isolated + confirmed) infected"
            COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)
          }
          
          
          # Antigen - Asymptomatic Infected : True Positive rate
          count_antigen_result_requested_yesterday_asymptomatic_infectious <- length(antigen_result_requested_yesterday_asymptomatic_infectious_ids)
          if(count_antigen_result_requested_yesterday_asymptomatic_infectious > 0){
            output_state_antigen_result_requested_yesterday_asymptomatic_infectious <- replicate( count_antigen_result_requested_yesterday_asymptomatic_infectious,
                                                                                                  sample(result_state_set_asymptomatic_infectious, 1,
                                                                                                         prob = asymptomatic_antigen_state_change_for_infected_prob) )
            
            # Change state
            STATE[antigen_result_requested_yesterday_asymptomatic_infectious_ids, day_index - 1] <- output_state_antigen_result_requested_yesterday_asymptomatic_infectious
            
            # Update test scheduling data structure
            positive_result_ids <- antigen_result_requested_yesterday_asymptomatic_infectious_ids[output_state_antigen_result_requested_yesterday_asymptomatic_infectious %in% result_state_positive]
            test_schedule_mat[positive_result_ids, "result"] <- "positive"
            
            negative_result_ids <- antigen_result_requested_yesterday_asymptomatic_infectious_ids[output_state_antigen_result_requested_yesterday_asymptomatic_infectious %in% result_state_negative]
            test_schedule_mat[negative_result_ids, "result"] <- "negative"
            
            # Book-keeping
            false_negative_test_result_ids <- c(false_negative_test_result_ids, negative_result_ids)
            all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)
            
            # Edge: "(isolated + unconfirmed) infected"--- Test Result (TPR) --> "(isolated + confirmed) infected"
            COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)
            
          }
          
          # Rapid Molecular - Asymptomatic Infected: True Positive rate
          count_molecular_result_requested_yesterday_asymptomatic_infectious <- length(molecular_result_requested_yesterday_asymptomatic_infectious_ids)
          if(count_molecular_result_requested_yesterday_asymptomatic_infectious > 0){
            output_state_molecular_result_requested_yesterday_asymptomatic_infectious <- replicate( count_molecular_result_requested_yesterday_asymptomatic_infectious,
                                                                                                    sample(result_state_set_asymptomatic_infectious, 1,
                                                                                                           prob = molecular_state_change_for_infected_prob) )
            
            # Change state
            STATE[molecular_result_requested_yesterday_asymptomatic_infectious_ids, day_index - 1] <- output_state_molecular_result_requested_yesterday_asymptomatic_infectious
            
            # Update test scheduling data structure
            positive_result_ids <- molecular_result_requested_yesterday_asymptomatic_infectious_ids[output_state_molecular_result_requested_yesterday_asymptomatic_infectious %in% result_state_positive]
            test_schedule_mat[positive_result_ids, "result"] <- "positive"
            
            negative_result_ids <- molecular_result_requested_yesterday_asymptomatic_infectious_ids[output_state_molecular_result_requested_yesterday_asymptomatic_infectious %in% result_state_negative]
            test_schedule_mat[negative_result_ids, "result"] <- "negative"
            
            # Book-keeping
            false_negative_test_result_ids <- c(false_negative_test_result_ids, negative_result_ids)
            all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)
            
            # Edge: "(isolated + unconfirmed) infected"--- Test Result (TPR) --> "(isolated + confirmed) infected"
            COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)
            
          }
          
          # Maintain internal iterator sets
          {
            # Isolating Susceptible agents receiving negative covid test result (and participate in disease propagation)
            previous_day_susceptible_person_ids <- union(previous_day_susceptible_person_ids, true_negative_sus_test_result_ids)
            previous_day_non_susceptible_person_ids <- setdiff(previous_day_non_susceptible_person_ids, true_negative_sus_test_result_ids)
            
            # Isolating Symptomatic and Asymptomatic Infectious agents receiving negative covid test result (and participate in disease propagation)
            previous_day_infectious_person_ids <- union(previous_day_infectious_person_ids, false_negative_test_result_ids)
          }
          
          # Tag: @calib11
          # For all disease models that include "Contact Tracing"
          # models: "SIR + Testing + Isolation + Contact Tracing",
          #         "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"
          if(CONST_ENABLE_CONTACT_TRACING && 
             ( (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") ||
               (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") ) ) {
            
            # Initialise
            all_contact_traced_neighbour_agent_ids <- c()
            
            # Get all neighbours
            all_contact_traced_neighbour_agent_ids <- batchContactTrace(all_positive_test_result_ids, day_index - 1)
            
            # For contact tracing delay of 1 day or more
            if(CONST_CONTACT_TRACING_DELAY > 0){
              # Assuming infinite contact tracing based testing capacity with antigen testing with same day results
              if(length(all_contact_traced_neighbour_agent_ids) > 0){
                test_schedule_mat[all_contact_traced_neighbour_agent_ids, "test type"] <- "antigen"
                test_schedule_mat[all_contact_traced_neighbour_agent_ids, "test scheduled on"] <- day_index - 1 + CONST_CONTACT_TRACING_DELAY
                test_schedule_mat[all_contact_traced_neighbour_agent_ids, "result day"] <- day_index - 1 + CONST_CONTACT_TRACING_DELAY + floor(available_covid_test_result_delays[available_antigen_test_index])
              }
              
            } else if(CONST_CONTACT_TRACING_DELAY == 0){
              # cat("  or Debug here!")
              
              # Assuming infinite contact tracing based testing capacity with antigen testing with same day results
              
              # Assuming predefined level of same day tracing and testing as "CONST_SAMEDAY_TRACING_LEVEL"
              # i.e. All neighbours of a particular agent who receives a positive test result
              # is tested same day. This process is not recursed further till the maximum tracing level is reached.
              
              
              for(tracing_level in 1:CONST_SAMEDAY_TRACING_LEVEL){
                
                # Get subset of "result_requested_yesterday_ids" which were also queued for testing yesterday
                # result_requested_yesterday_ids <- which(test_schedule_mat[ , "result day"] == day_index - 1)
                # same_day_tested_yesterday_ids <- result_requested_yesterday_ids[ which(as.numeric(test_schedule_mat[result_requested_yesterday_ids, "test scheduled on"]) == day_index - 1) ]
                
                # Alternatively: Union all the sets 
                same_day_tested_yesterday_ids <- union(result_requested_yesterday_not_infected_ids, 
                                                       union(result_requested_yesterday_symptomatic_infectious_ids,
                                                             result_requested_yesterday_asymptomatic_infectious_ids) )
                
                
                # Get neighbours who were not queued for same day testing yesterday
                # Note: This is applicable for models that does not require isolation while waiting for test results with result delay of 1 or more days
                not_tested_yesterday_neighbours_agent_ids <- setdiff(all_contact_traced_neighbour_agent_ids, same_day_tested_yesterday_ids)
                
                
                # Debug:
                # cat("\n Sim day:", day_index, " checking tracing for day: ", day_index - 1)
                # cat("\n  |- Total traced contacts  : ", length(all_contact_traced_neighbour_agent_ids) )
                # cat("\n  |- Already tested yesterday  : ", length(same_day_tested_yesterday_ids) )
                # cat("\n  '- New traced contact to test: ", length(not_tested_yesterday_neighbours_agent_ids), "\n")
                
                # Queue for testing (for book-keeping)
                if(length(not_tested_yesterday_neighbours_agent_ids) > 0){
                  test_schedule_mat[not_tested_yesterday_neighbours_agent_ids, "test type"] <- "antigen"
                  test_schedule_mat[not_tested_yesterday_neighbours_agent_ids, "test scheduled on"] <- day_index - 1 + CONST_CONTACT_TRACING_DELAY
                  test_schedule_mat[not_tested_yesterday_neighbours_agent_ids, "result day"] <- day_index - 1 + CONST_CONTACT_TRACING_DELAY + floor(available_covid_test_result_delays[available_antigen_test_index])
                }
                
                # Change states to waiting for test results for congruency
                {
                  COUNT_TOTAL_TEST_RESULTS_REQUESTED = length(not_tested_yesterday_neighbours_agent_ids)
                  
                  # Update previous_day_df
                  previous_day_df <- data.frame(STATE[ , day_index - 1])
                  colnames(previous_day_df) <- c("state_id")
                  
                  # Filter states only for same day traced result requesting ids
                  previous_day_result_requested_df <- subset(previous_day_df, as.numeric(rownames(previous_day_df)) %in% not_tested_yesterday_neighbours_agent_ids)
                  
                  if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Contact Tracing") {
                    test_requested_yesterday_susceptible_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id == which(STATE_NAMES == "susceptible"))))
                    test_requested_yesterday_removed_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id == which(STATE_NAMES == "removed"))))
                    
                  } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {
                    test_requested_yesterday_susceptible_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id == which(STATE_NAMES == "susceptible"))))
                    test_requested_yesterday_recovered_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id == which(STATE_NAMES == "recovered"))))
                  }
                  
                  # Filter out symptomatic infectious test seekers
                  test_requested_yesterday_symptomatic_infectious_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id %in% STATELIST_SYMPTOMATIC_INFECTIOUS_TEST_SEEKER)))
                  # Filter out asymptomatic infectious test seekers
                  test_requested_yesterday_asymptomatic_infectious_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id %in% STATELIST_ASYMPTOMATIC_INFECTIOUS_TEST_SEEKER)))
                  
                  
                  
                  
                  if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Contact Tracing") {
                    # For SIR+++ process multiple non-infected test-seeker states, namely: susceptible and removed
                    STATE[ test_requested_yesterday_susceptible_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) susceptible")
                    STATE[ test_requested_yesterday_removed_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) removed")
                    
                  } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {
                    STATE[ test_requested_yesterday_susceptible_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) susceptible")
                    STATE[ test_requested_yesterday_recovered_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) recovered")
                  }
                  
                  # For SIR+++, the infected test seeker state is "infected"
                  STATE[ test_requested_yesterday_symptomatic_infectious_ids , day_index - 1] <- which(STATE_NAMES == "(isolated + unconfirmed) infected")
                  
                  # For SIR+++, there are no asymptomatic and infectious test seekers
                  
                  # Change infectious and susceptible sets (as per disease propagation model)
                  {
                    previous_day_susceptible_person_ids <- setdiff(previous_day_susceptible_person_ids, test_requested_yesterday_susceptible_ids)
                    previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, test_requested_yesterday_susceptible_ids)
                    
                    # "Symptomatic Infectious agents waiting for covid test result isolate themselves (and do not participate in disease propagation)
                    previous_day_infectious_person_ids <- setdiff(previous_day_infectious_person_ids, test_requested_yesterday_symptomatic_infectious_ids)
                    
                    # "Asymptomatic Infectious agents waiting for covid test result isolate themselves (and do not participate in disease propagation)
                    previous_day_infectious_person_ids <- setdiff(previous_day_infectious_person_ids, test_requested_yesterday_asymptomatic_infectious_ids)
                  }
                }
                
                # Compute result states
                # Filter result seekers into appropriate compartments
                {
                  # Pass "(isolated) susceptible"
                  result_requested_yesterday_susceptible_ids <- test_requested_yesterday_susceptible_ids
                  antigen_result_requested_yesterday_susceptible_ids <- test_requested_yesterday_susceptible_ids
                  
                  if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Contact Tracing") {
                    # Pass "(isolated) removed"
                    result_requested_yesterday_removed_ids <- test_requested_yesterday_removed_ids
                    antigen_result_requested_yesterday_removed_ids <- test_requested_yesterday_removed_ids
                    
                  } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {
                    result_requested_yesterday_recovered_ids <- test_requested_yesterday_recovered_ids
                    antigen_result_requested_yesterday_recovered_ids <- test_requested_yesterday_recovered_ids
                  }
                  
                  
                  # Pass symptomatic infectious result seeker
                  result_requested_yesterday_symptomatic_infectious_ids <- test_requested_yesterday_symptomatic_infectious_ids
                  antigen_result_requested_yesterday_symptomatic_infectious_ids <- test_requested_yesterday_symptomatic_infectious_ids
                  
                  
                  # Pass asymptomatic infectious result seeker
                  result_requested_yesterday_asymptomatic_infectious_ids <- test_requested_yesterday_asymptomatic_infectious_ids
                  antigen_result_requested_yesterday_asymptomatic_infectious_ids <- test_requested_yesterday_asymptomatic_infectious_ids
                  
                  # Debug:
                  # cat("\n Sim day:", day_index, " checking tracing for", day_index - 1)
                  # cat("\n  |- Already tested yesterday  : ", length(same_day_tested_yesterday_ids) )
                  # cat("\n  '- New traced contact to test: ", length(not_tested_yesterday_neighbours_agent_ids) )
                  # cat("\n   |- Susceptible traced contact to test: ", length(result_requested_yesterday_susceptible_ids), " / ",  length(test_requested_yesterday_susceptible_ids) )
                  # cat("\n   |- Removed traced contact to test: ", length(result_requested_yesterday_removed_ids), " / ",  length(test_requested_yesterday_removed_ids) )
                  # cat("\n   '- Infectious traced contact to test: ", length(result_requested_yesterday_symptomatic_infectious_ids), " / ",  length(test_requested_yesterday_symptomatic_infectious_ids), "\n")
                }
                
                # Book-keeping
                true_negative_sus_test_result_ids <- c()
                false_negative_test_result_ids <- c()
                
                # Tag: @calib11 - for same day recursive contact tracing
                all_sameday_traced_positive_test_result_ids <- c()
                
                # Calculate covid test results for each model
                # or process each Covid test based state transition
                # --------------------------
                
                # Antigen - Not infected : True Negative rate
                if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Contact Tracing") {
                  count_antigen_result_for_not_infected <- length( union(result_requested_yesterday_susceptible_ids, antigen_result_requested_yesterday_removed_ids) )
                  
                } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {
                  count_antigen_result_for_not_infected <- length( union(result_requested_yesterday_susceptible_ids, antigen_result_requested_yesterday_recovered_ids) )
                }
                output_state_antigen_result_for_not_infected <- c()
                if(count_antigen_result_for_not_infected > 0){
                  
                  
                  if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Contact Tracing"){
                    
                    susceptible_positive_result_ids <- c()
                    susceptible_negative_result_ids <- c()
                    # Count "(isolated) susceptible" result seeker
                    count_antigen_result_for_susceptible <- length(antigen_result_requested_yesterday_susceptible_ids)
                    output_state_antigen_result_for_susceptible <- c()
                    if(count_antigen_result_for_susceptible > 0){
                      
                      # Calculate output
                      output_state_antigen_result_for_susceptible <- replicate(count_antigen_result_for_susceptible,
                                                                               sample(result_state_set_susceptible, 1,
                                                                                      prob = asymptomatic_antigen_state_change_for_not_infected_prob))
                      
                      # Change state
                      STATE[antigen_result_requested_yesterday_susceptible_ids, day_index - 1] <- output_state_antigen_result_for_susceptible
                      
                      # Aggregate agent ids receiving positive result
                      susceptible_positive_result_ids <- antigen_result_requested_yesterday_susceptible_ids[output_state_antigen_result_for_susceptible %in% result_state_positive]
                      
                      # Aggregate agent ids receiving negative result
                      susceptible_negative_result_ids <- antigen_result_requested_yesterday_susceptible_ids[output_state_antigen_result_for_susceptible %in% result_state_negative]
                    }
                    
                    removed_positive_result_ids <- c()
                    removed_negative_result_ids <- c()
                    # Count "(isolated) removed" result seeker
                    count_antigen_result_for_removed <- length(antigen_result_requested_yesterday_removed_ids)
                    output_state_antigen_result_for_removed <- c()
                    if(count_antigen_result_for_removed > 0){
                      
                      # Calculate output
                      output_state_antigen_result_for_removed <- replicate(count_antigen_result_for_removed,
                                                                           sample(result_state_set_removed, 1,
                                                                                  prob = asymptomatic_antigen_state_change_for_not_infected_prob))
                      
                      # Change state
                      STATE[antigen_result_requested_yesterday_removed_ids, day_index - 1] <- output_state_antigen_result_for_removed
                      
                      # Aggregate agent ids receiving positive result
                      removed_positive_result_ids <- antigen_result_requested_yesterday_removed_ids[output_state_antigen_result_for_removed %in% result_state_positive]
                      
                      # Aggregate agent ids receiving negative result
                      removed_negative_result_ids <- antigen_result_requested_yesterday_removed_ids[output_state_antigen_result_for_removed %in% result_state_negative]
                    }
                    
                    # Aggregate positives and negative agent ids for book-keeping
                    positive_result_ids <- c(susceptible_positive_result_ids, removed_positive_result_ids)
                    negative_result_ids <- c(susceptible_negative_result_ids, removed_negative_result_ids)
                    
                  } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){
                    
                    susceptible_positive_result_ids <- c()
                    susceptible_negative_result_ids <- c()
                    # Count "(isolated) susceptible" result seeker
                    count_antigen_result_for_susceptible <- length(antigen_result_requested_yesterday_susceptible_ids)
                    output_state_antigen_result_for_susceptible <- c()
                    if(count_antigen_result_for_susceptible > 0){
                      
                      # Calculate output
                      output_state_antigen_result_for_susceptible <- replicate(count_antigen_result_for_susceptible,
                                                                               sample(result_state_set_susceptible, 1,
                                                                                      prob = asymptomatic_antigen_state_change_for_not_infected_prob))
                      
                      # Change state
                      STATE[antigen_result_requested_yesterday_susceptible_ids, day_index - 1] <- output_state_antigen_result_for_susceptible
                      
                      # Aggregate agent ids receiving positive result
                      susceptible_positive_result_ids <- antigen_result_requested_yesterday_susceptible_ids[output_state_antigen_result_for_susceptible %in% result_state_positive]
                      
                      # Aggregate agent ids receiving negative result
                      susceptible_negative_result_ids <- antigen_result_requested_yesterday_susceptible_ids[output_state_antigen_result_for_susceptible %in% result_state_negative]
                    }
                    
                    recovered_positive_result_ids <- c()
                    recovered_negative_result_ids <- c()
                    # Count "(isolated) recovered" result seeker
                    count_antigen_result_for_recovered <- length(antigen_result_requested_yesterday_recovered_ids)
                    output_state_antigen_result_for_recovered <- c()
                    if(count_antigen_result_for_recovered > 0){
                      
                      # Calculate output
                      output_state_antigen_result_for_recovered <- replicate(count_antigen_result_for_recovered,
                                                                             sample(result_state_set_recovered, 1,
                                                                                    prob = asymptomatic_antigen_state_change_for_not_infected_prob))
                      
                      # Change state
                      STATE[antigen_result_requested_yesterday_recovered_ids, day_index - 1] <- output_state_antigen_result_for_recovered
                      
                      # Aggregate agent ids receiving positive result
                      recovered_positive_result_ids <- antigen_result_requested_yesterday_recovered_ids[output_state_antigen_result_for_recovered %in% result_state_positive]
                      
                      # Aggregate agent ids receiving negative result
                      recovered_negative_result_ids <- antigen_result_requested_yesterday_recovered_ids[output_state_antigen_result_for_recovered %in% result_state_negative]
                    }
                    
                    # Aggregate positives and negative agent ids for book-keeping
                    positive_result_ids <- c(susceptible_positive_result_ids, recovered_positive_result_ids)
                    negative_result_ids <- c(susceptible_negative_result_ids, recovered_negative_result_ids)
                    
                  }
                  
                  
                  # Update test scheduling data structure
                  test_schedule_mat[positive_result_ids, "result"] <- "positive"
                  test_schedule_mat[negative_result_ids, "result"] <- "negative"
                  
                  
                  true_negative_sus_test_result_ids <- c(true_negative_sus_test_result_ids, susceptible_negative_result_ids)
                  
                  
                  all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)
                  
                  # For recursion on same day contact tracing and testing
                  all_sameday_traced_positive_test_result_ids <- c(all_sameday_traced_positive_test_result_ids, positive_result_ids)
                  
                  # Edge: "(isolated) susceptible/removed" -- Test Result (FPR) --> (isolated + false positive) susceptible/removed
                  COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)
                }
                
                
                # Antigen - Symptomatic Infected : True Positive rate
                count_antigen_result_requested_yesterday_symptomatic_infectious <- length(antigen_result_requested_yesterday_symptomatic_infectious_ids)
                if(count_antigen_result_requested_yesterday_symptomatic_infectious > 0){
                  output_state_antigen_result_requested_yesterday_symptomatic_infectious <- replicate( count_antigen_result_requested_yesterday_symptomatic_infectious,
                                                                                                       sample(result_state_set_symptomatic_infectious, 1,
                                                                                                              prob = symptomatic_antigen_state_change_for_infected_prob) )
                  
                  # Change state
                  STATE[antigen_result_requested_yesterday_symptomatic_infectious_ids, day_index - 1] <- output_state_antigen_result_requested_yesterday_symptomatic_infectious
                  
                  # Update test scheduling data structure
                  positive_result_ids <- antigen_result_requested_yesterday_symptomatic_infectious_ids[output_state_antigen_result_requested_yesterday_symptomatic_infectious %in% result_state_positive]
                  test_schedule_mat[positive_result_ids, "result"] <- "positive"
                  
                  negative_result_ids <- antigen_result_requested_yesterday_symptomatic_infectious_ids[output_state_antigen_result_requested_yesterday_symptomatic_infectious %in% result_state_negative]
                  test_schedule_mat[negative_result_ids, "result"] <- "negative"
                  
                  # Book-keeping
                  false_negative_test_result_ids <- c(false_negative_test_result_ids, negative_result_ids)
                  all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)
                  
                  # For recursion on same day contact tracing and testing
                  all_sameday_traced_positive_test_result_ids <- c(all_sameday_traced_positive_test_result_ids, positive_result_ids)
                  
                  # Edge: "(isolated + unconfirmed) infected"--- Test Result (TPR) --> "(isolated + confirmed) infected"
                  COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)
                }
                
                # Antigen - Asymptomatic Infected : True Positive rate
                count_antigen_result_requested_yesterday_asymptomatic_infectious <- length(antigen_result_requested_yesterday_asymptomatic_infectious_ids)
                if(count_antigen_result_requested_yesterday_asymptomatic_infectious > 0){
                  output_state_antigen_result_requested_yesterday_asymptomatic_infectious <- replicate( count_antigen_result_requested_yesterday_asymptomatic_infectious,
                                                                                                        sample(result_state_set_asymptomatic_infectious, 1,
                                                                                                               prob = asymptomatic_antigen_state_change_for_infected_prob) )
                  
                  # Change state
                  STATE[antigen_result_requested_yesterday_asymptomatic_infectious_ids, day_index - 1] <- output_state_antigen_result_requested_yesterday_asymptomatic_infectious
                  
                  # Update test scheduling data structure
                  positive_result_ids <- antigen_result_requested_yesterday_asymptomatic_infectious_ids[output_state_antigen_result_requested_yesterday_asymptomatic_infectious %in% result_state_positive]
                  test_schedule_mat[positive_result_ids, "result"] <- "positive"
                  
                  negative_result_ids <- antigen_result_requested_yesterday_asymptomatic_infectious_ids[output_state_antigen_result_requested_yesterday_asymptomatic_infectious %in% result_state_negative]
                  test_schedule_mat[negative_result_ids, "result"] <- "negative"
                  
                  # Book-keeping
                  false_negative_test_result_ids <- c(false_negative_test_result_ids, negative_result_ids)
                  all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)
                  
                  # For recursion on same day contact tracing and testing
                  all_sameday_traced_positive_test_result_ids <- c(all_sameday_traced_positive_test_result_ids, positive_result_ids)
                  
                  # Edge: "(isolated + unconfirmed) infected"--- Test Result (TPR) --> "(isolated + confirmed) infected"
                  COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)
                  
                }
                
                
                # Maintain internal iterator sets
                {
                  # Isolating Susceptible agents receiving negative covid test result (and participate in disease propagation)
                  previous_day_susceptible_person_ids <- union(previous_day_susceptible_person_ids, true_negative_sus_test_result_ids)
                  previous_day_non_susceptible_person_ids <- setdiff(previous_day_non_susceptible_person_ids, true_negative_sus_test_result_ids)
                  
                  # Isolating Symptomatic and Asymptomatic Infectious agents receiving negative covid test result (and participate in disease propagation)
                  previous_day_infectious_person_ids <- union(previous_day_infectious_person_ids, false_negative_test_result_ids)
                }
                
                
                
                
                
                # Debug:
                # cat("\n Tracing level: ", tracing_level)
                # Recurse on new positive cases
                
                all_contact_traced_neighbour_agent_ids <- c()
                
                # Get all neighbours
                all_contact_traced_neighbour_agent_ids <- batchContactTrace(all_sameday_traced_positive_test_result_ids, day_index - 1)
                
                # If this is the final level of same day tracing, queue up this level of traced contacts for next day testing
                if(tracing_level == CONST_SAMEDAY_TRACING_LEVEL){
                  
                  if(length(all_contact_traced_neighbour_agent_ids) > 0){
                    test_schedule_mat[all_contact_traced_neighbour_agent_ids, "test type"] <- "antigen"
                    test_schedule_mat[all_contact_traced_neighbour_agent_ids, "test scheduled on"] <- day_index - 1 + 1
                    test_schedule_mat[all_contact_traced_neighbour_agent_ids, "result day"] <- day_index - 1 + 1 + floor(available_covid_test_result_delays[available_antigen_test_index])
                  }
                  
                }
                
                # In compliance with Jarod's implementation
                # Sending these contacts to their isolated compartments 
                if (CONST_ISOLATE_TRACED_OVERFLOW){
                  
                  
                  # for (traced_agent_id in all_contact_traced_neighbour_agent_ids) {
                  #   if (STATE[traced_agent_id, day_index - 1] == which(STATE_NAMES == "susceptible")) {
                  #     STATE[traced_agent_id, day_index - 1] <- which(STATE_NAMES == "(isolated) susceptible")
                  #     previous_day_susceptible_person_ids <- setdiff(previous_day_susceptible_person_ids, traced_agent_id)
                  #     previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, traced_agent_id)
                  #     
                  #   } else if (STATE[traced_agent_id, day_index - 1] == which(STATE_NAMES == "(unconfirmed) infected")) {
                  #     STATE[traced_agent_id, day_index - 1] <- which(STATE_NAMES == "(isolated + unconfirmed) infected")
                  #     previous_day_infectious_person_ids <- setdiff(previous_day_infectious_person_ids, traced_agent_id)
                  #     
                  #   } else if (STATE[traced_agent_id, day_index - 1] == which(STATE_NAMES == "removed")) {
                  #     STATE[traced_agent_id, day_index - 1] <- which(STATE_NAMES == "(isolated) removed")
                  #     previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, traced_agent_id)
                  #     
                  #   } 
                  # }
                
                  # Update previous_day_df
                  previous_day_df <- data.frame(STATE[ , day_index - 1])
                  colnames(previous_day_df) <- c("state_id")
                  
                  # Filter states only for test requesting ids
                  next_day_test_requested_df <- subset(previous_day_df, as.numeric(rownames(previous_day_df)) %in% all_contact_traced_neighbour_agent_ids)
                  
                  
                  if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") {
                    # For SIR+++ process multiple non-infected test-seeker states, namely: susceptible and removed
                    
                    test_requested_next_day_susceptible_ids <- as.numeric(rownames(subset(next_day_test_requested_df, state_id == which(STATE_NAMES == "susceptible"))))
                    test_requested_next_day_removed_ids <- as.numeric(rownames(subset(next_day_test_requested_df, state_id == which(STATE_NAMES == "removed"))))
                  } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {
                    # For augmented SIR with separate recovered and dead compartments,
                    # process multiple non-infected test-seeker states, namely: susceptible and recovered
                    
                    test_requested_next_day_susceptible_ids <- as.numeric(rownames(subset(next_day_test_requested_df, state_id == which(STATE_NAMES == "susceptible"))))
                    test_requested_next_day_recovered_ids <- as.numeric(rownames(subset(next_day_test_requested_df, state_id == which(STATE_NAMES == "recovered"))))
                  }
                  # Filter out symptomatic infectious test seekers
                  test_requested_next_day_symptomatic_infectious_ids <- as.numeric(rownames(subset(next_day_test_requested_df, state_id %in% STATELIST_SYMPTOMATIC_INFECTIOUS_TEST_SEEKER)))
                  # Filter out asymptomatic infectious test seekers
                  test_requested_next_day_asymptomatic_infectious_ids <- as.numeric(rownames(subset(next_day_test_requested_df, state_id %in% STATELIST_ASYMPTOMATIC_INFECTIOUS_TEST_SEEKER)))
                    
                
                  
                  # Set "waiting for result" state retroactively
                  if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){
                    
                    # For SIR+++ process multiple non-infected test-seeker states, namely: susceptible and removed
                    STATE[ test_requested_next_day_susceptible_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) susceptible")
                    STATE[ test_requested_next_day_removed_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) removed")
                    
                    # For SIR+++, the infected test seeker state is "infected"
                    STATE[ test_requested_next_day_symptomatic_infectious_ids , day_index - 1] <- which(STATE_NAMES == "(isolated + unconfirmed) infected")
                    
                    # For SIR+++, there are no asymptomatic and infectious test seekers
                  } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){
                    
                    # For augmented SIR with separate recovered and dead compartments,
                    # multiple non-infected test-seeker states, namely: susceptible and recovered
                    STATE[ test_requested_next_day_susceptible_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) susceptible")
                    STATE[ test_requested_next_day_recovered_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) recovered")
                    
                    # For SIR+++, the infected test seeker state is "infected"
                    STATE[ test_requested_next_day_symptomatic_infectious_ids , day_index - 1] <- which(STATE_NAMES == "(isolated + unconfirmed) infected")
                    
                    # For SIR+++, there are no asymptomatic and infectious test seekers
                  }
                  
                  
                  # Manage iterator sets
                  if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){
                    previous_day_susceptible_person_ids <- setdiff(previous_day_susceptible_person_ids, test_requested_next_day_susceptible_ids)
                    previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, test_requested_next_day_susceptible_ids)
                    
                    
                  } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){
                    previous_day_susceptible_person_ids <- setdiff(previous_day_susceptible_person_ids, test_requested_next_day_susceptible_ids)
                    previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, test_requested_next_day_susceptible_ids)
                  }
                  
                  # "Symptomatic Infectious agents waiting for covid test result isolate themselves (and do not participate in disease propagation)
                  previous_day_infectious_person_ids <- setdiff(previous_day_infectious_person_ids, test_requested_next_day_symptomatic_infectious_ids)
                  
                  # "Asymptomatic Infectious agents waiting for covid test result isolate themselves (and do not participate in disease propagation)
                  previous_day_infectious_person_ids <- setdiff(previous_day_infectious_person_ids, test_requested_next_day_asymptomatic_infectious_ids)
                  
                  
                  
                  
                }
                
                
                
                
                
              }
            }
          }
          # Debug:
          # cat("\n----------Moving back to susceptible from non-susceptible at Test Request: ", length(true_negative_sus_test_result_ids))
          
          # Isolating Susceptible agents receiving negative covid test result (and participate in disease propagation)
          # previous_day_susceptible_person_ids <- union(previous_day_susceptible_person_ids, true_negative_sus_test_result_ids)
          # previous_day_non_susceptible_person_ids <- setdiff(previous_day_non_susceptible_person_ids, true_negative_sus_test_result_ids)
          #
          # # Isolating Symptomatic and Asymptomatic Infectious agents receiving negative covid test result (and participate in disease propagation)
          # previous_day_infectious_person_ids <- union(previous_day_infectious_person_ids, false_negative_test_result_ids)
          
          
        }

        # "Did any susceptible agent get infected yesterday (such that the state change occurred today)?"
        for(person_id in previous_day_susceptible_person_ids){

          # Get state of this person from previous day
          PREV_STATE_INDEX <- as.numeric(STATE[person_id, day_index - 1])
          PREV_STATE_NAME <- STATE_NAMES[PREV_STATE_INDEX] # Note: At this point STATE_NAME is a matrix, by the end of execution it's converted to df for CSV IO

          # for no transitions
          next_state_id <- PREV_STATE_INDEX
          next_state_name <- PREV_STATE_NAME

          # Debug:
          # cat("    \n...............\n")
          # cat("    Previous state of Person no: ", person_id, ", state id: ", PREV_STATE_INDEX, ", state name: ", PREV_STATE_NAME, "\n", sep = "")


          # Get possible transitions
          POSSIBLE_TRANSITIONS_mat_colnames <- c("state_id", "transition_weights")
          POSSIBLE_TRANSITIONS_mat <- matrix( c(which(transition_matrix[PREV_STATE_INDEX, ] != ""),
                                                transition_matrix[PREV_STATE_INDEX, which(transition_matrix[PREV_STATE_INDEX, ] != "")]),
                                              ncol = 2)

          colnames(POSSIBLE_TRANSITIONS_mat) <- POSSIBLE_TRANSITIONS_mat_colnames

          # Get weights
          # Check which of the transition weights are CALCULATE_FROM_CONTACT_MATRIX
          # Debug:
          # print(POSSIBLE_TRANSITIONS_mat)

          # Hacky embedded stochastic infection possibility in state-evolution matri
          NO_OF_CONTACT_MATRIX_BASED_TRANSITION <- length(which(POSSIBLE_TRANSITIONS_mat[ , "transition_weights"] == "CALCULATE_FROM_CONTACT_MATRIX"))


          if(NO_OF_CONTACT_MATRIX_BASED_TRANSITION > 0){
            # Directly filtering aggregated contact matrix array
            # list_of_contacts <- which(contact_matrix[person_id, ] > 0)

            # Unrolling per agent per day contact bitmaps to indices (generated during reading contact matrix) ~ 300 MB for 30 contact matrix of 4602 agents
            # list_of_contacts_from_bitvector <- which(list_day_x_person_x_contact[[contact_matrix_index_lookup_list[day_index - 1]]][[person_id]] == TRUE)
            # Debug - passed
            # if(!all(list_of_contacts == list_of_contacts_from_bitvector)){ cat("\nBitvector optimisation failed for sim day: ", day_index, ", person id: ", person_id) }

            # Looking up per agent per contact vector (generated during reading contact matrix) ~ 2.5 GB for 30 contact matrix of 4602 agents
            # list_of_contacts_from_list_of_lists <- list_day_x_person_x_contact[[contact_matrix_index_lookup_list[day_index - 1]]][[person_id]]
            # Debug - passed
            # if(!all(list_of_contacts == list_of_contacts_from_list_of_lists)){ cat("\nBitvector optimisation failed for sim day: ", day_index, ", person id: ", person_id) }

            # This interface may remain the same for both location wise or aggregated contact matrix types
            # Contains duplicate agent ids for location wise contact matrix, they are removed in the set intersection method

            # Tag: @calib08
            # Load contacts with events during event duration
            if( GENERATE_EVENT_CONTACT_PAIRS && day_index >= EVENT_START_DATE && day_index <= EVENT_END_DATE){
              list_of_contacts <- list_day_x_person_x_contact_w_event[[contact_matrix_index_lookup_list[day_index - 1]]][[person_id]]
            } else {
              list_of_contacts <- list_day_x_person_x_contact[[contact_matrix_index_lookup_list[day_index - 1]]][[person_id]]
            }


            # Check if person 2 was infectious on day_index - 1
            list_of_infectious_contacts <- intersect(list_of_contacts, previous_day_infectious_person_ids)

            COUNT_OF_POSSIBLE_INFECTIOUS_CONTACTS <- length(list_of_infectious_contacts)

            next_state_id <- PREV_STATE_INDEX
            next_state_name <- STATE_NAMES[next_state_id]

            if(COUNT_OF_POSSIBLE_INFECTIOUS_CONTACTS > 0) {

              # Debug:
              # cat("       Person:, ", person_id, " may be infected by: ", COUNT_OF_POSSIBLE_INFECTIOUS_CONTACTS, " infected neighbours\n", sep = "")

              # Perform each Coin toss and check for possible infection till first occurs
              # @"Tag: Sequence of probabilities"
              ALREADY_INFECTED <- FALSE

              # Note: The order of infection events are lost because of the nature of the shared time calculaion
              # TODO: Change the for loop to random sampling to remove ordering bias of infector?
              for(contact_index in list_of_infectious_contacts){

                if( ALREADY_INFECTED ) { break }

                # Get relative infectivity of variant
                relative_infectivity <- as.numeric(active_voc_mat[which(active_voc_mat[ , "variant"] == infection_hist_mat[contact_index, "variant"]), "Relative infectivity"])

                # Tag: @calib09
                if(CONST_USE_INFECTIOUSNESS_PDF){
                  temp_days_since_infected <- day_index - as.numeric(infection_hist_mat[[contact_index, "infected on"]])
                  relative_infectivity <- relative_infectivity * normalised_infectiousness_pdf(temp_days_since_infected)
                }

                if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Aggregated"){

                  # Load contact time including time spent in event with events during event duration
                  if(GENERATE_EVENT_CONTACT_PAIRS && day_index >= EVENT_START_DATE && day_index <= EVENT_END_DATE){
                    contact_time <- list_of_contact_matrix_w_event[[contact_matrix_index_lookup_list[day_index - 1]]][person_id, contact_index]
                  } else {
                    # Note, old code based on iterator
                    contact_time <- contact_matrix[person_id, contact_index]
                    # Maybe changed to the following
                    # contact_time <- list_of_contact_matrix[[contact_matrix_index_lookup_list[day_index - 1]]][person_id, contact_index]
                  }

                } else if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Location based"){

                  # Note, this is where may access the location type as well
                  # list_of_contact_times <- contact_matrix_as_pairlist[
                  #   which(contact_matrix_as_pairlist[
                  #     which(contact_matrix_as_pairlist$person_id_1 == person_id), ]$person_id_2 == contact_index), ]$contact_in_seconds

                  # Tag: @calib08
                  # Load contact time including time spent in event with events during event duration
                  if( GENERATE_EVENT_CONTACT_PAIRS && day_index >= EVENT_START_DATE && day_index <= EVENT_END_DATE){
                    person_specific_contact_indices <- which(list_day_x_person_x_contact_w_event[[contact_matrix_index_lookup_list[day_index - 1]]][[person_id]] == contact_index)

                    # Get all person-specific contact times
                    list_of_contact_times <- list_day_x_person_x_contact_time_w_event[[contact_matrix_index_lookup_list[day_index - 1]]][[person_id]]
                    # Filter times for this particular contact
                    list_of_contact_times <- list_of_contact_times[person_specific_contact_indices]

                    # Get all person-specific contact location types
                    list_of_contact_location_types <- list_day_x_person_x_contact_location_type_w_event[[contact_matrix_index_lookup_list[day_index - 1]]][[person_id]]
                    # Filter location types for this particular contact
                    list_of_contact_location_types <- list_of_contact_location_types[person_specific_contact_indices]

                    # Get all person-specific contact location ids
                    list_of_contact_location_ids <- list_day_x_person_x_contact_location_id_w_event[[contact_matrix_index_lookup_list[day_index - 1]]][[person_id]]
                    # Filter location types for this particular contact
                    list_of_contact_location_ids <- list_of_contact_location_ids[person_specific_contact_indices]

                  } else {
                    person_specific_contact_indices <- which(list_day_x_person_x_contact[[contact_matrix_index_lookup_list[day_index - 1]]][[person_id]] == contact_index)

                    # Get all person-specific contact times
                    list_of_contact_times <- list_day_x_person_x_contact_time[[contact_matrix_index_lookup_list[day_index - 1]]][[person_id]]
                    # Filter times for this particular contact
                    list_of_contact_times <- list_of_contact_times[person_specific_contact_indices]

                    # Get all person-specific contact location types
                    list_of_contact_location_types <- list_day_x_person_x_contact_location_type[[contact_matrix_index_lookup_list[day_index - 1]]][[person_id]]
                    # Filter location types for this particular contact
                    list_of_contact_location_types <- list_of_contact_location_types[person_specific_contact_indices]

                    # Get all person-specific contact location ids
                    list_of_contact_location_ids <- list_day_x_person_x_contact_location_id[[contact_matrix_index_lookup_list[day_index - 1]]][[person_id]]
                    # Filter location types for this particular contact
                    list_of_contact_location_ids <- list_of_contact_location_ids[person_specific_contact_indices]
                  }
                }

                # Get vaccine efficacy w.r.t contact's variant
                vaccine_odds <- NA # Assume person_id is without vaccine

                # Get possible vaccine efficacies
                vaccine_odds1 <- as.numeric(active_voc_mat[which(active_voc_mat[ , "variant"] == infection_hist_mat[contact_index, "variant"]), "Vaccine dose 1 efficacy"])
                vaccine_odds2 <- as.numeric(active_voc_mat[which(active_voc_mat[ , "variant"] == infection_hist_mat[contact_index, "variant"]), "Vaccine dose 2 efficacy"])
                vaccine_odds3 <- as.numeric(active_voc_mat[which(active_voc_mat[ , "variant"] == infection_hist_mat[contact_index, "variant"]), "Vaccine dose 3 efficacy"])



                if(PREV_STATE_NAME == "received_dose1"){
                  vaccine_odds <- vaccine_odds1


                  # Scale w.r.t vaccination day # Remove: as now we have vaccinated pop without a set vaccination day
                  # dose1_received_on <- strtoi(infection_hist_mat[person_id, "dose 1 on"])
                  # vaccine_odds <- scaleVaccineOddsLinear(day_index - dose1_received_on) * vaccine_odds
                }

                else if(PREV_STATE_NAME == "received_dose2"){

                  # Scale w.r.t vaccination day # Remove: as now we have vaccinated pop without a set vaccination day
                  # dose1_received_on <- strtoi(infection_hist_mat[person_id, "dose 1 on"])
                  # vaccine_odds1 <- scaleVaccineOddsLinear(day_index - dose1_received_on) * vaccine_odds1

                  # Scale w.r.t vaccination day # Remove: as now we have vaccinated pop without a set vaccination day
                  # dose2_received_on <- strtoi(infection_hist_mat[person_id, "dose 2 on"])
                  # vaccine_odds2 <- scaleVaccineOddsLinear(day_index - dose2_received_on) * vaccine_odds2

                  vaccine_odds <- max(vaccine_odds1, vaccine_odds2)

                }

                # For 3rd dose{
                # Note: This may fail for variants whose 3rd dose efficacy in-spite of the guard clauses of 2nd and 1st dose efficacies
                else if(PREV_STATE_NAME == "received_dose3"){

                  vaccine_odds <- max(vaccine_odds1, vaccine_odds2, vaccine_odds3)

                }


                if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Aggregated"){
                  random_coin_toss <- runif(1)
                  # Debug:
                  # cat("Coin toss: ", random_coin_toss, "\n", sep = "")
                  # Debug:
                  COUNT_NO_OF_INFECTION_COIN_FLIPS = COUNT_NO_OF_INFECTION_COIN_FLIPS + 1

                  this_infection_chance <- getInfection_probability_w_variant_w_vaccine(vaccine_odds, relative_infectivity, contact_time)
                  # Debug:
                  # cat("           For neighbour: ", contact_index," unifrom_random_sample(0,1): ", random_coin_toss, " , infection probability: ", this_infection_chance, " => infection :", random_coin_toss <= this_infection_chance, "\n", sep = "")

                  # Calibration mode:

                  # if( random_coin_toss <= getInfection_probability(contact_time) ){ # For no vaccination, no variants with different infectivity

                  # if( random_coin_toss <= contact_matrix[person_id, contact_index]){
                  # if( random_coin_toss <= getInfection_probability_w_variant(relative_infectivity, contact_time)){

                  # if( random_coin_toss <= getInfection_probability_w_variant_w_vaccine(vaccine_odds, relative_infectivity, contact_time)){

                  if( random_coin_toss <= this_infection_chance ) {

                    # Debug:
                    COUNT_NO_OF_INFECTION_EVENTS = COUNT_NO_OF_INFECTION_EVENTS + 1

                    infection_WHO_label <- active_voc_mat[which(active_voc_mat[ , "variant"] == infection_hist_mat[contact_index, "variant"]), "WHO label"]


                    if (CONST_CHOSEN_DISEASE_MODEL == "Complex"){
                      # Infection event for un-vaccinated
                      if (PREV_STATE_NAME == "susceptible"){
                        # Check if omicron
                        if(infection_WHO_label == "Omicron") {
                          next_state_id <- sample( c( which(STATE_NAMES == "00_latent_infections_not_isolated_omicron"), which(STATE_NAMES == "00_latent_infections_isolated_omicron") ),
                                                   1,
                                                   prob = c((1 - CONST_q_omicron), CONST_q_omicron))
                        }

                        # Check if Wild, Alpha, Beta, Gamma or Delta
                        else if(infection_WHO_label == "Wild" ||
                                infection_WHO_label == "Alpha" ||
                                infection_WHO_label == "Beta" ||
                                infection_WHO_label == "Gamma" ||
                                infection_WHO_label == "Delta" ){
                          next_state_id <- sample( c( which(STATE_NAMES == "00_latent_infections_not_isolated"), which(STATE_NAMES == "00_latent_infections_isolated") ),
                                                   1,
                                                   prob = c((1 - CONST_q), CONST_q))
                        }

                        # Else throw warning on console output
                        else {
                          # Debug:
                          cat("[WARNING] Cannot find post-infection state for infectee_id: " ,  contact_index, ", with variant: ", infection_hist_mat[contact_index, "variant"], "\n", sep = "")
                        }

                      }

                      # Infection event for person with 1 dose
                      else if (PREV_STATE_NAME == "received_dose1"){
                        # Check if omicron
                        if(infection_WHO_label == "Omicron") {
                          next_state_id <- sample( c( which(STATE_NAMES == "01_latent_infections_not_isolated_omicron"), which(STATE_NAMES == "01_latent_infections_isolated_omicron") ),
                                                   1,
                                                   prob = c((1 - CONST_q_omicron), CONST_q_omicron))
                        }

                        # Check if Wild, Alpha, Beta, Gamma or Delta
                        else if(infection_WHO_label == "Wild" ||
                                infection_WHO_label == "Alpha" ||
                                infection_WHO_label == "Beta" ||
                                infection_WHO_label == "Gamma" ||
                                infection_WHO_label == "Delta" ){
                          next_state_id <- sample( c( which(STATE_NAMES == "01_latent_infections_not_isolated"), which(STATE_NAMES == "01_latent_infections_isolated") ),
                                                   1,
                                                   prob = c((1 - CONST_q), CONST_q))
                        }

                        # Else throw warning on console output
                        else {
                          # Debug:
                          cat("[WARNING] Cannot find post-infection state for person_id: ",  person_id, ", with infectee_id: " ,  contact_index, ", with variant: ", infection_hist_mat[contact_index, "variant"], "\n", sep = "")
                        }
                      }


                      # Infection event for person with 2 doses
                      else if (PREV_STATE_NAME == "received_dose2"){
                        # Check if omicron
                        if(infection_WHO_label == "Omicron") {
                          next_state_id <- sample( c( which(STATE_NAMES == "02_latent_infections_not_isolated_omicron"), which(STATE_NAMES == "02_latent_infections_isolated_omicron") ),
                                                   1,
                                                   prob = c((1 - CONST_q_omicron), CONST_q_omicron))
                        }

                        # Check if Wild, Alpha, Beta, Gamma or Delta
                        else if(infection_WHO_label == "Wild" ||
                                infection_WHO_label == "Alpha" ||
                                infection_WHO_label == "Beta" ||
                                infection_WHO_label == "Gamma" ||
                                infection_WHO_label == "Delta" ){
                          next_state_id <- sample( c( which(STATE_NAMES == "02_latent_infections_not_isolated"), which(STATE_NAMES == "02_latent_infections_isolated") ),
                                                   1,
                                                   prob = c((1 - CONST_q), CONST_q))
                        }

                        # Else throw warning on console output
                        else {
                          # Debug:
                          cat("[WARNING] Cannot find post-infection state for person_id: ",  person_id, ", with infectee_id: " ,  contact_index, ", with variant: ", infection_hist_mat[contact_index, "variant"], "\n", sep = "")
                        }
                      }

                      # Infection event for person with 3 doses
                      else if (PREV_STATE_NAME == "received_dose3"){
                        # Check if omicron
                        if(infection_WHO_label == "Omicron") {
                          next_state_id <- sample( c( which(STATE_NAMES == "03_latent_infections_not_isolated_omicron"), which(STATE_NAMES == "03_latent_infections_isolated_omicron") ),
                                                   1,
                                                   prob = c((1 - CONST_q_omicron), CONST_q_omicron))
                        }

                        # Check if Wild, Alpha, Beta or Delta
                        else if(infection_WHO_label == "Wild" ||
                                infection_WHO_label == "Alpha" ||
                                infection_WHO_label == "Beta" ||
                                infection_WHO_label == "Gamma" ||
                                infection_WHO_label == "Delta" ){

                          next_state_id <- sample( c( which(STATE_NAMES == "03_latent_infections_not_isolated"), which(STATE_NAMES == "03_latent_infections_isolated") ),
                                                   1,
                                                   prob = c((1 - CONST_q), CONST_q))
                        }

                        # Else throw warning on console output
                        else {
                          # Debug:
                          cat("[WARNING] Cannot find post-infection state for person_id: ",  person_id, ", with infectee_id: " ,  contact_index, ", with variant: ", infection_hist_mat[contact_index, "variant"], ", missing dose 3 state for non-omicron variants" ,"\n", sep = "")
                        }
                      }
                    } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR") {

                      # Infection event for un-vaccinated
                      if (PREV_STATE_NAME == "susceptible"){
                        next_state_id <- which(STATE_NAMES == "infected")
                      }
                      else {
                        # Debug:
                        cat("[WARNING] Cannot find post-infection state for person_id: ",  person_id, ", with infectee_id: " ,  contact_index, ", with variant: ", infection_hist_mat[contact_index, "variant"], "\n", sep = "")
                      }

                    } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation") {

                      # Infection event for un-vaccinated
                      if (PREV_STATE_NAME == "susceptible"){
                        next_state_id <- which(STATE_NAMES == "(unconfirmed) infected")
                      }
                      else {
                        # Debug:
                        cat("[WARNING] Cannot find post-infection state for person_id: ",  person_id, ", with infectee_id: " ,  contact_index, ", with variant: ", infection_hist_mat[contact_index, "variant"], "\n", sep = "")
                      }

                    } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") {

                      # Infection event for un-vaccinated
                      if (PREV_STATE_NAME == "susceptible"){
                        next_state_id <- which(STATE_NAMES == "(unconfirmed) infected")
                      }
                      else {
                        # Debug:
                        cat("[WARNING] Cannot find post-infection state for person_id: ",  person_id, ", with infectee_id: " ,  contact_index, ", with variant: ", infection_hist_mat[contact_index, "variant"], "\n", sep = "")
                      }

                    } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {
                      
                      # Infection event for un-vaccinated
                      if (PREV_STATE_NAME == "susceptible"){
                        next_state_id <- which(STATE_NAMES == "(unconfirmed) infected")
                        
                      } else {
                        # Debug:
                        cat("\n[WARNING] Cannot find post-infection state for person_id: ",  susceptible_agent_id, " (", PREV_STATE_NAME, "), with infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], "\n", sep = "")
                      }
                      
                    } else {

                      cat("\n [WARNING] Iterating over susceptibles, Infectious state assignment for Aggregated Contact Matrix has not been implemented for disease model: ", CONST_CHOSEN_DISEASE_MODEL)
                    }

                    ALREADY_INFECTED <- TRUE
                    moved_to_non_susceptible_today <- c(moved_to_non_susceptible_today, person_id)


                    # Book keeping
                    infection_hist_mat[person_id, "variant"] <- toString(infection_hist_mat[contact_index, "variant"])
                    infection_hist_mat[person_id, "infected on"] <- day_index
                    infection_hist_mat[person_id, "infected by"] <- contact_index

                    # Debug:
                    # Bookkeeping with variants
                    # cat("           Person: ", person_id,
                    #    ", infected with ", infection_hist_mat[person_id, "variant"],
                    #    ", by person: ", contact_index,
                    #    ", on sim day: ", day_index, "\n")

                  } # End of Single infection state change with book-keeping - For aggregated contact times

                } else if (CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Location based"){

                  # Loop over all the contact times with this particular infectious contact
                  for (contact_event_index in 1:length(list_of_contact_times))  {

                    temp_damp_for_commercial = 1.0


                    contact_event_location_type = levels(list_of_contact_location_types[contact_event_index])[list_of_contact_location_types[contact_event_index]]
                    contact_event_location_id = list_of_contact_location_ids[contact_event_index]


                    if(contact_event_location_type  == "Commercial"){
                      temp_damp_for_commercial = CONST_DAMP_FOR_COMMERCIAL
                      # Debug
                      # cat("\n         damping ", levels(list_of_contact_location_types[contact_event_index])[list_of_contact_location_types[contact_event_index]],
                      #     " type contact for susceptible agent id: ", person_id,
                      #     " with infectious agent id: ", person_specific_contact_indices[contact_event_index], sep ="")
                    }

                    if(ALREADY_INFECTED) { break }

                    # cat("\nDebug here")
                    random_coin_toss <- runif(1)
                    COUNT_NO_OF_INFECTION_COIN_FLIPS = COUNT_NO_OF_INFECTION_COIN_FLIPS + 1

                    this_infection_chance <- getInfection_probability_w_variant_w_vaccine(vaccine_odds, relative_infectivity, list_of_contact_times[contact_event_index])

                    # cat("\n Infection chance: ", this_infection_chance)
                    this_infection_chance <- this_infection_chance * temp_damp_for_commercial
                    # cat(" modified infection chance for ",
                    #     levels(list_of_contact_location_types[contact_event_index])[list_of_contact_location_types[contact_event_index]],
                    #     " : ", this_infection_chance)

                    if( random_coin_toss <= this_infection_chance ) {

                      # Debug:
                      COUNT_NO_OF_INFECTION_EVENTS = COUNT_NO_OF_INFECTION_EVENTS + 1

                      infection_WHO_label <- active_voc_mat[which(active_voc_mat[ , "variant"] == infection_hist_mat[contact_index, "variant"]), "WHO label"]


                      if (CONST_CHOSEN_DISEASE_MODEL == "Complex"){
                        # Infection event for un-vaccinated
                        if (PREV_STATE_NAME == "susceptible"){
                          # Check if omicron
                          if(infection_WHO_label == "Omicron") {
                            next_state_id <- sample( c( which(STATE_NAMES == "00_latent_infections_not_isolated_omicron"), which(STATE_NAMES == "00_latent_infections_isolated_omicron") ),
                                                     1,
                                                     prob = c((1 - CONST_q_omicron), CONST_q_omicron))
                          }

                          # Check if Wild, Alpha, Beta, Gamma or Delta
                          else if(infection_WHO_label == "Wild" ||
                                  infection_WHO_label == "Alpha" ||
                                  infection_WHO_label == "Beta" ||
                                  infection_WHO_label == "Gamma" ||
                                  infection_WHO_label == "Delta" ){
                            next_state_id <- sample( c( which(STATE_NAMES == "00_latent_infections_not_isolated"), which(STATE_NAMES == "00_latent_infections_isolated") ),
                                                     1,
                                                     prob = c((1 - CONST_q), CONST_q))
                          }

                          # Else throw warning on console output
                          else {
                            # Debug:
                            cat("[WARNING] Cannot find post-infection state for infectee_id: " ,  contact_index, ", with variant: ", infection_hist_mat[contact_index, "variant"], "\n", sep = "")
                          }

                        }

                        # Infection event for person with 1 dose
                        else if (PREV_STATE_NAME == "received_dose1"){
                          # Check if omicron
                          if(infection_WHO_label == "Omicron") {
                            next_state_id <- sample( c( which(STATE_NAMES == "01_latent_infections_not_isolated_omicron"), which(STATE_NAMES == "01_latent_infections_isolated_omicron") ),
                                                     1,
                                                     prob = c((1 - CONST_q_omicron), CONST_q_omicron))
                          }

                          # Check if Wild, Alpha, Beta, Gamma or Delta
                          else if(infection_WHO_label == "Wild" ||
                                  infection_WHO_label == "Alpha" ||
                                  infection_WHO_label == "Beta" ||
                                  infection_WHO_label == "Gamma" ||
                                  infection_WHO_label == "Delta" ){
                            next_state_id <- sample( c( which(STATE_NAMES == "01_latent_infections_not_isolated"), which(STATE_NAMES == "01_latent_infections_isolated") ),
                                                     1,
                                                     prob = c((1 - CONST_q), CONST_q))
                          }

                          # Else throw warning on console output
                          else {
                            # Debug:
                            cat("[WARNING] Cannot find post-infection state for person_id: ",  person_id, ", with infectee_id: " ,  contact_index, ", with variant: ", infection_hist_mat[contact_index, "variant"], "\n", sep = "")
                          }
                        }


                        # Infection event for person with 2 doses
                        else if (PREV_STATE_NAME == "received_dose2"){
                          # Check if omicron
                          if(infection_WHO_label == "Omicron") {
                            next_state_id <- sample( c( which(STATE_NAMES == "02_latent_infections_not_isolated_omicron"), which(STATE_NAMES == "02_latent_infections_isolated_omicron") ),
                                                     1,
                                                     prob = c((1 - CONST_q_omicron), CONST_q_omicron))
                          }

                          # Check if Wild, Alpha, Beta, Gamma or Delta
                          else if(infection_WHO_label == "Wild" ||
                                  infection_WHO_label == "Alpha" ||
                                  infection_WHO_label == "Beta" ||
                                  infection_WHO_label == "Gamma" ||
                                  infection_WHO_label == "Delta" ){
                            next_state_id <- sample( c( which(STATE_NAMES == "02_latent_infections_not_isolated"), which(STATE_NAMES == "02_latent_infections_isolated") ),
                                                     1,
                                                     prob = c((1 - CONST_q), CONST_q))
                          }

                          # Else throw warning on console output
                          else {
                            # Debug:
                            cat("[WARNING] Cannot find post-infection state for person_id: ",  person_id, ", with infectee_id: " ,  contact_index, ", with variant: ", infection_hist_mat[contact_index, "variant"], "\n", sep = "")
                          }
                        }

                        # Infection event for person with 3 doses
                        else if (PREV_STATE_NAME == "received_dose3"){
                          # Check if omicron
                          if(infection_WHO_label == "Omicron") {
                            next_state_id <- sample( c( which(STATE_NAMES == "03_latent_infections_not_isolated_omicron"), which(STATE_NAMES == "03_latent_infections_isolated_omicron") ),
                                                     1,
                                                     prob = c((1 - CONST_q_omicron), CONST_q_omicron))
                          }

                          # Check if Wild, Alpha, Beta or Delta
                          else if(infection_WHO_label == "Wild" ||
                                  infection_WHO_label == "Alpha" ||
                                  infection_WHO_label == "Beta" ||
                                  infection_WHO_label == "Gamma" ||
                                  infection_WHO_label == "Delta" ){
                            next_state_id <- sample( c( which(STATE_NAMES == "03_latent_infections_not_isolated"), which(STATE_NAMES == "03_latent_infections_isolated") ),
                                                     1,
                                                     prob = c((1 - CONST_q), CONST_q))
                          }

                          # Else throw warning on console output
                          else {
                            # Debug:
                            cat("[WARNING] Cannot find post-infection state for person_id: ",  person_id, ", with infectee_id: " ,  contact_index, ", with variant: ", infection_hist_mat[contact_index, "variant"], ", missing dose 3 state for non-omicron variants" ,"\n", sep = "")
                          }
                        }
                      } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR") {

                        # Infection event for un-vaccinated
                        if (PREV_STATE_NAME == "susceptible"){
                          next_state_id <- which(STATE_NAMES == "infected")
                        }
                        else {
                          # Debug:
                          cat("[WARNING] Cannot find post-infection state for person_id: ",  person_id, ", with infectee_id: " ,  contact_index, ", with variant: ", infection_hist_mat[contact_index, "variant"], "\n", sep = "")
                        }

                      } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation") {

                        # Infection event for un-vaccinated
                        if (PREV_STATE_NAME == "susceptible"){
                          next_state_id <- which(STATE_NAMES == "(unconfirmed) infected")
                        }
                        else {
                          # Debug:
                          cat("[WARNING] Cannot find post-infection state for person_id: ",  person_id, ", with infectee_id: " ,  contact_index, ", with variant: ", infection_hist_mat[contact_index, "variant"], "\n", sep = "")
                        }

                      } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") {

                        # Infection event for un-vaccinated
                        if (PREV_STATE_NAME == "susceptible"){
                          next_state_id <- which(STATE_NAMES == "(unconfirmed) infected")
                        }
                        else {
                          # Debug:
                          cat("[WARNING] Cannot find post-infection state for person_id: ",  person_id, ", with infectee_id: " ,  contact_index, ", with variant: ", infection_hist_mat[contact_index, "variant"], "\n", sep = "")
                        }

                      } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {
                        
                        # Infection event for un-vaccinated
                        if (PREV_STATE_NAME == "susceptible"){
                          next_state_id <- which(STATE_NAMES == "(unconfirmed) infected")
                          
                        } else {
                          # Debug:
                          cat("\n[WARNING] Cannot find post-infection state for person_id: ",  susceptible_agent_id, " (", PREV_STATE_NAME, "), with infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], "\n", sep = "")
                        }
                        
                      } else {

                        cat("\n [WARNING] Iterating over susceptibles, Infectious state assignment for Locationn based Contact Matrix has not been implemented for disease model: ", CONST_CHOSEN_DISEASE_MODEL)
                      }

                      ALREADY_INFECTED <- TRUE
                      moved_to_non_susceptible_today <- c(moved_to_non_susceptible_today, person_id)


                      # Book keeping
                      infection_hist_mat[person_id, "variant"] <- toString(infection_hist_mat[contact_index, "variant"])
                      infection_hist_mat[person_id, "infected on"] <- day_index
                      infection_hist_mat[person_id, "infected by"] <- contact_index
                      infection_hist_mat[person_id, "citisketch_location_type"] <- contact_event_location_type
                      infection_hist_mat[person_id, "citisketch_location_id"] <- contact_event_location_id

                      # Debug:
                      # Bookkeeping with variants
                      # cat("           Person: ", person_id,
                      #    ", infected with ", infection_hist_mat[person_id, "variant"],
                      #    ", by person: ", contact_index,
                      #    ", on sim day: ", day_index, "\n")

                    }  # End of Single infection state change with book-keeping - For location wise contact times

                  }
                }

              }
            }

            STATE[person_id, day_index] <- next_state_id
            next_state_name <- STATE_NAMES[next_state_id]

          }
          else {
            cat("\n      Found person id:", person_id, " in state id: ", PREV_STATE_INDEX, "'", PREV_STATE_NAME, "' with no contact matrix based or probabilistic transition")
            cat("\n      Possible Error, Ending simulation")
            stop()
          }
        }
      }
      # Tag: @calib06
      # Check contacts of infectious agents to infect
      else {

        # Debug:
        # cat("   \n Iterating on Infectors")

        # Tag: @Vaccination Rates "Did anyone get vaccinated yesterday?"
        if (CONST_CHOSEN_DISEASE_MODEL == "Complex"){
          for (person_id in previous_day_susceptible_person_ids){

            # Get state of this person from previous day
            PREV_STATE_INDEX <- as.numeric(STATE[person_id, day_index - 1])
            PREV_STATE_NAME <- STATE_NAMES[PREV_STATE_INDEX] # Note: At this point STATE_NAME is a matrix, by the end of execution it's converted to df for CSV IO

            # Vaccination state changes during simulation time
            # -------------------
            # Check if the person may have been vaccinated last simulation day
            # i.e. is in succeptible or received_dose_1 state
            # This breaks new evolution function: NO_OF_VACCINATION_DATA_BASED_TRANSITION <- length(which( POSSIBLE_TRANSITIONS_mat[ , "transition_weights"] == "CALCULATE_FROM_VACCINATION_DATA"))

            this_vaccination_chance = runif(1)

            if(PREV_STATE_NAME == "susceptible" || PREV_STATE_NAME == "received_dose1"){

              # Get vaccination data and roll dice for possible vaccination last day
              vaccination_day_index <- day_index - 1

              # Re-use last available vaccination rates
              if(vaccination_day_index > nrow(vaccination_perday_mat)) {
                vaccination_day_index <- nrow(vaccination_perday_mat)
              }

              if(PREV_STATE_NAME == "susceptible" &&
                 vaccination_perday_mat[vaccination_day_index , "dose1"] > 0){

                dose1_prob <- vaccination_perday_mat[vaccination_day_index , "dose1"] / CONST_NB_POP

                # Force negative fractions to 0, to counteract artifacts in the data
                if(dose1_prob < 0) { dose1_prob = 0 }

                else if( this_vaccination_chance <= dose1_prob){
                  prev_state_id <- which( colnames(transition_matrix) == "received_dose1" )

                  # Set state
                  STATE[person_id, day_index - 1] <- prev_state_id

                  # Book keeping
                  infection_hist_mat[person_id, "dose 1 on"] <- day_index - 1
                }
              }

              if(PREV_STATE_NAME == "received_dose1" &&
                 vaccination_perday_mat[vaccination_day_index , "dose2"] > 0 &&
                 (day_index - strtoi(infection_hist_mat[[person_id, "dose 1 on"]])) >= CONST_MIN_DAYS_TILL_SECOND_DOSE){

                dose2_prob <- vaccination_perday_mat[vaccination_day_index , "dose2"] / CONST_NB_POP

                # Force negative fractions to 0, to counteract artifacts in the data
                if(dose2_prob < 0) { dose2_prob = 0 }

                else if( this_vaccination_chance <= dose2_prob){
                  prev_state_id <- which( colnames(transition_matrix) == "received_dose2" )

                  # Set state
                  STATE[person_id, day_index - 1] <- prev_state_id

                  # Book keeping
                  infection_hist_mat[person_id, "dose 2 on"] <- day_index - 1
                }
              }


              # Reload states
              # PREV_STATE_INDEX <- as.numeric(STATE[person_id, day_index - 1])
              # PREV_STATE_NAME <- STATE_NAMES[PREV_STATE_INDEX]

            } # End of last days vaccination prob
          }
        }


        # Tag: @calib10
        # For models: "SIR + Testing + Isolation", "SIR + Testing + Isolation + Contact Tracing"
        # Queue voluntary tests: "Did anyone request test yesterday?"
        if ( (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation") ||
             (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") ||
             (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing")){

          # Generate voluntary test request from: Infectious and symptomatic agents
          # Note: for other models need to implement statelist (STATELIST_SYMPTOMATIC_INFECTIOUS_TEST_SEEKER) based helper functions

          list_symptomatic_infectious_test_seeker_ids <- previous_day_infectious_person_ids
          count_total_seeker <- length(list_symptomatic_infectious_test_seeker_ids)
          # Draw which of the agents requests for test
          sampled_indices_of_test_seeker_ids <- which(runif(count_total_seeker) < rep(CONST_SYMPTOMATIC_INFECTIOUS_TEST_CHANCE, count_total_seeker))
          # Set as empty list
          list_symptomatic_infectious_test_seeker_ids_yesterday <- c()

          # Populate based on sampling
          if(length(sampled_indices_of_test_seeker_ids) > 0){
            list_symptomatic_infectious_test_seeker_ids_yesterday <- list_symptomatic_infectious_test_seeker_ids[sampled_indices_of_test_seeker_ids]
            
            # Debug: 
            # cat("\n Sampled symptomatic test sekers: ", length(sampled_indices_of_test_seeker_ids), "/", length(previous_day_infectious_person_ids), " : ", length(sampled_indices_of_test_seeker_ids) / length(previous_day_infectious_person_ids))
          }


          # Generate voluntary test request from: Asymptomatic agents
          # Note: for other models need to implement statelist (STATELIST_ASYMPTOMATIC_INFECTIOUS_TEST_SEEKER, STATELIST_NOT_INFECTED_TEST_SEEKER) based helper functions
          # Note: for vaccination based model need to exclude agents who were vaccinated yesterday from STATELIST_NOT_INFECTED_TEST_SEEKER
          list_asymptomatic_test_seeker_ids <- previous_day_susceptible_person_ids
          count_total_seeker <- length(list_asymptomatic_test_seeker_ids)
          # Draw which of the agents requests for test
          sampled_indices_of_test_seeker_ids <- which(runif(count_total_seeker) < rep(CONST_ASYMPTOMATIC_TEST_CHANCE, count_total_seeker))
          # Set as empty list
          list_asymptomatic_test_seeker_ids_yesterday <- c()

          # Populate based on sampling
          if(length(sampled_indices_of_test_seeker_ids) > 0){
            list_asymptomatic_test_seeker_ids_yesterday <- list_asymptomatic_test_seeker_ids[sampled_indices_of_test_seeker_ids]
          }

          if(CONST_CHOSEN_TEST_ALLOCATION_STRATEGY == "Antigen test first"){

            # Default value:
            count_available_antigen_test <- 0
            count_available_molecular_test <- 0
            # cat("\n Debug here!!!")

            if(!identical(available_antigen_test_index, integer(0))){
              count_available_antigen_test <- available_covid_tests_count[ available_antigen_test_index ]
            }

            if(!identical(available_molecular_test_index, integer(0))){
              count_available_molecular_test <- available_covid_tests_count[ available_molecular_test_index  ]
            }

            all_covid_test_requesting_ids_yesterday <- union(list_symptomatic_infectious_test_seeker_ids_yesterday, list_asymptomatic_test_seeker_ids_yesterday)
            total_requested_covid_test <- length(all_covid_test_requesting_ids_yesterday)

            # Case 1, 2: No. of antigen tests are equal or greater than requested no. of covid test
            if(count_available_antigen_test >= total_requested_covid_test){
              test_schedule_mat[all_covid_test_requesting_ids_yesterday, "test type"] <- "antigen"
              test_schedule_mat[all_covid_test_requesting_ids_yesterday, "test scheduled on"] <- day_index - 1
              test_schedule_mat[all_covid_test_requesting_ids_yesterday, "result day"] <- day_index - 1 + floor(available_covid_test_result_delays[available_antigen_test_index])

              # Book-keeping:
              available_covid_tests_count[ available_antigen_test_index ] <- available_covid_tests_count[ available_antigen_test_index ] - total_requested_covid_test

            } else{
              # Case 3: No. of antigen tests are less than requested no. of covid test

              # Case 3.1, 3.2: No. of molecular test are equal or greater than difference
              count_overflow_antigent_test <- total_requested_covid_test - count_available_antigen_test
              if(count_available_molecular_test >= (count_overflow_antigent_test)){

                if(count_available_antigen_test >= 1){
                  # Schedule as many antigen test possible
                  test_schedule_mat[all_covid_test_requesting_ids_yesterday[1:count_available_antigen_test], "test type"] <- "antigen"
                  test_schedule_mat[all_covid_test_requesting_ids_yesterday[1:count_available_antigen_test], "test scheduled on"] <- day_index -1
                  test_schedule_mat[all_covid_test_requesting_ids_yesterday[1:count_available_antigen_test], "result day"] <- day_index - 1 + floor(available_covid_test_result_delays[available_antigen_test_index])
                  # Book-keeping:
                  available_covid_tests_count[ available_antigen_test_index ] <- 0 #available_covid_tests_count[ available_antigen_test_index ] - count_available_antigen_test
                }

                # Schedule the rest as molecular test
                test_schedule_mat[all_covid_test_requesting_ids_yesterday[(count_available_antigen_test + 1):length(all_covid_test_requesting_ids_yesterday)], "test type"] <- "rapid molecular"
                test_schedule_mat[all_covid_test_requesting_ids_yesterday[(count_available_antigen_test + 1):length(all_covid_test_requesting_ids_yesterday)], "test scheduled on"] <- day_index - 1
                test_schedule_mat[all_covid_test_requesting_ids_yesterday[(count_available_antigen_test + 1):length(all_covid_test_requesting_ids_yesterday)], "result day"] <- day_index - 1 + floor(available_covid_test_result_delays[available_molecular_test_index])
                # Book-keeping:
                available_covid_tests_count[ available_molecular_test_index ] <- available_covid_tests_count[ available_molecular_test_index ] - count_overflow_antigent_test

              } else{
                # Case 3.3: No. of molecular tests is less than the difference
                # Randomly choose from test requests up to cumulative available total no. tests
                # i.e. assuming voluntary testing capacity is capped, in other words some agents may not be able to test due to lack of availability
                # Not scheduling for the next day or subsequent days
                sampled_covid_test_requesting_ids_yesterday <- sample(all_covid_test_requesting_ids_yesterday, sum_available_covid_tests_count)

                # Schedule tests
                if(count_available_antigen_test >= 1){
                  # Schedule as many antigen test possible
                  test_schedule_mat[sampled_covid_test_requesting_ids_yesterday[1:count_available_antigen_test], "test type"] <- "antigen"
                  test_schedule_mat[sampled_covid_test_requesting_ids_yesterday[1:count_available_antigen_test], "test scheduled on"] <- day_index - 1
                  test_schedule_mat[sampled_covid_test_requesting_ids_yesterday[1:count_available_antigen_test], "result day"] <- day_index - 1 + floor(available_covid_test_result_delays[available_antigen_test_index])
                  # Book-keeping:
                  available_covid_tests_count[ available_antigen_test_index ] <- 0 #available_covid_tests_count[ available_antigen_test_index ] - count_available_antigen_test
                }
                # Schedule the rest as molecular test
                test_schedule_mat[sampled_covid_test_requesting_ids_yesterday[(count_available_antigen_test + 1):length(sampled_covid_test_requesting_ids_yesterday)], "test type"] <- "rapid molecular"
                test_schedule_mat[sampled_covid_test_requesting_ids_yesterday[(count_available_antigen_test + 1):length(sampled_covid_test_requesting_ids_yesterday)], "test scheduled on"] <- day_index - 1
                test_schedule_mat[sampled_covid_test_requesting_ids_yesterday[(count_available_antigen_test + 1):length(sampled_covid_test_requesting_ids_yesterday)], "result day"] <- day_index - 1 + floor(available_covid_test_result_delays[available_molecular_test_index])
                # Book-keeping:
                available_covid_tests_count[ available_molecular_test_index ] <- 0 #available_covid_tests_count[ available_molecular_test_index ] - count_overflow_antigent_test

              }

            }

          }

          # Apply "Waiting for result" State changes:
          test_requested_yesterday_ids <- which(test_schedule_mat[ , "test scheduled on"] == day_index - 1)

          # Filter states only for test requesting ids
          previous_day_test_requested_df <- subset(previous_day_df, as.numeric(rownames(previous_day_df)) %in% test_requested_yesterday_ids)

          # Filter out not infected test seekers
          test_requested_yesterday_not_infected_ids <- as.numeric(rownames(subset(previous_day_test_requested_df, state_id %in% STATELIST_NOT_INFECTED_TEST_SEEKER)))

          if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){
            # Nothing to do
          } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") {
            # For SIR+++ process multiple non-infected test-seeker states, namely: susceptible and removed

            test_requested_yesterday_susceptible_ids <- as.numeric(rownames(subset(previous_day_test_requested_df, state_id == which(STATE_NAMES == "susceptible"))))
            test_requested_yesterday_removed_ids <- as.numeric(rownames(subset(previous_day_test_requested_df, state_id == which(STATE_NAMES == "removed"))))
          } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {
            # For augmented SIR with separate recovered and dead compartments,
            # process multiple non-infected test-seeker states, namely: susceptible and recovered

            test_requested_yesterday_susceptible_ids <- as.numeric(rownames(subset(previous_day_test_requested_df, state_id == which(STATE_NAMES == "susceptible"))))
            test_requested_yesterday_recovered_ids <- as.numeric(rownames(subset(previous_day_test_requested_df, state_id == which(STATE_NAMES == "recovered"))))
          }
          # Filter out symptomatic infectious test seekers
          test_requested_yesterday_symptomatic_infectious_ids <- as.numeric(rownames(subset(previous_day_test_requested_df, state_id %in% STATELIST_SYMPTOMATIC_INFECTIOUS_TEST_SEEKER)))
          # Filter out asymptomatic infectious test seekers
          test_requested_yesterday_asymptomatic_infectious_ids <- as.numeric(rownames(subset(previous_day_test_requested_df, state_id %in% STATELIST_ASYMPTOMATIC_INFECTIOUS_TEST_SEEKER)))


          # inconsistent_tesk_seeker_ids <- setdiff(test_requested_yesterday_ids, test_requested_yesterday_not_infected_ids)
          # inconsistent_tesk_seeker_ids <- setdiff(inconsistent_tesk_seeker_ids, test_requested_yesterday_symptomatic_infectious_ids)
          # inconsistent_tesk_seeker_ids <- setdiff(inconsistent_tesk_seeker_ids, test_requested_yesterday_asymptomatic_infectious_ids)
          # 
          # if(length(inconsistent_tesk_seeker_ids) > 0) {
          #   # cat("\n [WARNING] The following test seeking agents were dropped: ", inconsistent_tesk_seeker_ids)
          #   cat("\n [WARNING] Count of test seeking agents were dropped: ", length(inconsistent_tesk_seeker_ids), "\n" )
          #   print( as.data.frame(table(STATE_NAMES[STATE[inconsistent_tesk_seeker_ids, day_index - 1] ] )) )
          #   
          # }



          # Set "waiting for result" state retroactively
          if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){
            # For SIR++, the only non infected test seeker state is "susceptible"
            STATE[ test_requested_yesterday_not_infected_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) susceptible")
            # For SIR++, the infected test seeker state is "infected"
            STATE[ test_requested_yesterday_symptomatic_infectious_ids , day_index - 1] <- which(STATE_NAMES == "(isolated + unconfirmed) infected")
            # For SIR++, there are no asymptomatic test seekers only non infected test seeker state is "susceptible"
          } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){

            # For SIR+++ process multiple non-infected test-seeker states, namely: susceptible and removed
            STATE[ test_requested_yesterday_susceptible_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) susceptible")
            STATE[ test_requested_yesterday_removed_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) removed")

            # For SIR+++, the infected test seeker state is "infected"
            STATE[ test_requested_yesterday_symptomatic_infectious_ids , day_index - 1] <- which(STATE_NAMES == "(isolated + unconfirmed) infected")

            # For SIR+++, there are no asymptomatic and infectious test seekers
          } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){

            # For augmented SIR with separate recovered and dead compartments,
            # multiple non-infected test-seeker states, namely: susceptible and recovered
            STATE[ test_requested_yesterday_susceptible_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) susceptible")
            STATE[ test_requested_yesterday_recovered_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) recovered")

            # For SIR+++, the infected test seeker state is "infected"
            STATE[ test_requested_yesterday_symptomatic_infectious_ids , day_index - 1] <- which(STATE_NAMES == "(isolated + unconfirmed) infected")

            # For SIR+++, there are no asymptomatic and infectious test seekers
          }



          # Change infectious and susceptible sets (as per disease propagation model)

          if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){
            # Debug:
            # cat("\n----------Moving from susceptible to non-susceptible at Test Request: ", length(test_requested_yesterday_not_infected_ids) )
            # "Susceptible agents waiting for covid test result isolate themselves (and do not participate in disease propagation)
            previous_day_susceptible_person_ids <- setdiff(previous_day_susceptible_person_ids, test_requested_yesterday_not_infected_ids)
            previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, test_requested_yesterday_not_infected_ids)


          } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){
            previous_day_susceptible_person_ids <- setdiff(previous_day_susceptible_person_ids, test_requested_yesterday_susceptible_ids)
            previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, test_requested_yesterday_susceptible_ids)


          } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){
            previous_day_susceptible_person_ids <- setdiff(previous_day_susceptible_person_ids, test_requested_yesterday_susceptible_ids)
            previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, test_requested_yesterday_susceptible_ids)
          }

          # "Symptomatic Infectious agents waiting for covid test result isolate themselves (and do not participate in disease propagation)
          previous_day_infectious_person_ids <- setdiff(previous_day_infectious_person_ids, test_requested_yesterday_symptomatic_infectious_ids)

          # "Asymptomatic Infectious agents waiting for covid test result isolate themselves (and do not participate in disease propagation)
          previous_day_infectious_person_ids <- setdiff(previous_day_infectious_person_ids, test_requested_yesterday_asymptomatic_infectious_ids)



          # Update previous_day_df
          previous_day_df <- data.frame(STATE[ , day_index - 1])
          colnames(previous_day_df) <- c("state_id")
        }








        # Tag: @calib10
        # For models: "SIR + Testing + Isolation", "SIR + Testing + Isolation + Contact Tracing"
        # Generate test result scheduled: "Did anyone receive test result yesterday?"
        if ( (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation") ||
             (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") ||
             (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing")){
          result_requested_yesterday_ids <- which(test_schedule_mat[ , "result day"] == day_index - 1)

          COUNT_TOTAL_TEST_RESULTS_REQUESTED = length(result_requested_yesterday_ids)

          # Filter states only for result requesting ids
          previous_day_result_requested_df <- subset(previous_day_df, as.numeric(rownames(previous_day_df)) %in% result_requested_yesterday_ids)


          # Filter out not infected result seekers
          result_requested_yesterday_not_infected_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id %in% STATELIST_NOT_INFECTED_RESULT_SEEKER)))
          antigen_result_requested_yesterday_not_infected_ids <- result_requested_yesterday_not_infected_ids[ test_schedule_mat[result_requested_yesterday_not_infected_ids, "test type"] == "antigen" ]
          molecular_result_requested_yesterday_not_infected_ids <- result_requested_yesterday_not_infected_ids[ test_schedule_mat[result_requested_yesterday_not_infected_ids, "test type"] == "rapid molecular" ]
          if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){
            # Nothing to do
          } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){
            # Filter out "(isolated) susceptible"
            result_requested_yesterday_susceptible_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id == which(STATE_NAMES == "(isolated) susceptible"))))
            antigen_result_requested_yesterday_susceptible_ids <- result_requested_yesterday_susceptible_ids[ test_schedule_mat[result_requested_yesterday_susceptible_ids, "test type"] == "antigen" ]
            molecular_result_requested_yesterday_susceptible_ids <- result_requested_yesterday_susceptible_ids[ test_schedule_mat[result_requested_yesterday_susceptible_ids, "test type"] == "rapid molecular" ]

            # Filter out "(isolated) removed"
            result_requested_yesterday_removed_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id == which(STATE_NAMES == "(isolated) removed"))))
            antigen_result_requested_yesterday_removed_ids <- result_requested_yesterday_removed_ids[ test_schedule_mat[result_requested_yesterday_removed_ids, "test type"] == "antigen" ]
            molecular_result_requested_yesterday_removed_ids <- result_requested_yesterday_removed_ids[ test_schedule_mat[result_requested_yesterday_removed_ids, "test type"] == "rapid molecular" ]


          } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){
            # Filter out "(isolated) susceptible"
            result_requested_yesterday_susceptible_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id == which(STATE_NAMES == "(isolated) susceptible"))))
            antigen_result_requested_yesterday_susceptible_ids <- result_requested_yesterday_susceptible_ids[ test_schedule_mat[result_requested_yesterday_susceptible_ids, "test type"] == "antigen" ]
            molecular_result_requested_yesterday_susceptible_ids <- result_requested_yesterday_susceptible_ids[ test_schedule_mat[result_requested_yesterday_susceptible_ids, "test type"] == "rapid molecular" ]

            # Filter out "(isolated) recovered"
            result_requested_yesterday_recovered_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id == which(STATE_NAMES == "(isolated) recovered"))))
            antigen_result_requested_yesterday_recovered_ids <- result_requested_yesterday_recovered_ids[ test_schedule_mat[result_requested_yesterday_recovered_ids, "test type"] == "antigen" ]
            molecular_result_requested_yesterday_recovered_ids <- result_requested_yesterday_recovered_ids[ test_schedule_mat[result_requested_yesterday_recovered_ids, "test type"] == "rapid molecular" ]
          }

          # Filter out symptomatic infectious result seeker
          result_requested_yesterday_symptomatic_infectious_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id %in% STATELIST_SYMPTOMATIC_INFECTIOUS_RESULT_SEEKER)))
          antigen_result_requested_yesterday_symptomatic_infectious_ids <- result_requested_yesterday_symptomatic_infectious_ids[ test_schedule_mat[result_requested_yesterday_symptomatic_infectious_ids, "test type"] == "antigen" ]
          molecular_result_requested_yesterday_symptomatic_infectious_ids <- result_requested_yesterday_symptomatic_infectious_ids[ test_schedule_mat[result_requested_yesterday_symptomatic_infectious_ids, "test type"] == "rapid molecular" ]

          # Filter out asymptomatic infectious result seeker
          result_requested_yesterday_asymptomatic_infectious_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id %in% STATELIST_ASYMPTOMATIC_INFECTIOUS_RESULT_SEEKER)))
          antigen_result_requested_yesterday_asymptomatic_infectious_ids <- result_requested_yesterday_asymptomatic_infectious_ids[ test_schedule_mat[result_requested_yesterday_asymptomatic_infectious_ids, "test type"] == "antigen" ]
          molecular_result_requested_yesterday_asymptomatic_infectious_ids <- result_requested_yesterday_asymptomatic_infectious_ids[ test_schedule_mat[result_requested_yesterday_asymptomatic_infectious_ids, "test type"] == "rapid molecular" ]




          # Generate covid test "result"

          result_state_set_not_infected <- c()
          result_state_set_symptomatic_infectious <- c()
          result_state_set_asymptomatic_infectious <- c()

          # Construct covid test result set for each model
          if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){
            result_state_set_not_infected <- c( which(STATE_NAMES == "susceptible"), which(STATE_NAMES == "(isolated + false positive) susceptible") )
            result_state_set_symptomatic_infectious <- c( which(STATE_NAMES == "(isolated + confirmed) infected"), which(STATE_NAMES == "(unconfirmed) infected") )

            result_state_positive <- c( which(STATE_NAMES == "(isolated + false positive) susceptible"), which(STATE_NAMES == "(isolated + confirmed) infected") )
            result_state_negative <- c( which(STATE_NAMES == "susceptible"), which(STATE_NAMES == "(unconfirmed) infected") )

          } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){

            result_state_set_susceptible <- c( which(STATE_NAMES == "susceptible"), which(STATE_NAMES == "(isolated + false positive) susceptible") )
            result_state_set_removed <- c( which(STATE_NAMES == "removed"), which(STATE_NAMES == "(isolated + false positive) removed") )

            result_state_set_symptomatic_infectious <- c( which(STATE_NAMES == "(isolated + confirmed) infected"), which(STATE_NAMES == "(unconfirmed) infected") )

            result_state_positive <- c( which(STATE_NAMES == "(isolated + false positive) susceptible"),
                                        which(STATE_NAMES == "(isolated + confirmed) infected"),
                                        which(STATE_NAMES == "(isolated + false positive) removed"))

            result_state_negative <- c( which(STATE_NAMES == "susceptible"),
                                        which(STATE_NAMES == "(unconfirmed) infected"),
                                        which(STATE_NAMES == "removed"))


          } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){

            result_state_set_susceptible <- c( which(STATE_NAMES == "susceptible"), which(STATE_NAMES == "(isolated + false positive) susceptible") )
            result_state_set_recovered <- c( which(STATE_NAMES == "recovered"), which(STATE_NAMES == "(isolated + false positive) recovered") )

            result_state_set_symptomatic_infectious <- c( which(STATE_NAMES == "(isolated + confirmed) infected"), which(STATE_NAMES == "(unconfirmed) infected") )

            result_state_positive <- c( which(STATE_NAMES == "(isolated + false positive) susceptible"),
                                        which(STATE_NAMES == "(isolated + confirmed) infected"),
                                        which(STATE_NAMES == "(isolated + false positive) recovered"))

            result_state_negative <- c( which(STATE_NAMES == "susceptible"),
                                        which(STATE_NAMES == "(unconfirmed) infected"),
                                        which(STATE_NAMES == "recovered"))
          }

          # Get state change probability for not infected
          # Antigen - Asymptomatic: True Negative rate, Assuming the same parameters as asymptomatic selectivity
          asymptomatic_antigen_state_change_for_not_infected_prob <- c(asymptomatic_antigen_TNR, asymptomatic_antigen_FPR)

          # Rapid Molecular: True Negative rate
          molecular_state_change_for_not_infected_prob <- c(molecular_TNR, molecular_FPR)



          # Get test state change probability for infected
          # Antigen - Symptomatic: True positive rate
          symptomatic_antigen_state_change_for_infected_prob <- c(symptomatic_antigen_TPR, symptomatic_antigen_FNR)
          # Antigen - Asymptomatic: True positive rate
          asymptomatic_antigen_state_change_for_infected_prob <- c(asymptomatic_antigen_TPR, asymptomatic_antigen_FNR)

          # Rapid Molecular (Assuming same for symptomatic and asymptomatic): True positive Rate
          molecular_state_change_for_infected_prob <- c(molecular_TPR, molecular_FNR)


          # Book-keeping
          true_negative_sus_test_result_ids <- c()
          false_negative_test_result_ids <- c()

          # Tag: @calib11 - for contact tracing
          all_positive_test_result_ids <- c()

          # Calculate covid test results for each model
          # or process each Covid test based state transition
          # --------------------------

          # Antigen - Not infected : True Negative rate
          count_antigen_result_for_not_infected <- length(antigen_result_requested_yesterday_not_infected_ids)
          output_state_antigen_result_for_not_infected <- c()

          if(count_antigen_result_for_not_infected > 0){


            if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation"){
              # For SIR++, the only not_infected result seeking state is susceptible
              output_state_antigen_result_for_not_infected <- replicate(count_antigen_result_for_not_infected,
                                                                        sample(result_state_set_not_infected, 1,
                                                                               prob = asymptomatic_antigen_state_change_for_not_infected_prob) )

              # Change state
              STATE[antigen_result_requested_yesterday_not_infected_ids, day_index - 1] <- output_state_antigen_result_for_not_infected

              # Aggregate agent ids receiving positive result
              positive_result_ids <- antigen_result_requested_yesterday_not_infected_ids[output_state_antigen_result_for_not_infected %in% result_state_positive]

              # Aggregate agent ids receiving negative result
              negative_result_ids <- antigen_result_requested_yesterday_not_infected_ids[output_state_antigen_result_for_not_infected %in% result_state_negative]


            } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Contact Tracing"){

              susceptible_positive_result_ids <- c()
              susceptible_negative_result_ids <- c()
              # Count "(isolated) susceptible" result seeker
              count_antigen_result_for_susceptible <- length(antigen_result_requested_yesterday_susceptible_ids)
              output_state_antigen_result_for_susceptible <- c()
              if(count_antigen_result_for_susceptible > 0){

                # Calculate output
                output_state_antigen_result_for_susceptible <- replicate(count_antigen_result_for_susceptible,
                                                                         sample(result_state_set_susceptible, 1,
                                                                                prob = asymptomatic_antigen_state_change_for_not_infected_prob))

                # Change state
                STATE[antigen_result_requested_yesterday_susceptible_ids, day_index - 1] <- output_state_antigen_result_for_susceptible

                # Aggregate agent ids receiving positive result
                susceptible_positive_result_ids <- antigen_result_requested_yesterday_susceptible_ids[output_state_antigen_result_for_susceptible %in% result_state_positive]

                # Aggregate agent ids receiving negative result
                susceptible_negative_result_ids <- antigen_result_requested_yesterday_susceptible_ids[output_state_antigen_result_for_susceptible %in% result_state_negative]
              }

              removed_positive_result_ids <- c()
              removed_negative_result_ids <- c()
              # Count "(isolated) removed" result seeker
              count_antigen_result_for_removed <- length(antigen_result_requested_yesterday_removed_ids)
              output_state_antigen_result_for_removed <- c()
              if(count_antigen_result_for_removed > 0){

                # Calculate output
                output_state_antigen_result_for_removed <- replicate(count_antigen_result_for_removed,
                                                                     sample(result_state_set_removed, 1,
                                                                            prob = asymptomatic_antigen_state_change_for_not_infected_prob))

                # Change state
                STATE[antigen_result_requested_yesterday_removed_ids, day_index - 1] <- output_state_antigen_result_for_removed

                # Aggregate agent ids receiving positive result
                removed_positive_result_ids <- antigen_result_requested_yesterday_removed_ids[output_state_antigen_result_for_removed %in% result_state_positive]

                # Aggregate agent ids receiving negative result
                removed_negative_result_ids <- antigen_result_requested_yesterday_removed_ids[output_state_antigen_result_for_removed %in% result_state_negative]
              }

              # Aggregate positives and negative agent ids for book-keeping
              positive_result_ids <- c(susceptible_positive_result_ids, removed_positive_result_ids)
              negative_result_ids <- c(susceptible_negative_result_ids, removed_negative_result_ids)

            } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){

              susceptible_positive_result_ids <- c()
              susceptible_negative_result_ids <- c()
              # Count "(isolated) susceptible" result seeker
              count_antigen_result_for_susceptible <- length(antigen_result_requested_yesterday_susceptible_ids)
              output_state_antigen_result_for_susceptible <- c()
              if(count_antigen_result_for_susceptible > 0){

                # Calculate output
                output_state_antigen_result_for_susceptible <- replicate(count_antigen_result_for_susceptible,
                                                                         sample(result_state_set_susceptible, 1,
                                                                                prob = asymptomatic_antigen_state_change_for_not_infected_prob))

                # Change state
                STATE[antigen_result_requested_yesterday_susceptible_ids, day_index - 1] <- output_state_antigen_result_for_susceptible

                # Aggregate agent ids receiving positive result
                susceptible_positive_result_ids <- antigen_result_requested_yesterday_susceptible_ids[output_state_antigen_result_for_susceptible %in% result_state_positive]

                # Aggregate agent ids receiving negative result
                susceptible_negative_result_ids <- antigen_result_requested_yesterday_susceptible_ids[output_state_antigen_result_for_susceptible %in% result_state_negative]
              }

              recovered_positive_result_ids <- c()
              recovered_negative_result_ids <- c()
              # Count "(isolated) recovered" result seeker
              count_antigen_result_for_recovered <- length(antigen_result_requested_yesterday_recovered_ids)
              output_state_antigen_result_for_recovered <- c()
              if(count_antigen_result_for_recovered > 0){

                # Calculate output
                output_state_antigen_result_for_recovered <- replicate(count_antigen_result_for_recovered,
                                                                     sample(result_state_set_recovered, 1,
                                                                            prob = asymptomatic_antigen_state_change_for_not_infected_prob))

                # Change state
                STATE[antigen_result_requested_yesterday_recovered_ids, day_index - 1] <- output_state_antigen_result_for_recovered

                # Aggregate agent ids receiving positive result
                recovered_positive_result_ids <- antigen_result_requested_yesterday_recovered_ids[output_state_antigen_result_for_recovered %in% result_state_positive]

                # Aggregate agent ids receiving negative result
                recovered_negative_result_ids <- antigen_result_requested_yesterday_recovered_ids[output_state_antigen_result_for_recovered %in% result_state_negative]
              }

              # Aggregate positives and negative agent ids for book-keeping
              positive_result_ids <- c(susceptible_positive_result_ids, recovered_positive_result_ids)
              negative_result_ids <- c(susceptible_negative_result_ids, recovered_negative_result_ids)

            }


            # Update test scheduling data structure
            test_schedule_mat[positive_result_ids, "result"] <- "positive"
            test_schedule_mat[negative_result_ids, "result"] <- "negative"

            # Book-keeping
            if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){
              true_negative_sus_test_result_ids <- c(true_negative_sus_test_result_ids, negative_result_ids)
            } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){
              true_negative_sus_test_result_ids <- c(true_negative_sus_test_result_ids, susceptible_negative_result_ids)
            } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){
              true_negative_sus_test_result_ids <- c(true_negative_sus_test_result_ids, susceptible_negative_result_ids)
            }

            all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)

            # Edge: "(isolated) susceptible/removed" -- Test Result (FPR) --> (isolated + false positive) susceptible/removed
            COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)
          }

          # Rapid molecular - Not infected : True Negative rate
          count_molecular_result_requested_yesterday_not_infected <- length(molecular_result_requested_yesterday_not_infected_ids)
          output_state_molecular_result_requested_yesterday_not_infected <- c()

          if(count_molecular_result_requested_yesterday_not_infected > 0){

            if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation"){
              # For SIR++, the only not_infected result seeking state is susceptible
              output_state_molecular_result_requested_yesterday_not_infected <- replicate(count_molecular_result_requested_yesterday_not_infected,
                                                                                        sample(result_state_set_not_infected, 1,
                                                                                               prob = molecular_state_change_for_not_infected_prob) )

              # Change state
              STATE[molecular_result_requested_yesterday_not_infected_ids, day_index - 1] <- output_state_molecular_result_requested_yesterday_not_infected

              # Aggregate agent ids receiving positive result
              positive_result_ids <- molecular_result_requested_yesterday_not_infected_ids[output_state_molecular_result_requested_yesterday_not_infected %in% result_state_positive]

              # Aggregate agent ids receiving negative result
              negative_result_ids <- molecular_result_requested_yesterday_not_infected_ids[output_state_molecular_result_requested_yesterday_not_infected %in% result_state_negative]


            } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Contact Tracing") {

              susceptible_positive_result_ids <- c()
              susceptible_negative_result_ids <- c()
              # Count "(isolated) susceptible" result seeker
              count_molecular_result_requested_yesterday_susceptible <- length(molecular_result_requested_yesterday_susceptible_ids)
              output_state_molecular_result_susceptible <- c()
              if(count_molecular_result_requested_yesterday_susceptible > 0){
                # Calculate output
                output_state_molecular_result_susceptible <- replicate(count_molecular_result_requested_yesterday_susceptible,
                                                                       sample(result_state_set_susceptible, 1,
                                                                              prob = molecular_state_change_for_not_infected_prob) )

                # Change state
                STATE[molecular_result_requested_yesterday_susceptible_ids, day_index - 1] <- output_state_molecular_result_susceptible


                # Aggregate agent ids receiving positive result
                susceptible_positive_result_ids <- molecular_result_requested_yesterday_susceptible_ids[output_state_molecular_result_susceptible %in% result_state_positive]

                # Aggregate agent ids receiving negative result
                susceptible_negative_result_ids <- molecular_result_requested_yesterday_susceptible_ids[output_state_molecular_result_susceptible %in% result_state_negative]
              }

              removed_positive_result_ids <- c()
              removed_negative_result_ids <- c()
              # Count "(isolated) removed" result seeker
              count_molecular_result_requested_yesterday_removed <- length(molecular_result_requested_yesterday_removed_ids)
              output_state_molecular_result_removed <- c()
              if(count_molecular_result_requested_yesterday_removed > 0){
                # Calculate output
                output_state_molecular_result_removed <- replicate(count_molecular_result_requested_yesterday_removed,
                                                                   sample(result_state_set_removed, 1,
                                                                          prob = molecular_state_change_for_not_infected_prob) )

                # Change state
                STATE[output_state_molecular_result_removed, day_index - 1] <- output_state_molecular_result_removed


                # Aggregate agent ids receiving positive result
                removed_positive_result_ids <- molecular_result_requested_yesterday_removed_ids[output_state_molecular_result_removed %in% result_state_positive]

                # Aggregate agent ids receiving negative result
                removed_negative_result_ids <- molecular_result_requested_yesterday_removed_ids[output_state_molecular_result_removed %in% result_state_negative]
              }


              # Aggregate positives and negative agent ids for book-keeping
              positive_result_ids <- c(susceptible_positive_result_ids, removed_positive_result_ids)
              negative_result_ids <- c(susceptible_negative_result_ids, removed_negative_result_ids)

            } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {

              susceptible_positive_result_ids <- c()
              susceptible_negative_result_ids <- c()
              # Count "(isolated) susceptible" result seeker
              count_molecular_result_requested_yesterday_susceptible <- length(molecular_result_requested_yesterday_susceptible_ids)
              output_state_molecular_result_susceptible <- c()
              if(count_molecular_result_requested_yesterday_susceptible > 0){
                # Calculate output
                output_state_molecular_result_susceptible <- replicate(count_molecular_result_requested_yesterday_susceptible,
                                                                       sample(result_state_set_susceptible, 1,
                                                                              prob = molecular_state_change_for_not_infected_prob) )

                # Change state
                STATE[molecular_result_requested_yesterday_susceptible_ids, day_index - 1] <- output_state_molecular_result_susceptible


                # Aggregate agent ids receiving positive result
                susceptible_positive_result_ids <- molecular_result_requested_yesterday_susceptible_ids[output_state_molecular_result_susceptible %in% result_state_positive]

                # Aggregate agent ids receiving negative result
                susceptible_negative_result_ids <- molecular_result_requested_yesterday_susceptible_ids[output_state_molecular_result_susceptible %in% result_state_negative]
              }

              recovered_positive_result_ids <- c()
              recovered_negative_result_ids <- c()
              # Count "(isolated) recovered" result seeker
              count_molecular_result_requested_yesterday_recovered <- length(molecular_result_requested_yesterday_recovered_ids)
              output_state_molecular_result_recovered <- c()
              if(count_molecular_result_requested_yesterday_recovered > 0){
                # Calculate output
                output_state_molecular_result_recovered <- replicate(count_molecular_result_requested_yesterday_recovered,
                                                                   sample(result_state_set_recovered, 1,
                                                                          prob = molecular_state_change_for_not_infected_prob) )

                # Change state
                STATE[output_state_molecular_result_recovered, day_index - 1] <- output_state_molecular_result_recovered


                # Aggregate agent ids receiving positive result
                recovered_positive_result_ids <- molecular_result_requested_yesterday_recovered_ids[output_state_molecular_result_recovered %in% result_state_positive]

                # Aggregate agent ids receiving negative result
                recovered_negative_result_ids <- molecular_result_requested_yesterday_recovered_ids[output_state_molecular_result_recovered %in% result_state_negative]
              }


              # Aggregate positives and negative agent ids for book-keeping
              positive_result_ids <- c(susceptible_positive_result_ids, recovered_positive_result_ids)
              negative_result_ids <- c(susceptible_negative_result_ids, recovered_negative_result_ids)

            }

            # Update test scheduling data structure
            test_schedule_mat[positive_result_ids, "result"] <- "positive"
            test_schedule_mat[negative_result_ids, "result"] <- "negative"

            # Book-keeping
            if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){
              true_negative_sus_test_result_ids <- c(true_negative_sus_test_result_ids, negative_result_ids)
            } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){
              true_negative_sus_test_result_ids <- c(true_negative_sus_test_result_ids, susceptible_negative_result_ids)
            } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){
              true_negative_sus_test_result_ids <- c(true_negative_sus_test_result_ids, susceptible_negative_result_ids)
            }

            all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)

            # Edge: "(isolated) susceptible" -- Test Result (FPR) --> (isolated + false positive) susceptible
            COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)
          }






          # Antigen - Symptomatic Infected : True Positive rate
          count_antigen_result_requested_yesterday_symptomatic_infectious <- length(antigen_result_requested_yesterday_symptomatic_infectious_ids)
          if(count_antigen_result_requested_yesterday_symptomatic_infectious > 0){
            output_state_antigen_result_requested_yesterday_symptomatic_infectious <- replicate( count_antigen_result_requested_yesterday_symptomatic_infectious,
                                                                                                 sample(result_state_set_symptomatic_infectious, 1,
                                                                                                        prob = symptomatic_antigen_state_change_for_infected_prob) )

            # Change state
            STATE[antigen_result_requested_yesterday_symptomatic_infectious_ids, day_index - 1] <- output_state_antigen_result_requested_yesterday_symptomatic_infectious

            # Update test scheduling data structure
            positive_result_ids <- antigen_result_requested_yesterday_symptomatic_infectious_ids[output_state_antigen_result_requested_yesterday_symptomatic_infectious %in% result_state_positive]
            test_schedule_mat[positive_result_ids, "result"] <- "positive"

            negative_result_ids <- antigen_result_requested_yesterday_symptomatic_infectious_ids[output_state_antigen_result_requested_yesterday_symptomatic_infectious %in% result_state_negative]
            test_schedule_mat[negative_result_ids, "result"] <- "negative"

            # Book-keeping
            false_negative_test_result_ids <- c(false_negative_test_result_ids, negative_result_ids)
            all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)

            # Edge: "(isolated + unconfirmed) infected"--- Test Result (TPR) --> "(isolated + confirmed) infected"
            COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)
          }

          # Rapid Molecular - Symptomatic Infected : True Positive rate
          count_molecular_result_requested_yesterday_symptomatic_infectious <- length(molecular_result_requested_yesterday_symptomatic_infectious_ids)
          if(count_molecular_result_requested_yesterday_symptomatic_infectious){
            output_state_molecular_result_requested_yesterday_symptomatic_infectious <- replicate( count_molecular_result_requested_yesterday_symptomatic_infectious,
                                                                                                   sample(result_state_set_symptomatic_infectious, 1,
                                                                                                          prob = molecular_state_change_for_infected_prob) )

            # Change state
            STATE[molecular_result_requested_yesterday_symptomatic_infectious_ids, day_index - 1] <- output_state_molecular_result_requested_yesterday_symptomatic_infectious

            # Update test scheduling data structures
            positive_result_ids <- molecular_result_requested_yesterday_symptomatic_infectious_ids[output_state_molecular_result_requested_yesterday_symptomatic_infectious %in% result_state_positive]
            test_schedule_mat[positive_result_ids, "result"] <- "positive"

            negative_result_ids <- molecular_result_requested_yesterday_symptomatic_infectious_ids[output_state_molecular_result_requested_yesterday_symptomatic_infectious %in% result_state_negative]
            test_schedule_mat[positive_result_ids, "result"] <- "negative"

            # Book-keeping
            false_negative_test_result_ids <- c(false_negative_test_result_ids, negative_result_ids)
            all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)

            # Edge: "(isolated + unconfirmed) infected"--- Test Result (TPR) --> "(isolated + confirmed) infected"
            COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)
          }


          # Antigen - Asymptomatic Infected : True Positive rate
          count_antigen_result_requested_yesterday_asymptomatic_infectious <- length(antigen_result_requested_yesterday_asymptomatic_infectious_ids)
          if(count_antigen_result_requested_yesterday_asymptomatic_infectious > 0){
            output_state_antigen_result_requested_yesterday_asymptomatic_infectious <- replicate( count_antigen_result_requested_yesterday_asymptomatic_infectious,
                                                                                                  sample(result_state_set_asymptomatic_infectious, 1,
                                                                                                         prob = asymptomatic_antigen_state_change_for_infected_prob) )

            # Change state
            STATE[antigen_result_requested_yesterday_asymptomatic_infectious_ids, day_index - 1] <- output_state_antigen_result_requested_yesterday_asymptomatic_infectious

            # Update test scheduling data structure
            positive_result_ids <- antigen_result_requested_yesterday_asymptomatic_infectious_ids[output_state_antigen_result_requested_yesterday_asymptomatic_infectious %in% result_state_positive]
            test_schedule_mat[positive_result_ids, "result"] <- "positive"

            negative_result_ids <- antigen_result_requested_yesterday_asymptomatic_infectious_ids[output_state_antigen_result_requested_yesterday_asymptomatic_infectious %in% result_state_negative]
            test_schedule_mat[negative_result_ids, "result"] <- "negative"

            # Book-keeping
            false_negative_test_result_ids <- c(false_negative_test_result_ids, negative_result_ids)
            all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)

            # Edge: "(isolated + unconfirmed) infected"--- Test Result (TPR) --> "(isolated + confirmed) infected"
            COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)

          }

          # Rapid Molecular - Asymptomatic Infected: True Positive rate
          count_molecular_result_requested_yesterday_asymptomatic_infectious <- length(molecular_result_requested_yesterday_asymptomatic_infectious_ids)
          if(count_molecular_result_requested_yesterday_asymptomatic_infectious > 0){
            output_state_molecular_result_requested_yesterday_asymptomatic_infectious <- replicate( count_molecular_result_requested_yesterday_asymptomatic_infectious,
                                                                                                    sample(result_state_set_asymptomatic_infectious, 1,
                                                                                                           prob = molecular_state_change_for_infected_prob) )

            # Change state
            STATE[molecular_result_requested_yesterday_asymptomatic_infectious_ids, day_index - 1] <- output_state_molecular_result_requested_yesterday_asymptomatic_infectious

            # Update test scheduling data structure
            positive_result_ids <- molecular_result_requested_yesterday_asymptomatic_infectious_ids[output_state_molecular_result_requested_yesterday_asymptomatic_infectious %in% result_state_positive]
            test_schedule_mat[positive_result_ids, "result"] <- "positive"

            negative_result_ids <- molecular_result_requested_yesterday_asymptomatic_infectious_ids[output_state_molecular_result_requested_yesterday_asymptomatic_infectious %in% result_state_negative]
            test_schedule_mat[negative_result_ids, "result"] <- "negative"

            # Book-keeping
            false_negative_test_result_ids <- c(false_negative_test_result_ids, negative_result_ids)
            all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)

            # Edge: "(isolated + unconfirmed) infected"--- Test Result (TPR) --> "(isolated + confirmed) infected"
            COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)

          }

          # Maintain internal iterator sets
          {
            # Isolating Susceptible agents receiving negative covid test result (and participate in disease propagation)
            previous_day_susceptible_person_ids <- union(previous_day_susceptible_person_ids, true_negative_sus_test_result_ids)
            previous_day_non_susceptible_person_ids <- setdiff(previous_day_non_susceptible_person_ids, true_negative_sus_test_result_ids)

            # Isolating Symptomatic and Asymptomatic Infectious agents receiving negative covid test result (and participate in disease propagation)
            previous_day_infectious_person_ids <- union(previous_day_infectious_person_ids, false_negative_test_result_ids)
          }

          # Tag: @calib11
          # For all disease models that include "Contact Tracing"
          # models: "SIR + Testing + Isolation + Contact Tracing",
          #         "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"
          if(CONST_ENABLE_CONTACT_TRACING && 
             ( (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") ||
              (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") ) ) {

            # Initialise
            all_contact_traced_neighbour_agent_ids <- c()

            # Get all neighbours
            all_contact_traced_neighbour_agent_ids <- batchContactTrace(all_positive_test_result_ids, day_index - 1)

            # For contact tracing delay of 1 day or more
            if(CONST_CONTACT_TRACING_DELAY > 0){
              # Assuming infinite contact tracing based testing capacity with antigen testing with same day results
              if(length(all_contact_traced_neighbour_agent_ids) > 0){
                test_schedule_mat[all_contact_traced_neighbour_agent_ids, "test type"] <- "antigen"
                test_schedule_mat[all_contact_traced_neighbour_agent_ids, "test scheduled on"] <- day_index - 1 + CONST_CONTACT_TRACING_DELAY
                test_schedule_mat[all_contact_traced_neighbour_agent_ids, "result day"] <- day_index - 1 + CONST_CONTACT_TRACING_DELAY + floor(available_covid_test_result_delays[available_antigen_test_index])
              }

            } else if(CONST_CONTACT_TRACING_DELAY == 0){
              # cat("  or Debug here!")

              # Assuming infinite contact tracing based testing capacity with antigen testing with same day results

              # Assuming predefined level of same day tracing and testing as "CONST_SAMEDAY_TRACING_LEVEL"
              # i.e. All neighbours of a particular agent who receives a positive test result
              # is tested same day. This process is not recurred further till the maximum tracing level is reached.


              for(tracing_level in 1:CONST_SAMEDAY_TRACING_LEVEL){

                # Get subset of "result_requested_yesterday_ids" which were also queued for testing yesterday
                result_requested_yesterday_ids <- which(test_schedule_mat[ , "result day"] == day_index - 1)
                same_day_tested_yesterday_ids <- result_requested_yesterday_ids[ which(as.numeric(test_schedule_mat[result_requested_yesterday_ids, "test scheduled on"]) == day_index - 1) ]

                # Get neighbours who were not queued for same day testing yesterday
                # Note: This is applicable for models that does not require isolation while waiting for test results with result delay of 1 or more days
                not_tested_yesterday_neighbours_agent_ids <- setdiff(all_contact_traced_neighbour_agent_ids, same_day_tested_yesterday_ids)
                

                # Debug:
                # cat("\n Sim day:", day_index, " checking tracing for day: ", day_index - 1)
                # cat("\n  |- Total traced contacts  : ", length(all_contact_traced_neighbour_agent_ids) )
                # cat("\n  |- Already tested yesterday  : ", length(same_day_tested_yesterday_ids) )
                # cat("\n  '- New traced contact to test: ", length(not_tested_yesterday_neighbours_agent_ids), "\n")

                # Queue for testing (for book-keeping)
                if(length(not_tested_yesterday_neighbours_agent_ids) > 0){
                  test_schedule_mat[not_tested_yesterday_neighbours_agent_ids, "test type"] <- "antigen"
                  test_schedule_mat[not_tested_yesterday_neighbours_agent_ids, "test scheduled on"] <- day_index - 1 + CONST_CONTACT_TRACING_DELAY
                  test_schedule_mat[not_tested_yesterday_neighbours_agent_ids, "result day"] <- day_index - 1 + CONST_CONTACT_TRACING_DELAY + floor(available_covid_test_result_delays[available_antigen_test_index])
                }

                # Change states to waiting for test results for congruency
                {
                  COUNT_TOTAL_TEST_RESULTS_REQUESTED = length(not_tested_yesterday_neighbours_agent_ids)

                  # Update previous_day_df
                  previous_day_df <- data.frame(STATE[ , day_index - 1])
                  colnames(previous_day_df) <- c("state_id")

                  # Update previous_day_df
                  previous_day_df <- data.frame(STATE[ , day_index - 1])
                  colnames(previous_day_df) <- c("state_id")
                  
                  # Filter states only for same day traced result requesting ids
                  previous_day_result_requested_df <- subset(previous_day_df, as.numeric(rownames(previous_day_df)) %in% not_tested_yesterday_neighbours_agent_ids)
                  
                  if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Contact Tracing") {
                    test_requested_yesterday_susceptible_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id == which(STATE_NAMES == "susceptible"))))
                    test_requested_yesterday_removed_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id == which(STATE_NAMES == "removed"))))
                    
                  } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {
                    test_requested_yesterday_susceptible_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id == which(STATE_NAMES == "susceptible"))))
                    test_requested_yesterday_recovered_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id == which(STATE_NAMES == "recovered"))))
                  }
                  
                  # Filter out symptomatic infectious test seekers
                  test_requested_yesterday_symptomatic_infectious_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id %in% STATELIST_SYMPTOMATIC_INFECTIOUS_TEST_SEEKER)))
                  # Filter out asymptomatic infectious test seekers
                  test_requested_yesterday_asymptomatic_infectious_ids <- as.numeric(rownames(subset(previous_day_result_requested_df, state_id %in% STATELIST_ASYMPTOMATIC_INFECTIOUS_TEST_SEEKER)))
                  
                  
                  
                  if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Contact Tracing") {
                    # For SIR+++ process multiple non-infected test-seeker states, namely: susceptible and removed
                    STATE[ test_requested_yesterday_susceptible_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) susceptible")
                    STATE[ test_requested_yesterday_removed_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) removed")

                  } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {
                    STATE[ test_requested_yesterday_susceptible_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) susceptible")
                    STATE[ test_requested_yesterday_recovered_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) recovered")
                  }

                  # For SIR+++, the infected test seeker state is "infected"
                  STATE[ test_requested_yesterday_symptomatic_infectious_ids , day_index - 1] <- which(STATE_NAMES == "(isolated + unconfirmed) infected")

                  # For SIR+++, there are no asymptomatic and infectious test seekers

                  # Change infectious and susceptible sets (as per disease propagation model)
                  {
                    previous_day_susceptible_person_ids <- setdiff(previous_day_susceptible_person_ids, test_requested_yesterday_susceptible_ids)
                    previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, test_requested_yesterday_susceptible_ids)

                    # "Symptomatic Infectious agents waiting for covid test result isolate themselves (and do not participate in disease propagation)
                    previous_day_infectious_person_ids <- setdiff(previous_day_infectious_person_ids, test_requested_yesterday_symptomatic_infectious_ids)

                    # "Asymptomatic Infectious agents waiting for covid test result isolate themselves (and do not participate in disease propagation)
                    previous_day_infectious_person_ids <- setdiff(previous_day_infectious_person_ids, test_requested_yesterday_asymptomatic_infectious_ids)
                  }
                }

                # Compute result states
                # Filter result seekers into appropriate compartments
                {
                  # Pass "(isolated) susceptible"
                  result_requested_yesterday_susceptible_ids <- test_requested_yesterday_susceptible_ids
                  antigen_result_requested_yesterday_susceptible_ids <- test_requested_yesterday_susceptible_ids

                  if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Contact Tracing") {
                    # Pass "(isolated) removed"
                    result_requested_yesterday_removed_ids <- test_requested_yesterday_removed_ids
                    antigen_result_requested_yesterday_removed_ids <- test_requested_yesterday_removed_ids

                  } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {
                    result_requested_yesterday_recovered_ids <- test_requested_yesterday_recovered_ids
                    antigen_result_requested_yesterday_recovered_ids <- test_requested_yesterday_recovered_ids
                  }


                  # Pass symptomatic infectious result seeker
                  result_requested_yesterday_symptomatic_infectious_ids <- test_requested_yesterday_symptomatic_infectious_ids
                  antigen_result_requested_yesterday_symptomatic_infectious_ids <- test_requested_yesterday_symptomatic_infectious_ids


                  # Pass asymptomatic infectious result seeker
                  result_requested_yesterday_asymptomatic_infectious_ids <- test_requested_yesterday_asymptomatic_infectious_ids
                  antigen_result_requested_yesterday_asymptomatic_infectious_ids <- test_requested_yesterday_asymptomatic_infectious_ids

                  # Debug:
                  # cat("\n Sim day:", day_index, " checking tracing for", day_index - 1)
                  # cat("\n  |- Already tested yesterday  : ", length(same_day_tested_yesterday_ids) )
                  # cat("\n  '- New traced contact to test: ", length(not_tested_yesterday_neighbours_agent_ids) )
                  # cat("\n   |- Susceptible traced contact to test: ", length(result_requested_yesterday_susceptible_ids), " / ",  length(test_requested_yesterday_susceptible_ids) )
                  # cat("\n   |- Removed traced contact to test: ", length(result_requested_yesterday_removed_ids), " / ",  length(test_requested_yesterday_removed_ids) )
                  # cat("\n   '- Infectious traced contact to test: ", length(result_requested_yesterday_symptomatic_infectious_ids), " / ",  length(test_requested_yesterday_symptomatic_infectious_ids), "\n")
                }

                # Book-keeping
                true_negative_sus_test_result_ids <- c() # For mutating list of susceptibles
                false_negative_test_result_ids <- c() # For mutating list of infectious

                # Tag: @calib11 - for same day recursive contact tracing
                all_sameday_traced_positive_test_result_ids <- c()

                # Calculate covid test results for each model
                # or process each Covid test based state transition
                # --------------------------

                # Antigen - Not infected : True Negative rate
                if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Contact Tracing") {
                  count_antigen_result_for_not_infected <- length( union(result_requested_yesterday_susceptible_ids, result_requested_yesterday_removed_ids) )

                } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {
                  count_antigen_result_for_not_infected <- length( union(result_requested_yesterday_susceptible_ids, result_requested_yesterday_recovered_ids) )
                }
                
                
                if(count_antigen_result_for_not_infected > 0){


                  if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Contact Tracing"){

                    susceptible_positive_result_ids <- c()
                    susceptible_negative_result_ids <- c()
                    # Count "(isolated) susceptible" result seeker
                    count_antigen_result_for_susceptible <- length(antigen_result_requested_yesterday_susceptible_ids)
                    output_state_antigen_result_for_susceptible <- c()
                    if(count_antigen_result_for_susceptible > 0){

                      # Calculate output
                      output_state_antigen_result_for_susceptible <- replicate(count_antigen_result_for_susceptible,
                                                                               sample(result_state_set_susceptible, 1,
                                                                                      prob = asymptomatic_antigen_state_change_for_not_infected_prob))

                      # Change state
                      STATE[antigen_result_requested_yesterday_susceptible_ids, day_index - 1] <- output_state_antigen_result_for_susceptible

                      # Aggregate agent ids receiving positive result
                      susceptible_positive_result_ids <- antigen_result_requested_yesterday_susceptible_ids[output_state_antigen_result_for_susceptible %in% result_state_positive]

                      # Aggregate agent ids receiving negative result
                      susceptible_negative_result_ids <- antigen_result_requested_yesterday_susceptible_ids[output_state_antigen_result_for_susceptible %in% result_state_negative]
                    }

                    removed_positive_result_ids <- c()
                    removed_negative_result_ids <- c()
                    # Count "(isolated) removed" result seeker
                    count_antigen_result_for_removed <- length(antigen_result_requested_yesterday_removed_ids)
                    output_state_antigen_result_for_removed <- c()
                    if(count_antigen_result_for_removed > 0){

                      # Calculate output
                      output_state_antigen_result_for_removed <- replicate(count_antigen_result_for_removed,
                                                                           sample(result_state_set_removed, 1,
                                                                                  prob = asymptomatic_antigen_state_change_for_not_infected_prob))

                      # Change state
                      STATE[antigen_result_requested_yesterday_removed_ids, day_index - 1] <- output_state_antigen_result_for_removed

                      # Aggregate agent ids receiving positive result
                      removed_positive_result_ids <- antigen_result_requested_yesterday_removed_ids[output_state_antigen_result_for_removed %in% result_state_positive]

                      # Aggregate agent ids receiving negative result
                      removed_negative_result_ids <- antigen_result_requested_yesterday_removed_ids[output_state_antigen_result_for_removed %in% result_state_negative]
                    }

                    # Aggregate positives and negative agent ids for book-keeping
                    positive_result_ids <- c(susceptible_positive_result_ids, removed_positive_result_ids)
                    negative_result_ids <- c(susceptible_negative_result_ids, removed_negative_result_ids)

                  } else if(CONST_CHOSEN_DISEASE_MODEL ==  "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){

                    susceptible_positive_result_ids <- c()
                    susceptible_negative_result_ids <- c()
                    # Count "(isolated) susceptible" result seeker
                    count_antigen_result_for_susceptible <- length(antigen_result_requested_yesterday_susceptible_ids)
                    output_state_antigen_result_for_susceptible <- c()
                    if(count_antigen_result_for_susceptible > 0){

                      # Calculate output
                      output_state_antigen_result_for_susceptible <- replicate(count_antigen_result_for_susceptible,
                                                                               sample(result_state_set_susceptible, 1,
                                                                                      prob = asymptomatic_antigen_state_change_for_not_infected_prob))

                      # Change state
                      STATE[antigen_result_requested_yesterday_susceptible_ids, day_index - 1] <- output_state_antigen_result_for_susceptible

                      # Aggregate agent ids receiving positive result
                      susceptible_positive_result_ids <- antigen_result_requested_yesterday_susceptible_ids[output_state_antigen_result_for_susceptible %in% result_state_positive]

                      # Aggregate agent ids receiving negative result
                      susceptible_negative_result_ids <- antigen_result_requested_yesterday_susceptible_ids[output_state_antigen_result_for_susceptible %in% result_state_negative]
                    }

                    recovered_positive_result_ids <- c()
                    recovered_negative_result_ids <- c()
                    # Count "(isolated) recovered" result seeker
                    count_antigen_result_for_recovered <- length(antigen_result_requested_yesterday_recovered_ids)
                    output_state_antigen_result_for_recovered <- c()
                    if(count_antigen_result_for_recovered > 0){

                      # Calculate output
                      output_state_antigen_result_for_recovered <- replicate(count_antigen_result_for_recovered,
                                                                           sample(result_state_set_recovered, 1,
                                                                                  prob = asymptomatic_antigen_state_change_for_not_infected_prob))

                      # Change state
                      STATE[antigen_result_requested_yesterday_recovered_ids, day_index - 1] <- output_state_antigen_result_for_recovered

                      # Aggregate agent ids receiving positive result
                      recovered_positive_result_ids <- antigen_result_requested_yesterday_recovered_ids[output_state_antigen_result_for_recovered %in% result_state_positive]

                      # Aggregate agent ids receiving negative result
                      recovered_negative_result_ids <- antigen_result_requested_yesterday_recovered_ids[output_state_antigen_result_for_recovered %in% result_state_negative]
                    }

                    # Aggregate positives and negative agent ids for book-keeping
                    positive_result_ids <- c(susceptible_positive_result_ids, recovered_positive_result_ids)
                    negative_result_ids <- c(susceptible_negative_result_ids, recovered_negative_result_ids)

                  }


                  # Update test scheduling data structure
                  test_schedule_mat[positive_result_ids, "result"] <- "positive"
                  test_schedule_mat[negative_result_ids, "result"] <- "negative"


                  true_negative_sus_test_result_ids <- c(true_negative_sus_test_result_ids, susceptible_negative_result_ids)


                  all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)

                  # For recursion on same day contact tracing and testing
                  all_sameday_traced_positive_test_result_ids <- c(all_sameday_traced_positive_test_result_ids, positive_result_ids)

                  # Edge: "(isolated) susceptible/removed" -- Test Result (FPR) --> (isolated + false positive) susceptible/removed
                  COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)
                }


                # Antigen - Symptomatic Infected : True Positive rate
                count_antigen_result_requested_yesterday_symptomatic_infectious <- length(antigen_result_requested_yesterday_symptomatic_infectious_ids)
                if(count_antigen_result_requested_yesterday_symptomatic_infectious > 0){
                  output_state_antigen_result_requested_yesterday_symptomatic_infectious <- replicate( count_antigen_result_requested_yesterday_symptomatic_infectious,
                                                                                                       sample(result_state_set_symptomatic_infectious, 1,
                                                                                                              prob = symptomatic_antigen_state_change_for_infected_prob) )

                  # Change state
                  STATE[antigen_result_requested_yesterday_symptomatic_infectious_ids, day_index - 1] <- output_state_antigen_result_requested_yesterday_symptomatic_infectious

                  # Update test scheduling data structure
                  positive_result_ids <- antigen_result_requested_yesterday_symptomatic_infectious_ids[output_state_antigen_result_requested_yesterday_symptomatic_infectious %in% result_state_positive]
                  test_schedule_mat[positive_result_ids, "result"] <- "positive"

                  negative_result_ids <- antigen_result_requested_yesterday_symptomatic_infectious_ids[output_state_antigen_result_requested_yesterday_symptomatic_infectious %in% result_state_negative]
                  test_schedule_mat[negative_result_ids, "result"] <- "negative"

                  # Book-keeping
                  false_negative_test_result_ids <- c(false_negative_test_result_ids, negative_result_ids)
                  all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)

                  # For recursion on same day contact tracing and testing
                  all_sameday_traced_positive_test_result_ids <- c(all_sameday_traced_positive_test_result_ids, positive_result_ids)

                  # Edge: "(isolated + unconfirmed) infected"--- Test Result (TPR) --> "(isolated + confirmed) infected"
                  COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)
                }

                # Antigen - Asymptomatic Infected : True Positive rate
                count_antigen_result_requested_yesterday_asymptomatic_infectious <- length(antigen_result_requested_yesterday_asymptomatic_infectious_ids)
                if(count_antigen_result_requested_yesterday_asymptomatic_infectious > 0){
                  output_state_antigen_result_requested_yesterday_asymptomatic_infectious <- replicate( count_antigen_result_requested_yesterday_asymptomatic_infectious,
                                                                                                        sample(result_state_set_asymptomatic_infectious, 1,
                                                                                                               prob = asymptomatic_antigen_state_change_for_infected_prob) )

                  # Change state
                  STATE[antigen_result_requested_yesterday_asymptomatic_infectious_ids, day_index - 1] <- output_state_antigen_result_requested_yesterday_asymptomatic_infectious

                  # Update test scheduling data structure
                  positive_result_ids <- antigen_result_requested_yesterday_asymptomatic_infectious_ids[output_state_antigen_result_requested_yesterday_asymptomatic_infectious %in% result_state_positive]
                  test_schedule_mat[positive_result_ids, "result"] <- "positive"

                  negative_result_ids <- antigen_result_requested_yesterday_asymptomatic_infectious_ids[output_state_antigen_result_requested_yesterday_asymptomatic_infectious %in% result_state_negative]
                  test_schedule_mat[negative_result_ids, "result"] <- "negative"

                  # Book-keeping
                  false_negative_test_result_ids <- c(false_negative_test_result_ids, negative_result_ids)
                  all_positive_test_result_ids <- c(all_positive_test_result_ids, positive_result_ids)

                  # For recursion on same day contact tracing and testing
                  all_sameday_traced_positive_test_result_ids <- c(all_sameday_traced_positive_test_result_ids, positive_result_ids)

                  # Edge: "(isolated + unconfirmed) infected"--- Test Result (TPR) --> "(isolated + confirmed) infected"
                  COUNT_NEW_INFECTIONS_YESTERDAY <- COUNT_NEW_INFECTIONS_YESTERDAY + length(positive_result_ids)

                }


                # Maintain internal iterator sets
                {
                  # Isolating Susceptible agents receiving negative covid test result (and participate in disease propagation)
                  previous_day_susceptible_person_ids <- union(previous_day_susceptible_person_ids, true_negative_sus_test_result_ids)
                  previous_day_non_susceptible_person_ids <- setdiff(previous_day_non_susceptible_person_ids, true_negative_sus_test_result_ids)

                  # Isolating Symptomatic and Asymptomatic Infectious agents receiving negative covid test result (and participate in disease propagation)
                  previous_day_infectious_person_ids <- union(previous_day_infectious_person_ids, false_negative_test_result_ids)
                }





                # Debug:
                # cat("\n Tracing level: ", tracing_level)
                # Recurse on new positive cases

                all_contact_traced_neighbour_agent_ids <- c()

                # Get all neighbours
                all_contact_traced_neighbour_agent_ids <- batchContactTrace(all_sameday_traced_positive_test_result_ids, day_index - 1)

                # If this is the final level of same day tracing, queue up this level of traced contacts for next day testing
                if(tracing_level == CONST_SAMEDAY_TRACING_LEVEL){

                  if(length(all_contact_traced_neighbour_agent_ids) > 0){
                    test_schedule_mat[all_contact_traced_neighbour_agent_ids, "test type"] <- "antigen"
                    test_schedule_mat[all_contact_traced_neighbour_agent_ids, "test scheduled on"] <- day_index - 1 + 1
                    test_schedule_mat[all_contact_traced_neighbour_agent_ids, "result day"] <- day_index - 1 + 1 + floor(available_covid_test_result_delays[available_antigen_test_index])
                  }

                }
                
                # In compliance with Jarod's implementation
                # Sending these contacts to their isolated compartments 
                if (CONST_ISOLATE_TRACED_OVERFLOW){
                  
                  
                  # for (traced_agent_id in all_contact_traced_neighbour_agent_ids) {
                  #   if (STATE[traced_agent_id, day_index - 1] == which(STATE_NAMES == "susceptible")) {
                  #     STATE[traced_agent_id, day_index - 1] <- which(STATE_NAMES == "(isolated) susceptible")
                  #     previous_day_susceptible_person_ids <- setdiff(previous_day_susceptible_person_ids, traced_agent_id)
                  #     previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, traced_agent_id)
                  #     
                  #   } else if (STATE[traced_agent_id, day_index - 1] == which(STATE_NAMES == "(unconfirmed) infected")) {
                  #     STATE[traced_agent_id, day_index - 1] <- which(STATE_NAMES == "(isolated + unconfirmed) infected")
                  #     previous_day_infectious_person_ids <- setdiff(previous_day_infectious_person_ids, traced_agent_id)
                  #     
                  #   } else if (STATE[traced_agent_id, day_index - 1] == which(STATE_NAMES == "removed")) {
                  #     STATE[traced_agent_id, day_index - 1] <- which(STATE_NAMES == "(isolated) removed")
                  #     previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, traced_agent_id)
                  #     
                  #   } 
                  # }
                  
                  # Update previous_day_df
                  previous_day_df <- data.frame(STATE[ , day_index - 1])
                  colnames(previous_day_df) <- c("state_id")
                  
                  # Filter states only for test requesting ids
                  next_day_test_requested_df <- subset(previous_day_df, as.numeric(rownames(previous_day_df)) %in% all_contact_traced_neighbour_agent_ids)
                  
                  
                  if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") {
                    # For SIR+++ process multiple non-infected test-seeker states, namely: susceptible and removed
                    
                    test_requested_next_day_susceptible_ids <- as.numeric(rownames(subset(next_day_test_requested_df, state_id == which(STATE_NAMES == "susceptible"))))
                    test_requested_next_day_removed_ids <- as.numeric(rownames(subset(next_day_test_requested_df, state_id == which(STATE_NAMES == "removed"))))
                  } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {
                    # For augmented SIR with separate recovered and dead compartments,
                    # process multiple non-infected test-seeker states, namely: susceptible and recovered
                    
                    test_requested_next_day_susceptible_ids <- as.numeric(rownames(subset(next_day_test_requested_df, state_id == which(STATE_NAMES == "susceptible"))))
                    test_requested_next_day_recovered_ids <- as.numeric(rownames(subset(next_day_test_requested_df, state_id == which(STATE_NAMES == "recovered"))))
                  }
                  # Filter out symptomatic infectious test seekers
                  test_requested_next_day_symptomatic_infectious_ids <- as.numeric(rownames(subset(next_day_test_requested_df, state_id %in% STATELIST_SYMPTOMATIC_INFECTIOUS_TEST_SEEKER)))
                  # Filter out asymptomatic infectious test seekers
                  test_requested_next_day_asymptomatic_infectious_ids <- as.numeric(rownames(subset(next_day_test_requested_df, state_id %in% STATELIST_ASYMPTOMATIC_INFECTIOUS_TEST_SEEKER)))
                  
                  
                  
                  # Set "waiting for result" state retroactively
                  if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){
                    
                    # For SIR+++ process multiple non-infected test-seeker states, namely: susceptible and removed
                    STATE[ test_requested_next_day_susceptible_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) susceptible")
                    STATE[ test_requested_next_day_removed_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) removed")
                    
                    # For SIR+++, the infected test seeker state is "infected"
                    STATE[ test_requested_next_day_symptomatic_infectious_ids , day_index - 1] <- which(STATE_NAMES == "(isolated + unconfirmed) infected")
                    
                    # For SIR+++, there are no asymptomatic and infectious test seekers
                  } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){
                    
                    # For augmented SIR with separate recovered and dead compartments,
                    # multiple non-infected test-seeker states, namely: susceptible and recovered
                    STATE[ test_requested_next_day_susceptible_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) susceptible")
                    STATE[ test_requested_next_day_recovered_ids , day_index - 1] <- which(STATE_NAMES == "(isolated) recovered")
                    
                    # For SIR+++, the infected test seeker state is "infected"
                    STATE[ test_requested_next_day_symptomatic_infectious_ids , day_index - 1] <- which(STATE_NAMES == "(isolated + unconfirmed) infected")
                    
                    # For SIR+++, there are no asymptomatic and infectious test seekers
                  }
                  
                  
                  # Manage iterator sets
                  if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){
                    previous_day_susceptible_person_ids <- setdiff(previous_day_susceptible_person_ids, test_requested_next_day_susceptible_ids)
                    previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, test_requested_next_day_susceptible_ids)
                    
                    
                  } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){
                    previous_day_susceptible_person_ids <- setdiff(previous_day_susceptible_person_ids, test_requested_next_day_susceptible_ids)
                    previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, test_requested_next_day_susceptible_ids)
                  }
                  
                  # "Symptomatic Infectious agents waiting for covid test result isolate themselves (and do not participate in disease propagation)
                  previous_day_infectious_person_ids <- setdiff(previous_day_infectious_person_ids, test_requested_next_day_symptomatic_infectious_ids)
                  
                  # "Asymptomatic Infectious agents waiting for covid test result isolate themselves (and do not participate in disease propagation)
                  previous_day_infectious_person_ids <- setdiff(previous_day_infectious_person_ids, test_requested_next_day_asymptomatic_infectious_ids)
                  
                  
                  
                  
                } # End of CONST_ISOLATE_TRACED_OVERFLOW
                


              }
            }
          }
          # Debug:
          # cat("\n----------Moving back to susceptible from non-susceptible at Test Request: ", length(true_negative_sus_test_result_ids))

          # Isolating Susceptible agents receiving negative covid test result (and participate in disease propagation)
          # previous_day_susceptible_person_ids <- union(previous_day_susceptible_person_ids, true_negative_sus_test_result_ids)
          # previous_day_non_susceptible_person_ids <- setdiff(previous_day_non_susceptible_person_ids, true_negative_sus_test_result_ids)
          #
          # # Isolating Symptomatic and Asymptomatic Infectious agents receiving negative covid test result (and participate in disease propagation)
          # previous_day_infectious_person_ids <- union(previous_day_infectious_person_ids, false_negative_test_result_ids)


        }

        # Debug:
        # DONE: Debug iterator inconsistency
        # miss_labled_sus_ids <- which( STATE[previous_day_susceptible_person_ids,  day_index - 1] != which(STATE_NAMES == "susceptible"))
        # if( length(miss_labled_sus_ids) > 0){
        #   cat("\n Miss labled Susceptibles: ", miss_labled_sus_ids, "\n")
        # }

        # "Did any infectious agent spread contagion yesterday (such that the state change occurred today)?"
        # Initialise with previous day's agents
        current_day_susceptible_person_ids <- previous_day_susceptible_person_ids

        for(infector_id in previous_day_infectious_person_ids){
          # Tag: @calib08
          # Find the susceptible subset of neighbours of an infector for this sim day

          if( GENERATE_EVENT_CONTACT_PAIRS && day_index >= EVENT_START_DATE && day_index <= EVENT_END_DATE){
            # With event contacts
            list_of_susceptibles <- intersect(list_day_x_person_x_contact_w_event[[contact_matrix_index_lookup_list[day_index]]][[infector_id]], current_day_susceptible_person_ids)
          } else {
             # Without event contacts
            list_of_susceptibles <- intersect(list_day_x_person_x_contact[[contact_matrix_index_lookup_list[day_index]]][[infector_id]], current_day_susceptible_person_ids)
          }
          COUNT_OF_POSSIBLE_TRANSMISSIONS <- length(list_of_susceptibles)
          relative_infectivity <- as.numeric(active_voc_mat[which(active_voc_mat[ , "variant"] == infection_hist_mat[infector_id, "variant"]), "Relative infectivity"])
          # Tag: @calib09
          if(CONST_USE_INFECTIOUSNESS_PDF){
            temp_days_since_infected <- day_index - as.numeric(infection_hist_mat[[infector_id, "infected on"]])
            relative_infectivity <- relative_infectivity * normalised_infectiousness_pdf(temp_days_since_infected)
          }

          infection_WHO_label <- active_voc_mat[which(active_voc_mat[ , "variant"] == infection_hist_mat[infector_id, "variant"]), "WHO label"]
          # Get possible vaccine efficacies
          vaccine_odds1 <- as.numeric(active_voc_mat[which(active_voc_mat[ , "variant"] == infection_hist_mat[infector_id, "variant"]), "Vaccine dose 1 efficacy"])
          vaccine_odds2 <- as.numeric(active_voc_mat[which(active_voc_mat[ , "variant"] == infection_hist_mat[infector_id, "variant"]), "Vaccine dose 2 efficacy"])
          vaccine_odds3 <- as.numeric(active_voc_mat[which(active_voc_mat[ , "variant"] == infection_hist_mat[infector_id, "variant"]), "Vaccine dose 3 efficacy"])


          for (susceptible_agent_id in list_of_susceptibles){

            PREV_STATE_INDEX <- as.numeric(STATE[susceptible_agent_id, day_index - 1])
            PREV_STATE_NAME <- STATE_NAMES[PREV_STATE_INDEX]

            # for no transitions
            next_state_id <- PREV_STATE_INDEX
            next_state_name <- PREV_STATE_NAME

            # Get vaccine efficacy w.r.t contact's variant
            vaccine_odds <- NA # Assume person_id is without vaccine

            if(PREV_STATE_NAME == "received_dose1"){
              vaccine_odds <- vaccine_odds1

              # Scale w.r.t vaccination day # Remove: as now we have vaccinated pop without a set vaccination day
              # dose1_received_on <- strtoi(infection_hist_mat[person_id, "dose 1 on"])
              # vaccine_odds <- scaleVaccineOddsLinear(day_index - dose1_received_on) * vaccine_odds
            }

            else if(PREV_STATE_NAME == "received_dose2"){

              # Scale w.r.t vaccination day # Remove: as now we have vaccinated pop without a set vaccination day
              # dose1_received_on <- strtoi(infection_hist_mat[person_id, "dose 1 on"])
              # vaccine_odds1 <- scaleVaccineOddsLinear(day_index - dose1_received_on) * vaccine_odds1

              # Scale w.r.t vaccination day # Remove: as now we have vaccinated pop without a set vaccination day
              # dose2_received_on <- strtoi(infection_hist_mat[person_id, "dose 2 on"])
              # vaccine_odds2 <- scaleVaccineOddsLinear(day_index - dose2_received_on) * vaccine_odds2

              vaccine_odds <- max(vaccine_odds1, vaccine_odds2)

            }

            # For 3rd dose{
            # Note: This may fail for variants whose 3rd dose efficacy in-spite of the guard clauses of 2nd and 1st dose efficacies
            else if(PREV_STATE_NAME == "received_dose3"){

              vaccine_odds <- max(vaccine_odds1, vaccine_odds2, vaccine_odds3)

            }


            if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Aggregated"){

              # Tag: @calib08
              # Load contact time including time spent in event with events during event duration
              if( GENERATE_EVENT_CONTACT_PAIRS && day_index >= EVENT_START_DATE && day_index <= EVENT_END_DATE){
                contact_time <- list_of_contact_matrix_w_event[[contact_matrix_index_lookup_list[day_index]]][susceptible_agent_id, infector_id]
              } else {
                # Note, old code based on iterator
                contact_time <- contact_matrix[susceptible_agent_id, infector_id]
                # Maybe changed to the following
                # contact_time <- list_of_contact_matrix[[contact_matrix_index_lookup_list[day_index]]][susceptible_agent_id, infector_id]
              }


              random_coin_toss <- runif(1)
              COUNT_NO_OF_INFECTION_COIN_FLIPS = COUNT_NO_OF_INFECTION_COIN_FLIPS + 1

              this_infection_chance <- getInfection_probability_w_variant_w_vaccine(vaccine_odds, relative_infectivity, contact_time)

              # cat("           For susceptible: ", susceptible_agent_id, ", For infected neighbour: ", infector_id," unifrom_random_sample(0,1): ", random_coin_toss, " , infection probability: ", this_infection_chance, " => infection :", random_coin_toss <= this_infection_chance, "\n", sep = "")

              if( random_coin_toss <= this_infection_chance ) {

                # Debug:
                COUNT_NO_OF_INFECTION_EVENTS = COUNT_NO_OF_INFECTION_EVENTS + 1


                if (CONST_CHOSEN_DISEASE_MODEL == "Complex"){
                  # Infection event for un-vaccinated
                  if (PREV_STATE_NAME == "susceptible"){
                    # Check if omicron
                    if(infection_WHO_label == "Omicron") {
                      next_state_id <- sample( c( which(STATE_NAMES == "00_latent_infections_not_isolated_omicron"), which(STATE_NAMES == "00_latent_infections_isolated_omicron") ),
                                               1,
                                               prob = c((1 - CONST_q_omicron), CONST_q_omicron))
                    }

                    # Check if Wild, Alpha, Beta, Gamma or Delta
                    else if(infection_WHO_label == "Wild" ||
                            infection_WHO_label == "Alpha" ||
                            infection_WHO_label == "Beta" ||
                            infection_WHO_label == "Gamma" ||
                            infection_WHO_label == "Delta" ){
                      next_state_id <- sample( c( which(STATE_NAMES == "00_latent_infections_not_isolated"), which(STATE_NAMES == "00_latent_infections_isolated") ),
                                               1,
                                               prob = c((1 - CONST_q), CONST_q))
                    }

                    # Else throw warning on console output
                    else {
                      # Debug:
                      cat("[WARNING] Cannot find post-infection state for infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], "\n", sep = "")
                    }

                  }

                  # Infection event for person with 1 dose
                  else if (PREV_STATE_NAME == "received_dose1"){
                    # Check if omicron
                    if(infection_WHO_label == "Omicron") {
                      next_state_id <- sample( c( which(STATE_NAMES == "01_latent_infections_not_isolated_omicron"), which(STATE_NAMES == "01_latent_infections_isolated_omicron") ),
                                               1,
                                               prob = c((1 - CONST_q_omicron), CONST_q_omicron))
                    }

                    # Check if Wild, Alpha, Beta, Gamma or Delta
                    else if(infection_WHO_label == "Wild" ||
                            infection_WHO_label == "Alpha" ||
                            infection_WHO_label == "Beta" ||
                            infection_WHO_label == "Gamma" ||
                            infection_WHO_label == "Delta" ){
                      next_state_id <- sample( c( which(STATE_NAMES == "01_latent_infections_not_isolated"), which(STATE_NAMES == "01_latent_infections_isolated") ),
                                               1,
                                               prob = c((1 - CONST_q), CONST_q))
                    }

                    # Else throw warning on console output
                    else {
                      # Debug:
                      cat("[WARNING] Cannot find post-infection state for person_id: ",  susceptible_agent_id, ", with infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], "\n", sep = "")
                    }
                  }


                  # Infection event for person with 2 doses
                  else if (PREV_STATE_NAME == "received_dose2"){
                    # Check if omicron
                    if(infection_WHO_label == "Omicron") {
                      next_state_id <- sample( c( which(STATE_NAMES == "02_latent_infections_not_isolated_omicron"), which(STATE_NAMES == "02_latent_infections_isolated_omicron") ),
                                               1,
                                               prob = c((1 - CONST_q_omicron), CONST_q_omicron))
                    }

                    # Check if Wild, Alpha, Beta, Gamma or Delta
                    else if(infection_WHO_label == "Wild" ||
                            infection_WHO_label == "Alpha" ||
                            infection_WHO_label == "Beta" ||
                            infection_WHO_label == "Gamma" ||
                            infection_WHO_label == "Delta" ){
                      next_state_id <- sample( c( which(STATE_NAMES == "02_latent_infections_not_isolated"), which(STATE_NAMES == "02_latent_infections_isolated") ),
                                               1,
                                               prob = c((1 - CONST_q), CONST_q))
                    }

                    # Else throw warning on console output
                    else {
                      # Debug:
                      cat("[WARNING] Cannot find post-infection state for person_id: ",  susceptible_agent_id, ", with infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], "\n", sep = "")
                    }
                  }

                  # Infection event for person with 3 doses
                  else if (PREV_STATE_NAME == "received_dose3"){
                    # Check if omicron
                    if(infection_WHO_label == "Omicron") {
                      next_state_id <- sample( c( which(STATE_NAMES == "03_latent_infections_not_isolated_omicron"), which(STATE_NAMES == "03_latent_infections_isolated_omicron") ),
                                               1,
                                               prob = c((1 - CONST_q_omicron), CONST_q_omicron))
                    }

                    # Check if Wild, Alpha, Beta or Delta
                    else if(infection_WHO_label == "Wild" ||
                            infection_WHO_label == "Alpha" ||
                            infection_WHO_label == "Beta" ||
                            infection_WHO_label == "Gamma" ||
                            infection_WHO_label == "Delta" ){
                      next_state_id <- sample( c( which(STATE_NAMES == "03_latent_infections_not_isolated"), which(STATE_NAMES == "03_latent_infections_isolated") ),
                                               1,
                                               prob = c((1 - CONST_q), CONST_q))
                    }

                    # Else throw warning on console output
                    else {
                      # Debug:
                      cat("[WARNING] Cannot find post-infection state for person_id: ",  susceptible_agent_id, ", with infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], ", missing dose 3 state for non-omicron variants" ,"\n", sep = "")
                    }
                  }
                } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR") {

                  # Infection event for un-vaccinated
                  if (PREV_STATE_NAME == "susceptible"){
                    next_state_id <- which(STATE_NAMES == "infected")
                  }
                  else {
                    # Debug:
                    cat("[WARNING] Cannot find post-infection state for person_id: ",  susceptible_agent_id, ", with infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], "\n", sep = "")
                  }

                } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation") {

                  # Infection event for un-vaccinated
                  if (PREV_STATE_NAME == "susceptible"){
                    next_state_id <- which(STATE_NAMES == "(unconfirmed) infected")
                  }
                  else {
                    # Debug:
                    cat("\n[WARNING] Cannot find post-infection state for person_id: ",  susceptible_agent_id, " (", PREV_STATE_NAME, "), with infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], "\n", sep = "")
                  }

                } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") {

                  # Infection event for un-vaccinated
                  if (PREV_STATE_NAME == "susceptible"){
                    next_state_id <- which(STATE_NAMES == "(unconfirmed) infected")

                  } else {
                    # Debug:
                    cat("\n[WARNING] Cannot find post-infection state for person_id: ",  susceptible_agent_id, " (", PREV_STATE_NAME, "), with infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], "\n", sep = "")
                  }

                } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {

                  # Infection event for un-vaccinated
                  if (PREV_STATE_NAME == "susceptible"){
                    next_state_id <- which(STATE_NAMES == "(unconfirmed) infected")

                  } else {
                    # Debug:
                    cat("\n[WARNING] Cannot find post-infection state for person_id: ",  susceptible_agent_id, " (", PREV_STATE_NAME, "), with infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], "\n", sep = "")
                  }

                } else {

                  cat("\n [WARNING] Iterating over infectious, Infectious state assignment for Aggregated Contact Matrix has not been implemented for disease model: ", CONST_CHOSEN_DISEASE_MODEL)
                }



                moved_to_non_susceptible_today <- c(moved_to_non_susceptible_today, susceptible_agent_id)
                current_day_susceptible_person_ids <- current_day_susceptible_person_ids[! current_day_susceptible_person_ids %in% c(susceptible_agent_id)]

                STATE[susceptible_agent_id, day_index] <- next_state_id
                next_state_name <- STATE_NAMES[next_state_id]


                # Book keeping
                infection_hist_mat[susceptible_agent_id, "variant"] <- toString(infection_hist_mat[infector_id, "variant"])
                infection_hist_mat[susceptible_agent_id, "infected on"] <- day_index
                infection_hist_mat[susceptible_agent_id, "infected by"] <- infector_id

                # Debug:
                # Bookkeeping with variants
                # cat("           Person: ", susceptible_agent_id,
                #    ", infected with ", infection_hist_mat[susceptible_agent_id, "variant"],
                #    ", by person: ", infector_id,
                #    ", on sim day: ", day_index, "\n")

              } # End of Single infection state change with book-keeping - For aggregated contact times

            } else if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Location based"){
              # cat("\n Yet to implement infector iterator for location wise matrix")
              # stop()

              # Tag: @calib08
              # Load contact time including time spent in event with events during event duration
              if( GENERATE_EVENT_CONTACT_PAIRS && day_index >= EVENT_START_DATE && day_index <= EVENT_END_DATE){
                person_specific_contact_indices <- which(list_day_x_person_x_contact_w_event[[contact_matrix_index_lookup_list[day_index]]][[susceptible_agent_id]] == infector_id)

                # Get all person-specific contact times
                list_of_contact_times <- list_day_x_person_x_contact_time_w_event[[contact_matrix_index_lookup_list[day_index]]][[susceptible_agent_id]]
                # Filter times for this particular contact
                list_of_contact_times <- list_of_contact_times[person_specific_contact_indices]

                # Get all person-specific contact location types
                list_of_contact_location_types <- list_day_x_person_x_contact_location_type_w_event[[contact_matrix_index_lookup_list[day_index]]][[susceptible_agent_id]]
                # Filter location types for this particular contact
                list_of_contact_location_types <- list_of_contact_location_types[person_specific_contact_indices]

                # Get all person-specific contact location types
                list_of_contact_location_ids <- list_day_x_person_x_contact_location_id_w_event[[contact_matrix_index_lookup_list[day_index]]][[susceptible_agent_id]]
                # Filter location types for this particular contact
                list_of_contact_location_ids <- list_of_contact_location_ids[person_specific_contact_indices]


              } else {
                person_specific_contact_indices <- which(list_day_x_person_x_contact[[contact_matrix_index_lookup_list[day_index]]][[susceptible_agent_id]] == infector_id)

                # Get all person-specific contact times
                list_of_contact_times <- list_day_x_person_x_contact_time[[contact_matrix_index_lookup_list[day_index]]][[susceptible_agent_id]]
                # Filter times for this particular contact
                list_of_contact_times <- list_of_contact_times[person_specific_contact_indices]

                # Get all person-specific contact location types
                list_of_contact_location_types <- list_day_x_person_x_contact_location_type[[contact_matrix_index_lookup_list[day_index]]][[susceptible_agent_id]]
                # Filter location types for this particular contact
                list_of_contact_location_types <- list_of_contact_location_types[person_specific_contact_indices]

                # Get all person-specific contact location types
                list_of_contact_location_ids <- list_day_x_person_x_contact_location_id[[contact_matrix_index_lookup_list[day_index]]][[susceptible_agent_id]]
                # Filter location types for this particular contact
                list_of_contact_location_ids <- list_of_contact_location_ids[person_specific_contact_indices]
              }

              for (contact_event_index in 1:length(list_of_contact_times))  {

                temp_damp_for_commercial = 1.0


                contact_event_location_type = levels(list_of_contact_location_types[contact_event_index])[list_of_contact_location_types[contact_event_index]]
                contact_event_location_id = list_of_contact_location_ids[contact_event_index]



                if( contact_event_location_type == "Commercial"){
                  temp_damp_for_commercial = CONST_DAMP_FOR_COMMERCIAL
                  # Debug
                  # cat("\n         damping ", levels(list_of_contact_location_types[contact_event_index])[list_of_contact_location_types[contact_event_index]],
                  #     " type contact for susceptible agent id: ", person_id,
                  #     " with infectious agent id: ", person_specific_contact_indices[contact_event_index], sep ="")
                }

                # cat("\nDebug here")
                random_coin_toss <- runif(1)
                COUNT_NO_OF_INFECTION_COIN_FLIPS = COUNT_NO_OF_INFECTION_COIN_FLIPS + 1

                this_infection_chance <- getInfection_probability_w_variant_w_vaccine(vaccine_odds, relative_infectivity, list_of_contact_times[contact_event_index])

                # cat("\n Infection chance: ", this_infection_chance)
                this_infection_chance <- this_infection_chance * temp_damp_for_commercial
                # cat(" modified infection chance for ",
                #     levels(list_of_contact_location_types[contact_event_index])[list_of_contact_location_types[contact_event_index]],
                #     " : ", this_infection_chance)

                if( random_coin_toss <= this_infection_chance ) {

                  # Debug:
                  COUNT_NO_OF_INFECTION_EVENTS = COUNT_NO_OF_INFECTION_EVENTS + 1


                  if (CONST_CHOSEN_DISEASE_MODEL == "Complex"){
                    # Infection event for un-vaccinated
                    if (PREV_STATE_NAME == "susceptible"){
                      # Check if omicron
                      if(infection_WHO_label == "Omicron") {
                        next_state_id <- sample( c( which(STATE_NAMES == "00_latent_infections_not_isolated_omicron"), which(STATE_NAMES == "00_latent_infections_isolated_omicron") ),
                                                 1,
                                                 prob = c((1 - CONST_q_omicron), CONST_q_omicron))
                      }

                      # Check if Wild, Alpha, Beta, Gamma or Delta
                      else if(infection_WHO_label == "Wild" ||
                              infection_WHO_label == "Alpha" ||
                              infection_WHO_label == "Beta" ||
                              infection_WHO_label == "Gamma" ||
                              infection_WHO_label == "Delta" ){
                        next_state_id <- sample( c( which(STATE_NAMES == "00_latent_infections_not_isolated"), which(STATE_NAMES == "00_latent_infections_isolated") ),
                                                 1,
                                                 prob = c((1 - CONST_q), CONST_q))
                      }

                      # Else throw warning on console output
                      else {
                        # Debug:
                        cat("[WARNING] Cannot find post-infection state for infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], "\n", sep = "")
                      }

                    }

                    # Infection event for person with 1 dose
                    else if (PREV_STATE_NAME == "received_dose1"){
                      # Check if omicron
                      if(infection_WHO_label == "Omicron") {
                        next_state_id <- sample( c( which(STATE_NAMES == "01_latent_infections_not_isolated_omicron"), which(STATE_NAMES == "01_latent_infections_isolated_omicron") ),
                                                 1,
                                                 prob = c((1 - CONST_q_omicron), CONST_q_omicron))
                      }

                      # Check if Wild, Alpha, Beta, Gamma or Delta
                      else if(infection_WHO_label == "Wild" ||
                              infection_WHO_label == "Alpha" ||
                              infection_WHO_label == "Beta" ||
                              infection_WHO_label == "Gamma" ||
                              infection_WHO_label == "Delta" ){
                        next_state_id <- sample( c( which(STATE_NAMES == "01_latent_infections_not_isolated"), which(STATE_NAMES == "01_latent_infections_isolated") ),
                                                 1,
                                                 prob = c((1 - CONST_q), CONST_q))
                      }

                      # Else throw warning on console output
                      else {
                        # Debug:
                        cat("[WARNING] Cannot find post-infection state for person_id: ",  susceptible_agent_id, ", with infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], "\n", sep = "")
                      }
                    }


                    # Infection event for person with 2 doses
                    else if (PREV_STATE_NAME == "received_dose2"){
                      # Check if omicron
                      if(infection_WHO_label == "Omicron") {
                        next_state_id <- sample( c( which(STATE_NAMES == "02_latent_infections_not_isolated_omicron"), which(STATE_NAMES == "02_latent_infections_isolated_omicron") ),
                                                 1,
                                                 prob = c((1 - CONST_q_omicron), CONST_q_omicron))
                      }

                      # Check if Wild, Alpha, Beta, Gamma or Delta
                      else if(infection_WHO_label == "Wild" ||
                              infection_WHO_label == "Alpha" ||
                              infection_WHO_label == "Beta" ||
                              infection_WHO_label == "Gamma" ||
                              infection_WHO_label == "Delta" ){
                        next_state_id <- sample( c( which(STATE_NAMES == "02_latent_infections_not_isolated"), which(STATE_NAMES == "02_latent_infections_isolated") ),
                                                 1,
                                                 prob = c((1 - CONST_q), CONST_q))
                      }

                      # Else throw warning on console output
                      else {
                        # Debug:
                        cat("[WARNING] Cannot find post-infection state for person_id: ",  susceptible_agent_id, ", with infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], "\n", sep = "")
                      }
                    }

                    # Infection event for person with 3 doses
                    else if (PREV_STATE_NAME == "received_dose3"){
                      # Check if omicron
                      if(infection_WHO_label == "Omicron") {
                        next_state_id <- sample( c( which(STATE_NAMES == "03_latent_infections_not_isolated_omicron"), which(STATE_NAMES == "03_latent_infections_isolated_omicron") ),
                                                 1,
                                                 prob = c((1 - CONST_q_omicron), CONST_q_omicron))
                      }

                      # Check if Wild, Alpha, Beta or Delta
                      else if(infection_WHO_label == "Wild" ||
                              infection_WHO_label == "Alpha" ||
                              infection_WHO_label == "Beta" ||
                              infection_WHO_label == "Gamma" ||
                              infection_WHO_label == "Delta" ){
                        next_state_id <- sample( c( which(STATE_NAMES == "03_latent_infections_not_isolated"), which(STATE_NAMES == "03_latent_infections_isolated") ),
                                                 1,
                                                 prob = c((1 - CONST_q), CONST_q))
                      }

                      # Else throw warning on console output
                      else {
                        # Debug:
                        cat("[WARNING] Cannot find post-infection state for person_id: ",  susceptible_agent_id, ", with infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], ", missing dose 3 state for non-omicron variants" ,"\n", sep = "")
                      }
                    }
                  } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR") {

                    # Infection event for un-vaccinated
                    if (PREV_STATE_NAME == "susceptible"){
                      next_state_id <- which(STATE_NAMES == "infected")
                    }
                    else {
                      # Debug:
                      cat("[WARNING] Cannot find post-infection state for person_id: ",  susceptible_agent_id, ", with infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], "\n", sep = "")
                    }

                  } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation") {

                    # Infection event for un-vaccinated
                    if (PREV_STATE_NAME == "susceptible"){
                      next_state_id <- which(STATE_NAMES == "(unconfirmed) infected")
                    }
                    else {
                      # Debug:
                      cat("[WARNING] Cannot find post-infection state for person_id: ",  susceptible_agent_id, ", with infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], "\n", sep = "")
                    }

                  } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") {

                    # Infection event for un-vaccinated
                    if (PREV_STATE_NAME == "susceptible"){
                      next_state_id <- which(STATE_NAMES == "(unconfirmed) infected")
                    } else {
                      # Debug:
                      cat("\n[WARNING] Cannot find post-infection state for person_id: ",  susceptible_agent_id, " (", PREV_STATE_NAME, "), with infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], "\n", sep = "")
                    }

                  } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") {

                    # Infection event for un-vaccinated
                    if (PREV_STATE_NAME == "susceptible"){
                      next_state_id <- which(STATE_NAMES == "(unconfirmed) infected")

                    } else {
                      # Debug:
                      cat("\n[WARNING] Cannot find post-infection state for person_id: ",  susceptible_agent_id, " (", PREV_STATE_NAME, "), with infectee_id: " ,  infector_id, ", with variant: ", infection_hist_mat[infector_id, "variant"], "\n", sep = "")
                    }

                  } else {

                    cat("\n [WARNING] Iterating over infectious, Infectious state assignment for Location based Contact Matrix has not been implemented for disease model: ", CONST_CHOSEN_DISEASE_MODEL)
                  }

                  moved_to_non_susceptible_today <- c(moved_to_non_susceptible_today, susceptible_agent_id)
                  current_day_susceptible_person_ids <- current_day_susceptible_person_ids[! current_day_susceptible_person_ids %in% c(susceptible_agent_id)]

                  STATE[susceptible_agent_id, day_index] <- next_state_id
                  next_state_name <- STATE_NAMES[next_state_id]


                  # Book keeping
                  infection_hist_mat[susceptible_agent_id, "variant"] <- toString(infection_hist_mat[infector_id, "variant"])
                  infection_hist_mat[susceptible_agent_id, "infected on"] <- day_index
                  infection_hist_mat[susceptible_agent_id, "infected by"] <- infector_id
                  infection_hist_mat[susceptible_agent_id, "citisketch_location_type"] <- contact_event_location_type
                  infection_hist_mat[susceptible_agent_id, "citisketch_location_id"] <- contact_event_location_id

                  # Debug:
                  # Bookkeeping with variants
                  # cat("           Person: ", person_id,
                  #    ", infected with ", infection_hist_mat[person_id, "variant"],
                  #    ", by person: ", contact_index,
                  #    ", on sim day: ", day_index, "\n")

                }  # End of Single infection state change with book-keeping - For location wise contact times

              }


            }



          } # End of - For every susceptible neighbour of an infectious agent

        } # End of - For every infectous agent in list of infectious agents
      }
      # Note: Not looking out for transitions that may be a mix of both (event based and static)

      # Non-susceptible
      # Note: Without this guard clause the STATE vector was mutating to lists
      
      # Debug: 
      # cat("\nEvolving today: \n")
      # print(as.data.frame(table(STATE_NAMES[STATE[previous_day_non_susceptible_person_ids, day_index - 1]])))
      # cat("\n")
      
      if(length(previous_day_non_susceptible_person_ids) > 0){
        # Previous day states
        # previous_day_infected_person_states <- STATE[previous_day_infectious_person_ids, day_index - 1]
        previous_day_non_susceptible_person_states <- STATE[previous_day_non_susceptible_person_ids, day_index - 1]
        # Evolve
        concurrentEvolutionResult <- sapply(previous_day_non_susceptible_person_ids, evolveNonSusceptible)
        # Update state
        STATE[previous_day_non_susceptible_person_ids, day_index] <- concurrentEvolutionResult
      }

      # Check for transitions into active infections (Book-keeping)

      # For "SIR" iterate over moved_to_non_susceptible_today
      if(CONST_CHOSEN_DISEASE_MODEL == "SIR"){
        if(length(moved_to_non_susceptible_today) > 0) {
          for(person_id in moved_to_non_susceptible_today){

            PREV_STATE_INDEX <- STATE[person_id, day_index - 1]
            next_state_id <- STATE[person_id, day_index]

            PREV_STATE_NAME <- STATE_NAMES[PREV_STATE_INDEX]
            next_state_name <- STATE_NAMES[next_state_id]

            if( next_state_id %in% STATELIST_NEW_CASES ) {
              # Edge: "susceptible", "infected"
              if((PREV_STATE_NAME == "susceptible") && (next_state_name == "infected")){
                COUNT_NEW_INFECTIONS_TODAY <- COUNT_NEW_INFECTIONS_TODAY + 1
              }
            }

          }
        }
      }


      # For Complex model and SIR++ where observed infections are post latent infection states
      # Therefore iterating over evolution of non susceptible ids
      if( ( (CONST_CHOSEN_DISEASE_MODEL == "Complex") ||
            (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation") ||
            (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") ||
            (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") ) &&
          (length(previous_day_non_susceptible_person_states) > 0) ) {
        for(person_index in 1:length(previous_day_non_susceptible_person_states)){

          PREV_STATE_INDEX <- previous_day_non_susceptible_person_states[person_index]
          next_state_id <- concurrentEvolutionResult[person_index]

          PREV_STATE_NAME <- STATE_NAMES[PREV_STATE_INDEX]
          next_state_name <- STATE_NAMES[next_state_id]

          # Debug:
          # cat("\n Checking for special transition edges::::::........")

          # For each person check if transition occurred
          if(PREV_STATE_INDEX != next_state_id){
            # Debug:
            # cat("\n Analysing state change of Person id: ", previous_day_non_susceptible_person_ids[person_index], ":  ", STATE_NAMES[PREV_STATE_INDEX], "to ", STATE_NAMES[next_state_id])
            if(CONST_CHOSEN_DISEASE_MODEL == "Complex"){
              if( next_state_id %in% STATELIST_NEW_CASES) {

                # Check for correct transitions to "NEW CASES" (interpret as person has been tested and confirmed)
                if(
                  # Edge: "xx_latent_infections_isolated", "xx_pre_symptomatic_isolated", # π(2 → 4)
                  # For Wild, Alpha, Beta, Gamma, Delta
                  (PREV_STATE_NAME == "00_latent_infections_isolated") && (next_state_name == "00_pre_symptomatic_isolated")  ||
                  (PREV_STATE_NAME == "01_latent_infections_isolated") && (next_state_name == "01_pre_symptomatic_isolated")  ||
                  (PREV_STATE_NAME == "02_latent_infections_isolated") && (next_state_name == "02_pre_symptomatic_isolated")  ||

                  # For Omicron
                  (PREV_STATE_NAME == "00_latent_infections_isolated_omicron") && (next_state_name == "00_pre_symptomatic_isolated_omicron")  ||
                  (PREV_STATE_NAME == "01_latent_infections_isolated_omicron") && (next_state_name == "01_pre_symptomatic_isolated_omicron")  ||
                  (PREV_STATE_NAME == "02_latent_infections_isolated_omicron") && (next_state_name == "02_pre_symptomatic_isolated_omicron")  ||
                  (PREV_STATE_NAME == "03_latent_infections_isolated_omicron") && (next_state_name == "03_pre_symptomatic_isolated_omicron")  ) {
                  COUNT_NEW_INFECTIONS_TODAY <- COUNT_NEW_INFECTIONS_TODAY + 1
                } else if(
                  #  Edge: "xx_pre_symptomatic_non_isolated", "xx_mild_isolated", # π(3 → 6)
                  # For Wild, Alpha, Beta, Gamma, Delta
                  (PREV_STATE_NAME == "00_pre_symptomatic_non_isolated") && (next_state_name == "00_mild_isolated")  ||
                  (PREV_STATE_NAME == "01_pre_symptomatic_non_isolated") && (next_state_name == "01_mild_isolated")  ||
                  (PREV_STATE_NAME == "02_pre_symptomatic_non_isolated") && (next_state_name == "02_mild_isolated")  ||

                  # For Omicron
                  (PREV_STATE_NAME == "00_pre_symptomatic_non_isolated_omicron") && (next_state_name == "00_mild_isolated_omicron")  ||
                  (PREV_STATE_NAME == "01_pre_symptomatic_non_isolated_omicron") && (next_state_name == "01_mild_isolated_omicron")  ||
                  (PREV_STATE_NAME == "02_pre_symptomatic_non_isolated_omicron") && (next_state_name == "02_mild_isolated_omicron")  ||
                  (PREV_STATE_NAME == "03_pre_symptomatic_non_isolated_omicron") && (next_state_name == "03_mild_isolated_omicron")  ) {
                  COUNT_NEW_INFECTIONS_TODAY <- COUNT_NEW_INFECTIONS_TODAY + 1
                }
              }
            } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){

              # Debug:
              # cat("\n ---- Entering SIR++ block")

              # Please note the following edges are accounted while geenrating test results
              # Edge: "(unconfirmed) infected" ---Same day testing temp state "(isolated + unconfirmed) infected"--- Test Result (TPR) --> "(isolated + confirmed) infected"
              # Edge: "susceptible" -- Same day testing temp state "(isolated) susceptible" -- Test Result (FPR) --> (isolated + false positive) susceptible


              if(next_state_id %in% STATELIST_SUSCEPTIBLE){
                # Debug:
                # cat("\n |--- next state is in ", STATE_NAMES[STATELIST_SUSCEPTIBLE])

                if( (PREV_STATE_NAME == "(isolated + false positive) susceptible") && (next_state_name == "susceptible") ){
                  # Edge: "(isolated) susceptible" -- Test Result (TNR) --> susceptible
                  # Move the person id back to susceptible at the end of the day

                  # Debug:
                  # cat("\n |--- Changed Person id: ", previous_day_non_susceptible_person_ids[person_index], ":  ", STATE_NAMES[PREV_STATE_INDEX], "to ", STATE_NAMES[next_state_id])
                  # cat("\n '----- moved to sus list: ", moved_to_susceptible_today)

                  moved_to_susceptible_today <- c(moved_to_susceptible_today, previous_day_non_susceptible_person_ids[person_index])
                  # cat("\n '----* moved to sus list: ", moved_to_susceptible_today, "\n")
                }
              }
            } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){

              # Debug:
              # cat("\n ---- Entering SIR+++ block")

              # Please note the following edges are accounted while geenrating test results
              # Edge: "(unconfirmed) infected" ---Same day testing temp state "(isolated + unconfirmed) infected"--- Test Result (TPR) --> "(isolated + confirmed) infected"
              # Edge: "susceptible" -- Same day testing temp state "(isolated) susceptible" -- Test Result (FPR) --> (isolated + false positive) susceptible
              # Edge: "removed" -- Same day testing temp state "(isolated) removed" -- Test Result (FPR) --> (isolated + false positive) removed


              if(next_state_id %in% STATELIST_SUSCEPTIBLE){
                # Debug:
                # cat("\n |--- next state is in ", STATE_NAMES[STATELIST_SUSCEPTIBLE])

                if( (PREV_STATE_NAME == "(isolated + false positive) susceptible") && (next_state_name == "susceptible") ){
                  # Edge: "(isolated) susceptible" -- Test Result (TNR) --> susceptible
                  # Move the person id back to susceptible at the end of the day

                  # Debug:
                  # cat("\n |--- Changed Person id: ", previous_day_non_susceptible_person_ids[person_index], ":  ", STATE_NAMES[PREV_STATE_INDEX], "to ", STATE_NAMES[next_state_id])
                  # cat("\n '----- moved to sus list: ", moved_to_susceptible_today)

                  moved_to_susceptible_today <- c(moved_to_susceptible_today, previous_day_non_susceptible_person_ids[person_index])
                  # cat("\n '----* moved to sus list: ", moved_to_susceptible_today, "\n")
                }
              }
            } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){

              # Please note the following edges are accounted while geenrating test results
              # Edge: "(unconfirmed) infected" ---Same day testing temp state "(isolated + unconfirmed) infected"--- Test Result (TPR) --> "(isolated + confirmed) infected"
              # Edge: "susceptible" -- Same day testing temp state "(isolated) susceptible" -- Test Result (FPR) --> (isolated + false positive) susceptible
              # Edge: "recovered" -- Same day testing temp state "(isolated) recovered" -- Test Result (FPR) --> (isolated + false positive) recovered


              if(next_state_id %in% STATELIST_SUSCEPTIBLE){
                # Debug:
                # cat("\n |--- next state is in ", STATE_NAMES[STATELIST_SUSCEPTIBLE])

                if( (PREV_STATE_NAME == "(isolated + false positive) susceptible") && (next_state_name == "susceptible") ){
                  # Edge: "(isolated) susceptible" -- Test Result (TNR) --> susceptible
                  # Move the person id back to susceptible at the end of the day

                  # Debug:
                  # cat("\n |--- Changed Person id: ", previous_day_non_susceptible_person_ids[person_index], ":  ", STATE_NAMES[PREV_STATE_INDEX], "to ", STATE_NAMES[next_state_id])
                  # cat("\n '----- moved to sus list: ", moved_to_susceptible_today)

                  moved_to_susceptible_today <- c(moved_to_susceptible_today, previous_day_non_susceptible_person_ids[person_index])
                  # cat("\n '----* moved to sus list: ", moved_to_susceptible_today, "\n")
                }
              }
            }




          }
        }

      }


    }

    # Book-keeping
    {
      # Tabulate new infections


      if( (CONST_CHOSEN_DISEASE_MODEL == "SIR") ||
          (CONST_CHOSEN_DISEASE_MODEL == "Complex")){
        # For models without test based new case discovery
        DAILY_NEW_CASES[day_index] <- COUNT_NEW_INFECTIONS_TODAY
      } else if ( (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation") ||
                  (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") ||
                  (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") ){
        # For models with test based new case discovery
        DAILY_NEW_CASES[day_index - 1] <- COUNT_NEW_INFECTIONS_YESTERDAY
      } else {
        cat("\n[WARNING] Could not tabulate new cases for disease model: ", CONST_CHOSEN_DISEASE_MODEL)
      }
      # Debug:
      # cat("      \n New infections today (unobserved): ", COUNT_NEW_INFECTIONS_TODAY, "\n")

      # Check if active cases today were beyond the threshold for school closures
      current_day_df <- data.frame(STATE[ , day_index])
      colnames(current_day_df) <- c("state_id")
      COUNT_ACTIVE_CASES_TODAY <- nrow(filter(current_day_df, state_id %in% STATELIST_ACTIVE_CASES))

      previous_day_df <- data.frame(STATE[ , day_index - 1])
      colnames(previous_day_df) <- c("state_id")
      COUNT_ACTIVE_CASES_YESTERDAY <- nrow(filter(previous_day_df, state_id %in% STATELIST_ACTIVE_CASES))
      
      COUNT_UNOBSERVED_INFECTIOUS_YESTERDAY <- nrow(filter(previous_day_df, state_id %in% STATELIST_INFECTIOUS) )

      # this_day_active_cases_person_ids <- which(sapply(STATE[ , day_index], FUN = isActiveCaseState) == TRUE)
      # COUNT_ACTIVE_CASES <- length(this_day_active_cases_person_ids)

      # Book keeping for SIR, Complex
      DAILY_ACTIVE_CASES [day_index] <- COUNT_ACTIVE_CASES_TODAY

      # Book keeping for SIR + Testing + Isolation, SIR + Testing + Isolation + Contact Tracing
      DAILY_ACTIVE_CASES [day_index - 1] <- COUNT_ACTIVE_CASES_YESTERDAY
      
      DAILY_UNOBSERVED_INFECTIOUS_CASES[day_index - 1] <- COUNT_UNOBSERVED_INFECTIOUS_YESTERDAY

      # For models with testing log test positivity
      if( (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation") ||
          (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") ||
          (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") ){
        agents_received_test_result_yesterday_ids <- which(test_schedule_mat[ , "result day"] == day_index - 1)

        total_covid_test_result_yesterday <- length(agents_received_test_result_yesterday_ids)
        total_negative_result_yesterday <- length(which(test_schedule_mat[agents_received_test_result_yesterday_ids, "result"] == "negative"))
        total_positive_result_yesterday <- length(which(test_schedule_mat[agents_received_test_result_yesterday_ids, "result"] == "positive"))

        test_positivity_rate_yesterday <- 0.0

        if(total_covid_test_result_yesterday > 0){
          test_positivity_rate_yesterday <- total_positive_result_yesterday / total_covid_test_result_yesterday
        }

        COVID_TEST_STATS_mat[day_index - 1, "total result received"] <- total_covid_test_result_yesterday
        COVID_TEST_STATS_mat[day_index - 1, "positive result"] <- total_positive_result_yesterday
        COVID_TEST_STATS_mat[day_index - 1, "negative result"] <- total_negative_result_yesterday
        COVID_TEST_STATS_mat[day_index - 1, "test positivity"] <- test_positivity_rate_yesterday
      }

      # if there's another contact matrix available, then iterate
      # Generates "Error: StopIteration" when reaches end
      contact_matrix <- tryCatch(
        {
          nextElem(contact_matrix_iterator)
        },
        error = function(e){
          # cat("Setting up day ", day_index, " with last known Contact matrix index: ", COUNT_CONTACT_MATRIX)
          # contact_matrix

          # Calibration mode:
          # Reset contact matrix to first index
          # Re-initialise iterator
          contact_matrix_iterator <- iter(list_of_contact_matrix)

          # Get first contact matrix
          # contact_matrix_as_pairlist <- nextElem(contact_matrix_iterator)
          nextElem(contact_matrix_iterator)

        }
      )
    }



    # Set-up infectious, susceptibles, non-susceptibles and remove terminals before next day
    {
      # Set-up set of susceptibles, non-susceptibles and infectious for next day

      # Remove the newly non-susceptible from susceptible set
      previous_day_susceptible_person_ids <- setdiff(previous_day_susceptible_person_ids, moved_to_non_susceptible_today)

      # Add the newly non-susceptible to non-susceptible set
      previous_day_non_susceptible_person_ids <- union(previous_day_non_susceptible_person_ids, moved_to_non_susceptible_today)
      # Remove newly susceptible frin non-susceptible set
      previous_day_non_susceptible_person_ids <- setdiff(previous_day_non_susceptible_person_ids, moved_to_susceptible_today)

      # If spreading infection iterating over infectious instead of susceptible, propagate state
      if(!ITERATE_OVER_SUSCEPTIBLE){
        STATE[previous_day_susceptible_person_ids, day_index] <- STATE[previous_day_susceptible_person_ids, day_index - 1]
      }

      # Add newly transitioned to susceptible set
      previous_day_susceptible_person_ids <- union(previous_day_susceptible_person_ids, moved_to_susceptible_today)

      # Refresh infected person ids
      previous_day_infectious_person_ids <- which(sapply(STATE[ , day_index], FUN = isInfectiousState) == TRUE)

      # Find the agent-ids moved to a final state in the transition matrix a.k.a terminal states
      this_day_terminal_state_person_ids <- c()
      
      if(!is.null(STATELIST_TERMINAL)){
        this_day_terminal_state_person_ids <- which(sapply(STATE[ , day_index], FUN = isTerminalState) == TRUE)
      }
      
      # Set the rest of their state vector to this state
      if(length(this_day_terminal_state_person_ids) > 0 & (day_index < TOTAL_SIMULATION_DAYS)){

        # cat("Debug here")
        for(terminal_person_id in this_day_terminal_state_person_ids){
          terminal_state <- STATE[terminal_person_id, day_index]
          STATE[terminal_person_id, (day_index + 1) : TOTAL_SIMULATION_DAYS] <- terminal_state

          # previous_day_non_susceptible_person_ids <- previous_day_non_susceptible_person_ids[previous_day_non_susceptible_person_ids != terminal_person_id]
        }
      }

      # Remove them from non-susceptible list
      previous_day_non_susceptible_person_ids <- setdiff(previous_day_non_susceptible_person_ids, this_day_terminal_state_person_ids)
    }

    # Verbose:
    cat("\r")
    # Tag: calib08
    # Denote event days
    if( GENERATE_EVENT_CONTACT_PAIRS && day_index >= EVENT_START_DATE && day_index <= EVENT_END_DATE){
      cat("\r    Concluded - Run: ", run_index, ", Day (with Event): ", day_index, sep = "")
    } else {
      cat("\r    Concluded - Run: ", run_index, ", Day: ", day_index, sep = "")
    }
    cat("  ::  Infectious count: ", length(previous_day_infectious_person_ids),
        ", Susceptible count: ", length(previous_day_susceptible_person_ids),
        ", Non-Susceptible count: ", length(previous_day_non_susceptible_person_ids),
        ", Moved to terminal count: ", length(this_day_terminal_state_person_ids), sep ="")

    # Simulation preemptive ending condition, no infectious agent and all agents are either in susceptible or in terminal states
    if(
      ( length(previous_day_infectious_person_ids) == 0 ) &
      ( (length(previous_day_susceptible_person_ids) + length(this_day_terminal_state_person_ids)) == TOTAL_SIMULATED_PERSONS) &
      (day_index < TOTAL_SIMULATION_DAYS)
    ){
      # Replicate this state till end of sim
      STATE[ , (day_index + 1) : TOTAL_SIMULATION_DAYS ] <- STATE[ , day_index]
      day_index <- TOTAL_SIMULATION_DAYS
      break
    }


  } # End of day iterator

  # setting Time as unique run identifier
  UUID <- Sys.time()

  # Run config and metadata
  run_config_mat <- matrix (ncol = 20, nrow = 2)
  run_config_mat_colnames <- c("CONST_CHOSEN_DISEASE_MODEL",
                               "CONST_ENABLE_CONTACT_TRACING",
                               "CONST_MAX_CHANCE",
                               "CONST_USE_INFECTIOUSNESS_PDF",
                               "GENERATE_EVENT_CONTACT_PAIRS",
                               "CONST_TIME_TILL_76p_CHANCE",
                               "TOTAL_SIMULATION_DAYS",
                               "TOTAL_SIMULATED_PERSONS",
                               "sampled_y0",
                               "sampled_y1",
                               "sampled_y2",
                               "sampled_y3",
                               "COUNT_FIRST_DAY_DISEASE_IMPORT",
                               "CONST_CHOSEN_CONTACT_MATRIX_TYPE",
                               "COUNT_CONTACT_MATRIX",
                               "COUNT_NO_OF_INFECTION_COIN_FLIPS",
                               "COUNT_NO_OF_INFECTION_EVENTS",
                               "sim_setup_time",
                               "sim_run_time",
                               "SIM_SCRIPT_NAME"
                               )

  colnames(run_config_mat) <- run_config_mat_colnames

  run_config_mat[1, "CONST_CHOSEN_DISEASE_MODEL"] <- CONST_CHOSEN_DISEASE_MODEL
  run_config_mat[2, "CONST_CHOSEN_DISEASE_MODEL"] <- "Human readable disease model name"
  if( (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation") ||
      (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") ) { run_config_mat[2, "CONST_CHOSEN_DISEASE_MODEL"] <- paste(run_config_mat[2, "CONST_CHOSEN_DISEASE_MODEL"], " with Testing strategy: ", CONST_CHOSEN_TEST_ALLOCATION_STRATEGY) }

  run_config_mat[1, "CONST_ENABLE_CONTACT_TRACING"] <- CONST_ENABLE_CONTACT_TRACING
  run_config_mat[2, "CONST_ENABLE_CONTACT_TRACING"] <- "Contact tracing based mandatory testing"
  if(CONST_ENABLE_CONTACT_TRACING){ run_config_mat[2, "CONST_ENABLE_CONTACT_TRACING"] <- paste(run_config_mat[2, "CONST_ENABLE_CONTACT_TRACING"], " with Tracing strategy: ", CONST_CHOSEN_CONTACT_TRACING_STRATEGY) }

  run_config_mat[1, "CONST_MAX_CHANCE"] <- CONST_MAX_CHANCE
  run_config_mat[2, "CONST_MAX_CHANCE"] <- "default amplitude for infectivity tanh function"

  run_config_mat[1, "CONST_USE_INFECTIOUSNESS_PDF"] <- CONST_USE_INFECTIOUSNESS_PDF
  run_config_mat[2, "CONST_USE_INFECTIOUSNESS_PDF"] <- paste("Infectiousness PDF: ", body(normalised_infectiousness_pdf)[2] ) # Note: Hacky, needs refactor

  run_config_mat[1, "GENERATE_EVENT_CONTACT_PAIRS"] <- GENERATE_EVENT_CONTACT_PAIRS
  run_config_mat[2, "GENERATE_EVENT_CONTACT_PAIRS"] <- "Rudimentary event contact pair augment"
  if(GENERATE_EVENT_CONTACT_PAIRS){ run_config_mat[2, "GENERATE_EVENT_CONTACT_PAIRS"] <- paste(run_config_mat[2, "GENERATE_EVENT_CONTACT_PAIRS"], " from ", EVENT_START_DATE, " to ", EVENT_END_DATE, "sim day, with duration: ", EVENT_DURATION_IN_SECONDS, " and expected population: ", COUNT_EVENT_ATTENDANCE); }

  run_config_mat[1, "CONST_TIME_TILL_76p_CHANCE"] <- CONST_TIME_TILL_76p_CHANCE
  run_config_mat[2, "CONST_TIME_TILL_76p_CHANCE"] <- "Time in seconds to scale the infection chance w.r.t contact time"

  run_config_mat[1, "TOTAL_SIMULATION_DAYS"] <- TOTAL_SIMULATION_DAYS
  run_config_mat[2, "TOTAL_SIMULATION_DAYS"] <- "No. of days sim was run + 1 extra day of disease import"

  run_config_mat[1, "TOTAL_SIMULATED_PERSONS"] <- TOTAL_SIMULATED_PERSONS
  run_config_mat[2, "TOTAL_SIMULATED_PERSONS"] <- "Agent population"

  run_config_mat[1, "sampled_y0"] <- sampled_y0
  run_config_mat[2, "sampled_y0"] <- "Initial unvaccinated fraction"

  run_config_mat[1, "sampled_y1"] <- sampled_y1
  run_config_mat[2, "sampled_y1"] <- "Initial single dose vaccinated fraction"

  run_config_mat[1, "sampled_y2"] <- sampled_y2
  run_config_mat[2, "sampled_y2"] <- "Initial double dose vaccinated fraction"

  run_config_mat[1, "sampled_y3"] <- sampled_y3
  run_config_mat[2, "sampled_y3"] <- "Initial triple dose vaccinated fraction"

  run_config_mat[1, "COUNT_FIRST_DAY_DISEASE_IMPORT"] <- COUNT_FIRST_DAY_DISEASE_IMPORT
  run_config_mat[2, "COUNT_FIRST_DAY_DISEASE_IMPORT"] <- "Initialised no. of infection for simulation day 1"

  run_config_mat[1, "CONST_CHOSEN_CONTACT_MATRIX_TYPE"] <- CONST_CHOSEN_CONTACT_MATRIX_TYPE
  run_config_mat[2, "CONST_CHOSEN_CONTACT_MATRIX_TYPE"] <- "Aggregated contact matrices sum the shared time between pairs of agent across all locations (unlike Location based ones)"

  run_config_mat[1, "COUNT_CONTACT_MATRIX"] <- COUNT_CONTACT_MATRIX
  run_config_mat[2, "COUNT_CONTACT_MATRIX"] <- "No. of contact matrix loaded"

  run_config_mat[1, "COUNT_NO_OF_INFECTION_COIN_FLIPS"] <- COUNT_NO_OF_INFECTION_COIN_FLIPS
  run_config_mat[2, "COUNT_NO_OF_INFECTION_COIN_FLIPS"] <- "No. of draws to check for probabilistic infection step"

  run_config_mat[1, "COUNT_NO_OF_INFECTION_EVENTS"] <- COUNT_NO_OF_INFECTION_EVENTS
  run_config_mat[2, "COUNT_NO_OF_INFECTION_EVENTS"] <- "No. of positive infections throughout the sim"

  run_config_mat[1, "sim_setup_time"] <- sim_setup_time[["elapsed"]]
  run_config_mat[2, "sim_setup_time"] <- "Time to load data till first simulation step"

  sim_run_time <- proc.time() - sim_time

  run_config_mat[1, "sim_run_time"] <- sim_run_time[["elapsed"]]
  run_config_mat[2, "sim_run_time"] <- "Total time taken to simulate"

  run_config_mat[1, "SIM_SCRIPT_NAME"] <- SIM_SCRIPT_NAME
  run_config_mat[2, "SIM_SCRIPT_NAME"] <- "Result directory"








  # set output path
  OUTPUT_CONFIG_FILEPATH_CSV <- paste(OUTPUT_DIRECTORY, UUID, "-CONFIG.csv",  sep = "")

  # write all state transitions as CSV
  write.table(run_config_mat, sep=",", OUTPUT_CONFIG_FILEPATH_CSV, row.names = FALSE, col.names = TRUE)

  # set output path
  OUTPUT_TRANSITION_MAT_FILEPATH_CSV <- paste(OUTPUT_DIRECTORY, UUID, "-DISEASE_MODEL.csv",  sep = "")

  # write the disease model (state machine) with calculated weights
  write.table(transition_matrix, sep=",", OUTPUT_TRANSITION_MAT_FILEPATH_CSV, row.names = TRUE, col.names = NA)


  # set output path
  OUTPUT_STATE_FILEPATH_CSV <- paste(OUTPUT_DIRECTORY, UUID, "-STATE.csv",  sep = "")

  # write all state transitions as CSV
  write.table(STATE, sep=",", OUTPUT_STATE_FILEPATH_CSV, row.names = FALSE, col.names = FALSE)



  # Human readable names
  # set output path
  OUTPUT_STATENAMES_FILEPATH_CSV <- paste(OUTPUT_DIRECTORY, UUID, "-STATENAME.csv",  sep = "")

  # STATE_NAMES <- data.frame(STATE_NAMES)
  # STATE_NAMES$state_id <- seq.int(nrow(STATE_NAMES))

  # write all state_id -> state name mapping as CSV
  write.table(STATE_NAMES, sep=",", OUTPUT_STATENAMES_FILEPATH_CSV, row.names = TRUE, col.names = FALSE)



  # Daily new cases
  OUTPUT_NEWCASES_FILEPATH_CSV <- paste(OUTPUT_DIRECTORY, UUID, "-DAILY-NEW-CASES.csv",  sep = "")

  # write all Daily new cases
  write.table(DAILY_NEW_CASES, sep=",", OUTPUT_NEWCASES_FILEPATH_CSV, row.names = FALSE, col.names = FALSE)



  # Total active cases each day
  OUTPUT_ACTIVECASES_FILEPATH_CSV <- paste(OUTPUT_DIRECTORY, UUID, "-DAILY-ACTIVE-CASES.csv",  sep = "")

  # write total active cases each day
  write.table(DAILY_ACTIVE_CASES, sep=",", OUTPUT_ACTIVECASES_FILEPATH_CSV, row.names = FALSE, col.names = FALSE)



  # Infection history
  OUTPUT_INFECTION_HISTORY_FILEPATH_CSV <- paste(OUTPUT_DIRECTORY, UUID, "-INFECTION-HISTORY.csv",  sep = "")

  # write all Daily new cases
  write.table(infection_hist_mat, sep=",", OUTPUT_INFECTION_HISTORY_FILEPATH_CSV, row.names = TRUE, col.names = NA)


  # For models with testing log test positivity log Covid testing stats
  if( (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation")  ||
      (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing") ||
      (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing") ){

  # Covid testing stats per day
    OUTPUT_COVID_TEST_STATS_CSV <- paste(OUTPUT_DIRECTORY, UUID, "-DAILY-COVID-TEST.csv",  sep = "")

    # write all Daily Covid testing stats
    write.table(COVID_TEST_STATS_mat, sep=",", OUTPUT_COVID_TEST_STATS_CSV, row.names = TRUE, col.names = NA)
  }






  # Debug:

  # Initial infections
  # cat("\n Initial counts: \n")
  # cat(table(day_1_variants_list))
  # table(day_1_varaint_list)

  # Final infections
  cat("\n\nPost simulation counts: for run no.: ", run_index, " of ", TOTAL_SIMULATION_RUNS,"\n", sep ="")
  cat("\n....................................\n")
  cat("No. of Wild infections: ", length(which(infection_hist_mat[, "variant"] == "A")), "\n")
  cat("No. of Alpha infections: ", length(which(infection_hist_mat[, "variant"] == "B.1.1.7")), "\n")
  cat("No. of Delta infections: ", length(which(infection_hist_mat[, "variant"] == "B.1.617.2")), "\n")
  cat("No. of Omicron infections: ", length(which(infection_hist_mat[, "variant"] == "B.1.1.529")), "\n")
  cat("-------------------\n")
  cat("No. of random samples drawn to check for infection success: ", COUNT_NO_OF_INFECTION_COIN_FLIPS, "\n", sep = "")
  cat("No. of positive infections: ", COUNT_NO_OF_INFECTION_EVENTS, "\n", sep = "")
  cat("Peak observed active cases: : ", max(DAILY_ACTIVE_CASES), "\n", sep = "")
  cat("Peak unobserved infectious cases: ", max(DAILY_UNOBSERVED_INFECTIOUS_CASES), "\n", sep = "")

  # cat("No. of Possiible infection events: ", random_sample_iterator, "\n")

  # Stop the clock
  cat("\n Sim time for no. of. days: ", TOTAL_SIMULATION_DAYS, ", with no. of. agents: ", TOTAL_SIMULATED_PERSONS, sep = "")
  cat("\n", proc.time() - sim_time)

  # Plot before refreshing
  {
    plot( DAILY_ACTIVE_CASES, col = "#0000FF", type = 'l', 
          main = "(Blue) Observed active vs. (Red) Unobserved infectous", 
          sub =  paste("Model: ", CONST_CHOSEN_DISEASE_MODEL ,", Run no.: ", run_index, " of ", TOTAL_SIMULATION_RUNS, sep = "" ),
          xlab = "Simulation day",
          ylab = "No. of agent")
    
    lines(DAILY_UNOBSERVED_INFECTIOUS_CASES, col = "#FF0000", lwd = 1, lty = 1)
  }


  if(run_index != TOTAL_SIMULATION_RUNS) # Hacky active debug data structures
  {
    cat("\n================================================\n")
    cat("\nResetting data structures for next run...")

    # Refresh data structured for the next run
    rm(STATE)

    STATE <- array(rep(NA, TOTAL_SIMULATED_PERSONS * TOTAL_SIMULATION_DAYS),
                   dim = c(TOTAL_SIMULATED_PERSONS, TOTAL_SIMULATION_DAYS) )



    # Set up simulation population

    # Set every person to susceptible for the first day
    STATE[ , 1] <- which( colnames(transition_matrix) == "susceptible" )


    # Setup infection history for each person in a matrix, where each row is a person id
    rm(infection_hist_mat)

    if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Aggregated"){
      infection_hist_mat <- matrix (ncol = 6, nrow = TOTAL_SIMULATED_PERSONS)
      infection_hist_mat_colnames <- c("variant", "infected on", "infected by", "dose 1 on", "dose 2 on", "dose 3 on")
      colnames(infection_hist_mat) <- infection_hist_mat_colnames
    } else if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Location based"){
      # Tag: @calib07 adding infection venue details i.e citisketch_location_type, citisketch_location_id
      infection_hist_mat <- matrix (ncol = 8, nrow = TOTAL_SIMULATED_PERSONS)
      infection_hist_mat_colnames <- c("variant", "infected on", "infected by", "dose 1 on", "dose 2 on", "dose 3 on", "citisketch_location_type", "citisketch_location_id")
      colnames(infection_hist_mat) <- infection_hist_mat_colnames
    }



    # agent ids of population > 5 years and are present in the schedules
    possible_vaccinated_agent_list <- intersect(demographic_data_mat[ which( strtoi( demographic_data_mat[ , "age"] ) >=  5)], seq(1:COUNT_MAX_PERSON_ID))

    # Set vaccination state for day 1
    if (CONST_CHOSEN_DISEASE_MODEL == "Complex"){
      for(person_id in possible_vaccinated_agent_list){
        vaccination_state <- sample(c( which(colnames(transition_matrix) == "susceptible"),
                                       which(colnames(transition_matrix) ==  "received_dose1"),
                                       which(colnames(transition_matrix) ==  "received_dose2"),
                                       which(colnames(transition_matrix) ==  "received_dose3")
        ),
        1,
        prob = c(CONST_y0, CONST_y1, CONST_y2, CONST_y3))

        STATE[person_id, 1] <- vaccination_state

        # Extra book-keeping (To eventually incorporate waning)
        if(vaccination_state == which(colnames(transition_matrix) ==  "received_dose3")) {
          # Negative value to state vaccination event occurred before simulation
          infection_hist_mat[person_id, "dose 3 on"] <- -1
          infection_hist_mat[person_id, "dose 2 on"] <- -1
          infection_hist_mat[person_id, "dose 1 on"] <- -1
        }
        else if(vaccination_state == which(colnames(transition_matrix) ==  "received_dose2")) {
          # Negative value to state vaccination event occurred before simulation
          infection_hist_mat[person_id, "dose 2 on"] <- -1
          infection_hist_mat[person_id, "dose 1 on"] <- -1
        }
        else if(vaccination_state == which(colnames(transition_matrix) ==  "received_dose1")) {
          # Negative value to state vaccination event occurred before simulation
          infection_hist_mat[person_id, "dose 1 on"] <- -1
        }
      }
    } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR") {

      # No vaccination in simple SIR
    }

    # Initial populations
    init_list_of_person_with_0_dose <- which(STATE[ , 1] == which(colnames(transition_matrix) == "susceptible"))
    init_list_of_person_with_1_dose <- which(STATE[ , 1] == which(colnames(transition_matrix) == "received_dose1"))
    init_list_of_person_with_2_dose <- which(STATE[ , 1] == which(colnames(transition_matrix) == "received_dose2"))
    init_list_of_person_with_3_dose <- which(STATE[ , 1] == which(colnames(transition_matrix) == "received_dose3"))

    sampled_y0 <- length(init_list_of_person_with_0_dose) / length(possible_vaccinated_agent_list)
    sampled_y1 <- length(init_list_of_person_with_1_dose) / length(possible_vaccinated_agent_list)
    sampled_y2 <- length(init_list_of_person_with_2_dose) / length(possible_vaccinated_agent_list)
    sampled_y3 <-  length(init_list_of_person_with_3_dose) / length(possible_vaccinated_agent_list)

    # Debug
    cat("\n")
    cat("Sampled Y0: ", sampled_y0, "\n")
    cat("Sampled Y1: ", sampled_y1, "\n")
    cat("Sampled Y2: ", sampled_y2, "\n")
    cat("Sampled Y3: ", sampled_y3, "\n")




    rm(DAILY_NEW_CASES)
    DAILY_NEW_CASES <- array( rep(0, TOTAL_SIMULATION_DAYS), dim = TOTAL_SIMULATION_DAYS )

    rm(DAILY_ACTIVE_CASES)
    DAILY_ACTIVE_CASES <- array( rep(0, TOTAL_SIMULATION_DAYS), dim = TOTAL_SIMULATION_DAYS )
    
    rm(DAILY_UNOBSERVED_INFECTIOUS_CASES)
    DAILY_UNOBSERVED_INFECTIOUS_CASES <- array( rep(0, TOTAL_SIMULATION_DAYS), dim = TOTAL_SIMULATION_DAYS )

    # Set first day new cases to COUNT_FIRST_DAY_DISEASE_IMPORT for bookkeeping
    DAILY_NEW_CASES[1] <- COUNT_FIRST_DAY_DISEASE_IMPORT

    # Randomly distribute the infection import
    # first_disease_import <- sample(seq_len(length(STATE[ , 1])), COUNT_FIRST_DAY_DISEASE_IMPORT) # Worked when there were 0 vaccinated on the 1st day

    # first_disease_import <- sample(init_list_of_person_with_0_dose, COUNT_FIRST_DAY_DISEASE_IMPORT)
    # first_disease_import <- sample(init_list_of_person_with_1_dose, COUNT_FIRST_DAY_DISEASE_IMPORT)
    # first_disease_import <- sample(init_list_of_person_with_2_dose, COUNT_FIRST_DAY_DISEASE_IMPORT)
    # first_disease_import <- sample(init_list_of_person_with_3_dose, COUNT_FIRST_DAY_DISEASE_IMPORT)

    # Randomly distribute the infection import
    first_disease_import <- sample(seq_len(length(STATE[ , 1])), COUNT_FIRST_DAY_DISEASE_IMPORT) # Worked when there were 0 vaccinated on the 1st day
    # Freezing disease import to agent 1, 2, 3 and 4 as decided with Robert and Jarod
    # first_disease_import <- c(1, 2, 3, 4)


    # active_voc_df <- filter(voc_df, Proportion > 0)
    # active_voc_mat <- as.matrix.data.frame(active_voc_df)


    day_1_varaint_list <- list()

    cat("Setting first day disease import to: ", COUNT_FIRST_DAY_DISEASE_IMPORT, "\n")
    cat("Infected agent ids: ")
    cat(print(first_disease_import), "\n")

    for(person_id in first_disease_import){


      # Sample based on proportions
      # infecting_variant <- sample(active_voc_df$variant, 1, prob = active_voc_df$Proportion/100)
      # [DONE] Migrate to matrix
      infecting_variant <- sample(active_voc_mat[ , "variant"], 1, prob = as.numeric(active_voc_mat[ , "Proportion"])/100)

      # Setting infection history
      infection_hist_mat[person_id, "variant"] <- toString(infecting_variant)
      infection_hist_mat[person_id, "infected on"] <- 1 # first day disease import

      if(CONST_CHOSEN_CONTACT_MATRIX_TYPE == "Location based"){
        # Special values for first day disease import
        infection_hist_mat[person_id, "citisketch_location_type"] <- "UNKNOWN"
        infection_hist_mat[person_id, "citisketch_location_id"] <- -1
      }

      present_state = STATE[person_id , 1]
      infected_state = "UNKNOWN"

      # Tag: @cailb03, the infeceted states are foced to be "XX_pre_symptomatic_non_isolated_YY
      if (CONST_CHOSEN_DISEASE_MODEL == "Complex") {
        # If 0 vaccination dose
        if(present_state == which(colnames(transition_matrix) == "susceptible")){

          #Check if Omicron
          if(toString(infecting_variant) == "B.1.1.529"){
            # infected_state <- sample( c( which(colnames(transition_matrix) == "00_pre_symptomatic_non_isolated_omicron"),
            #                              which(colnames(transition_matrix) == "00_mild_non_isolated_omicron") ), 1)

            infected_state <- which(colnames(transition_matrix) == "00_pre_symptomatic_non_isolated_omicron")
          }
          # For all other Variants of Concern
          else {
            # infected_state <- sample( c( which(colnames(transition_matrix) == "00_pre_symptomatic_non_isolated"),
            #                              which(colnames(transition_matrix) == "00_mild_non_isolated") ), 1)

            infected_state <- which(colnames(transition_matrix) == "00_pre_symptomatic_non_isolated")
          }
        }

        # If 1 vaccination dose
        else if(present_state == which(colnames(transition_matrix) == "received_dose1")){

          #Check if Omicron
          if(toString(infecting_variant) == "B.1.1.529"){
            # infected_state <- sample( c( which(colnames(transition_matrix) == "01_pre_symptomatic_non_isolated_omicron"),
            #                              which(colnames(transition_matrix) == "01_mild_non_isolated_omicron") ), 1)

            infected_state <- which(colnames(transition_matrix) == "01_pre_symptomatic_non_isolated_omicron")
          }
          # For all other Variants of Concern
          else {
            # infected_state <- sample( c( which(colnames(transition_matrix) == "01_pre_symptomatic_non_isolated"),
            #                              which(colnames(transition_matrix) == "01_mild_non_isolated") ), 1)

            infected_state <- which(colnames(transition_matrix) == "01_pre_symptomatic_non_isolated")
          }
        }

        # If 2 vaccination dose
        else if(present_state == which(colnames(transition_matrix) == "received_dose2")){

          #Check if Omicron
          if(toString(infecting_variant) == "B.1.1.529"){
            # infected_state <- sample( c( which(colnames(transition_matrix) == "02_pre_symptomatic_non_isolated_omicron"),
            #                              which(colnames(transition_matrix) == "02_mild_non_isolated_omicron") ), 1)

            infected_state <- which(colnames(transition_matrix) == "02_pre_symptomatic_non_isolated_omicron")
          }
          # For all other Variants of Concern
          else {
            # infected_state <- sample( c( which(colnames(transition_matrix) == "02_pre_symptomatic_non_isolated"),
            #                              which(colnames(transition_matrix) == "02_mild_non_isolated") ), 1)

            infected_state <- which(colnames(transition_matrix) == "02_pre_symptomatic_non_isolated")
          }
        }

        # If 3 vaccination dose
        else if(present_state == which(colnames(transition_matrix) == "received_dose3")){

          #Check if Omicron
          if(toString(infecting_variant) == "B.1.1.529"){
            # infected_state <- sample( c( which(colnames(transition_matrix) == "03_pre_symptomatic_non_isolated_omicron"),
            #                              which(colnames(transition_matrix) == "03_mild_non_isolated_omicron") ), 1)

            infected_state <- which(colnames(transition_matrix) == "03_pre_symptomatic_non_isolated_omicron")
          }
          # For all other Variants of Concern
          else {
          #   infected_state <- sample( c( which(colnames(transition_matrix) == "03_pre_symptomatic_non_isolated"),
          #                                which(colnames(transition_matrix) == "03_mild_non_isolated") ), 1)

            infected_state <- which(colnames(transition_matrix) == "03_pre_symptomatic_non_isolated")
          }
        }
      } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR"){

        if(present_state == which(colnames(transition_matrix) == "susceptible")){
          infected_state <- which(colnames(transition_matrix) == "infected")

        }
      } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation"){

        if(present_state == which(colnames(transition_matrix) == "susceptible")){
          infected_state <- which(colnames(transition_matrix) == "(unconfirmed) infected")

        }
      } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Contact Tracing"){

        if(present_state == which(colnames(transition_matrix) == "susceptible")){
          infected_state <- which(colnames(transition_matrix) == "(unconfirmed) infected")

        }
      } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR + Testing + Isolation + Hospitalisation + Contact Tracing"){

        if(present_state == which(colnames(transition_matrix) == "susceptible")){
          infected_state <- which(colnames(transition_matrix) == "(unconfirmed) infected")

        }
      } else {

        cat("\n [WARNING] Initial disease import has not been implemented for: ", CONST_CHOSEN_DISEASE_MODEL)
      }


      # Debug
      # cat("person id - to infect: ", person_id,
      #     "with variant: ", infection_hist_mat[person_id, "variant"],
      #     "present state: ", colnames(transition_matrix)[present_state], " -> ",
      #     "infected state: ", colnames(transition_matrix)[infected_state],
      #     "\n")

      # Check which variant to infect with
      STATE[person_id , 1] <- infected_state

      # Book keeping
      # day_1_variants_mat[toString(infecting_variant), "count"] <- day_1_variants_mat[toString(infecting_variant), "count"] + 1
      day_1_varaint_list <- append(day_1_varaint_list, toString(infecting_variant))
    }

    previous_day_susceptible_person_ids <- setdiff(seq(1:TOTAL_SIMULATED_PERSONS), first_disease_import)
    previous_day_non_susceptible_person_ids <- first_disease_import
    
    
    
    
    
    
    # Refresh test scheduler for models with testing
    rm(test_schedule_mat)
    
    test_schedule_mat <- matrix (ncol = 4, nrow = TOTAL_SIMULATED_PERSONS)
    test_schedule_mat_colnames <- c("test type", "test scheduled on", "result day", "result")
    colnames(test_schedule_mat) <- test_schedule_mat_colnames
    
    

    cat("\nReady to begin next run")
    cat("\n\n")
  }
}


# Code-scraps 

# Frequency count of a vector 'x'
# as.data.frame(table(x))
# 
# Eg.
# print(as.data.frame(table(STATE_NAMES[STATE[previous_day_non_susceptible_person_ids, day_index - 1]])))


# Change output from console to a file
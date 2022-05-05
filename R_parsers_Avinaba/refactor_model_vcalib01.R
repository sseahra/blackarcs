# Start the clock!
start_time <- proc.time()

library(dplyr) # for dataframe utitlities
library(iterators)
library(jsonlite) # for read_json()


# SIM_SCRIPT_NAME = 'LocalDebugCalibration_refactor_vcalib01_00375_for_210d'
# SIM_SCRIPT_NAME = 'NoVaccination_refactor_vcalib01_00375_for_210d'

# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_01000_for_210d'

# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_00100_for_210d'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_001095666_for_210d'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_00200_for_210d'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_00250_for_210d'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_00300_for_210d'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_00370_for_210d'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_00400_for_210d'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_00500_for_210d'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_00600_for_210d'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_00700_for_210d'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_00750_for_210d'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_00800_for_210d'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_00900_for_210d'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_01000_for_210d_bactch17Apr'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_01500_for_210d'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_03000_for_210d'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_02500_for_210d'
# SIM_SCRIPT_NAME = 'SIR_frozen_import_refactor_vcalib01_05000_for_210d'


# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_00100_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_001095666_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_00200_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_00250_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_00300_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_00370_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_00400_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_00500_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_00600_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_00700_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_00750_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_00800_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_00900_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_01000_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_01500_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_02500_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_03000_for_210d_gamma1upon9'
# SIM_SCRIPT_NAME = 'Modified_SIR_frozen_import_refactor_vcalib01_05000_for_210d_gamma1upon9'



TOTAL_SIMULATION_DAYS <- 210

CONST_CHOSEN_DISEASE_MODEL <- "SIR"

# Human readable disease model names 
# Note: this is loosely implemented with manual string matching 
DISEASE_MODEL_NAMES <- c ("SIR", "Complex") 

# CONST_FLAG_ITERATE_CONTACT_MATRIX = FALSE # to calibrate for SIR, instead taking a collection of a single contact matrix 



# for Omicron, Amplitude = 0.00375 = CONST_MAX_CHANCE * RELATIVE_INFECTIVITY


# Amplitude of infection                                  |   For Gamma = 1/10            |   For Gamma = 1/9 
# CONST_MAX_CHANCE <- 0.00375/3.42257472363763 # From Robert ~ 39.4% outbreaks - 500 runs | ~ 27.94% - 501 runs 

# To check for linear variance between R0 and Amplitude

# Amplitude of infection      |   For Gamma = 1/10            |   For Gamma = 1/9 
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
# CONST_MAX_CHANCE <- 0.00800 # - 100% outbreaks - 200 runs
# CONST_MAX_CHANCE <- 0.00900 # - 100% outbreaks - 200 runs
# CONST_MAX_CHANCE <- 0.01000 # Calibration for Wild x SIR - 99.5% outbreaks - 200 runs 
# CONST_MAX_CHANCE <- 0.01500
# CONST_MAX_CHANCE <- 0.02500
# CONST_MAX_CHANCE <- 0.03000
# CONST_MAX_CHANCE <- 0.05000

# Hacky diminishing Amplitude to match Robert's

# CONST_MAX_CHANCE <- CONST_MAX_CHANCE * 0.7 # ~970 Total omicron infection
# CONST_MAX_CHANCE <- CONST_MAX_CHANCE * 0.680 # ~963 Total omicron infection  
# CONST_MAX_CHANCE <- CONST_MAX_CHANCE * 0.62 # ~ 700 Total infections 
# CONST_MAX_CHANCE <- CONST_MAX_CHANCE * 0.5742709 # TODO: 
# CONST_MAX_CHANCE <- CONST_MAX_CHANCE * 0.59 # ~ 400 Total infections
# CONST_MAX_CHANCE <- CONST_MAX_CHANCE * 0.56 # ~350 Total omicron infection
# CONST_MAX_CHANCE <- CONST_MAX_CHANCE * 0.5 # 15 Total omicron infection
# CONST_MAX_CHANCE <- CONST_MAX_CHANCE * 0.33 # ~53 Total omicron infection 
cat("\n")
cat("CONST_MAX_CHANCE: ", CONST_MAX_CHANCE)

CONST_TIME_TILL_76p_CHANCE <- 1200 # seconds (20 minutes)


# Observational proportional data of vaccination status of population from 5 years and above
# CONST_y0 = 0.116909651
# CONST_y1 = 0.05952125148
# CONST_y2 = 0.5539364618
# CONST_y3 = 0.2175267146

# Calib
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


# input files

# contact matrix (pairlist)
# CONTACT_MATRIX_DIR <- 'schedules_Campbellton_Control_Jan_12_2022' 
# CONTACT_MATRIX_DIR <- 'schedules_Campbellton_NoSchool_Jan_12_2022'
CONTACT_MATRIX_DIR <- 'SIR_calib_contact_matrix'


LIST_OF_CONTACT_MATRIX_FILEPATH  <-
  list.files(CONTACT_MATRIX_DIR, full.names = TRUE, pattern="contact\\_matrix\\-total\\-real\\.csv$", ignore.case = TRUE)


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
CONST_epsilon_inf_wild_01 <- 0.6  # 1 - CONST_epsilon_if_wild_01 * prob of infection without vaccine = Prob infection from wild variant with 1 dose
CONST_epsilon_inf_wild_02 <- 0.95 # 1 - CONST_epsilon_if_wild_02 * prob of infection without vaccine = Prob infection from wild variant with 2 dose

voc_default <- data.frame("Wild",
                          "A",
                          CONST_mu_1,
                          CONST_epsilon_inf_wild_01,
                          CONST_epsilon_inf_wild_02,
                          NA,
                          NA)

colnames(voc_default) <- VOC_COLNAMES
voc_df <- voc_default


# According to early study in UK Oct - Nov 2020
# - Leung, Shum, Leung, Lam, & Wu et al, 2021
# Ref: https://pubmed.ncbi.nlm.nih.gov/33413740/


# For people who are vaccinated efficacy odds parameters (for Alpha variant)
CONST_epsilon_inf_alpha_01 <- 0.6
CONST_epsilon_inf_alpha_02 <- 0.95

voc_01 <- data.frame("Alpha",
                     "B.1.1.7",
                     CONST_mu_1 * CONST_mu_2,
                     CONST_epsilon_inf_alpha_01,
                     CONST_epsilon_inf_alpha_02,
                     NA,
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
    CONST_ita <- 1/5         # inverse of mean length of of asymptomatic/mild symptomatic period
  
    # Calibrating with Robert's help
    CONST_v_m <- 0.9812     # fraction of cases never hospitalized (-> mild)
    CONST_v_c <- 0.0028      # fraction of cases hospitalized in ICU for part of stay
    CONST_v_h <- 0.0160       # fraction of cases hospitalized but never in ICU
  
    # CONST_v_m <- 0.960       # fraction of cases never hospitalized (-> mild)
    # CONST_v_c <- 0.006       # fraction of cases hospitalized in ICU for part of stay
    # CONST_v_h <- 0.034       # fraction of cases hospitalized but never in ICU
  
    # CONST_v_m + CONST_v_c + CONST_v_h must be 1
  
    CONST_f_c <- 1/2        # fraction of ICU hospitalization resulting in death
    CONST_f_h <- 1/10       # fraction of non-ICU hospitalization resulting in death
  
    # Calibrating with Robert's help
    CONST_p <- 0.45         # fraction of non-hospitalized cases that isolate late
    # CONST_p <- 0.27         # fraction of non-hospitalized cases that isolate late
    CONST_q <- 0.0694       # fraction of all cases that isolate early
  
  
    CONST_epsilon_trans_01 <- 0.0     # Assuming no change in transmission
    CONST_epsilon_trans_02 <- 0.0     # Assuming no change in transmission
  
    CONST_epsilon_serious_01 <- 0.5   # Assuming for every 100 cases of hospitalization, 50 cases are with 1 dose of vaccine vs. no vaccination
    CONST_epsilon_serious_02 <- 0.75  # For every 100 cases of hospitalization, 25 cases are with 2 doses of vaccine vs. no vaccination
  
  
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
    E(disease_model, path = c("00_pre_symptomatic_non_isolated", "00_mild_non_isolated") )$weight <- CONST_v_m * (1 - CONST_p) * CONST_KAPPA
    E(disease_model, path = c("00_pre_symptomatic_non_isolated", "00_mild_non_isolated") )$label  <- paste0("(3 → 5) = ", CONST_v_m * (1 - CONST_p) * CONST_KAPPA)
  
    E(disease_model, path = c("00_pre_symptomatic_non_isolated", "00_mild_isolated") )$weight <- CONST_v_m * CONST_p * CONST_KAPPA
    E(disease_model, path = c("00_pre_symptomatic_non_isolated", "00_mild_isolated") )$label <- paste0("(3 → 6) = ", CONST_v_m * CONST_p * CONST_KAPPA)
  
    E(disease_model, path = c("00_pre_symptomatic_non_isolated", "00_hospitalized") )$weight <- CONST_v_h * CONST_KAPPA
    E(disease_model, path = c("00_pre_symptomatic_non_isolated", "00_hospitalized") )$label <- paste0("(3 → 7) = ", CONST_v_h  * CONST_KAPPA)
  
    E(disease_model, path = c("00_pre_symptomatic_non_isolated", "00_hospitalized_ICU") )$weight <- CONST_v_c * CONST_KAPPA
    E(disease_model, path = c("00_pre_symptomatic_non_isolated", "00_hospitalized_ICU") )$label <- paste0("(3 → 8) = ", CONST_v_c  * CONST_KAPPA)
  
  
    E(disease_model, path = c("00_pre_symptomatic_isolated", "00_mild_isolated") )$weight <- CONST_v_m * CONST_KAPPA
    E(disease_model, path = c("00_pre_symptomatic_isolated", "00_mild_isolated") )$label <- paste0("(4 → 6) = ", CONST_v_m * CONST_KAPPA)
  
    E(disease_model, path = c("00_pre_symptomatic_isolated", "00_hospitalized") )$weight <- CONST_v_h * CONST_KAPPA
    E(disease_model, path = c("00_pre_symptomatic_isolated", "00_hospitalized") )$label <- paste0("(4 → 7) = ", CONST_v_h  * CONST_KAPPA)
  
    E(disease_model, path = c("00_pre_symptomatic_isolated", "00_hospitalized_ICU") )$weight <- CONST_v_c * CONST_KAPPA
    E(disease_model, path = c("00_pre_symptomatic_isolated", "00_hospitalized_ICU") )$label <- paste0("(4 → 8) = ", CONST_v_c  * CONST_KAPPA)
  
  
  
    # From mild
    E(disease_model, path = c("00_mild_isolated", "00_recovered") )$weight <- CONST_ita
    E(disease_model, path = c("00_mild_isolated", "00_recovered") )$label <- paste0("(6 → r) = ", CONST_ita)
  
    E(disease_model, path = c("00_mild_non_isolated", "00_recovered") )$weight <- CONST_ita
    E(disease_model, path = c("00_mild_non_isolated", "00_recovered") )$label <- paste0("(5 → r) = ", CONST_ita)
  
  
  
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
    E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_mild_non_isolated") )$weight <- ( CONST_v_m + CONST_epsilon_serious_01 * (1 - CONST_v_m)  )* (1 - CONST_p) * CONST_KAPPA
    E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_mild_non_isolated") )$label  <- paste0("(3 → 5) = ", ( CONST_v_m + CONST_epsilon_serious_01 * (1 - CONST_v_m)  )* (1 - CONST_p) * CONST_KAPPA)
  
    E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_mild_isolated") )$weight <- ( CONST_v_m + CONST_epsilon_serious_01 * (1 - CONST_v_m)  ) * CONST_p * CONST_KAPPA
    E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_mild_isolated") )$label <- paste0("(3 → 6) = ", ( CONST_v_m + CONST_epsilon_serious_01 * (1 - CONST_v_m)  ) * CONST_p * CONST_KAPPA)
  
    E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_hospitalized") )$weight <- CONST_v_h * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
    E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_hospitalized") )$label <- paste0("(3 → 7) = ", CONST_v_h * (1 - CONST_epsilon_serious_01)  * CONST_KAPPA)
  
    E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_hospitalized_ICU") )$weight <- CONST_v_c * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
    E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_hospitalized_ICU") )$label <- paste0("(3 → 8) = ", CONST_v_c  * (1 - CONST_epsilon_serious_01) * CONST_KAPPA)
  
  
  
    E(disease_model, path = c("01_pre_symptomatic_isolated", "01_mild_isolated") )$weight <- ( CONST_v_m + CONST_epsilon_serious_01 * (1 - CONST_v_m)  ) * CONST_KAPPA
    E(disease_model, path = c("01_pre_symptomatic_isolated", "01_mild_isolated") )$label <- paste0("(4 → 6) = ", ( CONST_v_m + CONST_epsilon_serious_01 * (1 - CONST_v_m)  ) * CONST_KAPPA)
  
    E(disease_model, path = c("01_pre_symptomatic_isolated", "01_hospitalized") )$weight <- CONST_v_h * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
    E(disease_model, path = c("01_pre_symptomatic_isolated", "01_hospitalized") )$label <- paste0("(4 → 7) = ", CONST_v_h  * (1 - CONST_epsilon_serious_01) * CONST_KAPPA)
  
    E(disease_model, path = c("01_pre_symptomatic_isolated", "01_hospitalized_ICU") )$weight <- CONST_v_c * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
    E(disease_model, path = c("01_pre_symptomatic_isolated", "01_hospitalized_ICU") )$label <- paste0("(4 → 8) = ", CONST_v_c * (1 - CONST_epsilon_serious_01) * CONST_KAPPA)
  
  
  
  
    # From mild
    E(disease_model, path = c("01_mild_isolated", "01_recovered") )$weight <- CONST_ita
    E(disease_model, path = c("01_mild_isolated", "01_recovered") )$label <- paste0("(6 → r) = ", CONST_ita)
  
    E(disease_model, path = c("01_mild_non_isolated", "01_recovered") )$weight <- CONST_ita
    E(disease_model, path = c("01_mild_non_isolated", "01_recovered") )$label <- paste0("(5 → r) = ", CONST_ita)
  
  
  
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
    E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_mild_non_isolated") )$weight <- ( CONST_v_m + CONST_epsilon_serious_02 * (1 - CONST_v_m)  )* (1 - CONST_p) * CONST_KAPPA
    E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_mild_non_isolated") )$label  <- paste0("(3 → 5) = ", ( CONST_v_m + CONST_epsilon_serious_02 * (1 - CONST_v_m)  )* (1 - CONST_p) * CONST_KAPPA)
  
    E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_mild_isolated") )$weight <- ( CONST_v_m + CONST_epsilon_serious_02 * (1 - CONST_v_m)  ) * CONST_p * CONST_KAPPA
    E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_mild_isolated") )$label <- paste0("(3 → 6) = ", ( CONST_v_m + CONST_epsilon_serious_02 * (1 - CONST_v_m)  ) * CONST_p * CONST_KAPPA)
  
    E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_hospitalized") )$weight <- CONST_v_h * (1 - CONST_epsilon_serious_02) * CONST_KAPPA
    E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_hospitalized") )$label <- paste0("(3 → 7) = ", CONST_v_h * (1 - CONST_epsilon_serious_02)  * CONST_KAPPA)
  
    E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_hospitalized_ICU") )$weight <- CONST_v_c * (1 - CONST_epsilon_serious_02) * CONST_KAPPA
    E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_hospitalized_ICU") )$label <- paste0("(3 → 8) = ", CONST_v_c  * (1 - CONST_epsilon_serious_02) * CONST_KAPPA)
  
  
  
    E(disease_model, path = c("02_pre_symptomatic_isolated", "02_mild_isolated") )$weight <- ( CONST_v_m + CONST_epsilon_serious_02 * (1 - CONST_v_m)  ) * CONST_KAPPA
    E(disease_model, path = c("02_pre_symptomatic_isolated", "02_mild_isolated") )$label <- paste0("(4 → 6) = ", ( CONST_v_m + CONST_epsilon_serious_02 * (1 - CONST_v_m)  ) * CONST_KAPPA)
  
    E(disease_model, path = c("02_pre_symptomatic_isolated", "02_hospitalized") )$weight <- CONST_v_h * (1 - CONST_epsilon_serious_02) * CONST_KAPPA
    E(disease_model, path = c("02_pre_symptomatic_isolated", "02_hospitalized") )$label <- paste0("(4 → 7) = ", CONST_v_h  * (1 - CONST_epsilon_serious_02) * CONST_KAPPA)
  
    E(disease_model, path = c("02_pre_symptomatic_isolated", "02_hospitalized_ICU") )$weight <- CONST_v_c * (1 - CONST_epsilon_serious_02) * CONST_KAPPA
    E(disease_model, path = c("02_pre_symptomatic_isolated", "02_hospitalized_ICU") )$label <- paste0("(4 → 8) = ", CONST_v_c * (1 - CONST_epsilon_serious_02) * CONST_KAPPA)
  
  
  
  
    # From mild
    E(disease_model, path = c("02_mild_isolated", "02_recovered") )$weight <- CONST_ita
    E(disease_model, path = c("02_mild_isolated", "02_recovered") )$label <- paste0("(6 → r) = ", CONST_ita)
  
    E(disease_model, path = c("02_mild_non_isolated", "02_recovered") )$weight <- CONST_ita
    E(disease_model, path = c("02_mild_non_isolated", "02_recovered") )$label <- paste0("(5 → r) = ", CONST_ita)
  
  
  
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
  
  
  
  
  
  
  # Attach susceptible with latent_infections (Latent) type states (transition implemented via special weight value) @"### INELEGANT SOLUTION ###"
  # disease_model <- disease_model + edge("susceptible", "00_latent_infections_isolated",
  #                                       weight = "CALCULATE_FROM_CONTACT_MATRIX",
  #                                       label = paste0("if infected, then q = ", CONST_q))
  #
  # disease_model <- disease_model + edge("susceptible", "00_latent_infections_not_isolated",
  #                                        weight = "CALCULATE_FROM_CONTACT_MATRIX",
  #                                        label = paste0("if infected, then (1 - q) = ", (1 - CONST_q)))
  
  
  
  # disease_model <- disease_model + edge("received_dose1", "01_latent_infections_isolated",
  #                                       weight = "CALCULATE_FROM_CONTACT_MATRIX",
  #                                       label = paste0("if infected, then q = ", CONST_q))
  #
  # disease_model <- disease_model + edge("received_dose1", "01_latent_infections_not_isolated",
  #                                       weight = "CALCULATE_FROM_CONTACT_MATRIX",
  #                                       label = paste0("if infected, then (1 - q) = ", (1 - CONST_q)))
  #
  #
  # disease_model <- disease_model + edge("received_dose2", "02_latent_infections_isolated",
  #                                       weight = "CALCULATE_FROM_CONTACT_MATRIX",
  #                                       label = paste0("if infected, then q = ", CONST_q))
  #
  # disease_model <- disease_model + edge("received_dose2", "02_latent_infections_not_isolated",
  #                                       weight = "CALCULATE_FROM_CONTACT_MATRIX",
  #                                       label = paste0("if infected, then (1 - q) = ", (1 - CONST_q)))
  
  
  {
    
    # Omicron specific transitions
    
    CONST_CHI_omicron <- 2/3         # inverse of mean length of infected but not infectious (latent) period
    CONST_KAPPA_omicron <- 2/3       # inverse of mean length of pre-symptomatic period
    CONST_rho_omicron <- 1/4        # inverse of mean length of non-ICU based hospitalization
    CONST_sigma_omicron <- 0.1234567901      # inverse of mean length of ICU based hospitalization
    CONST_ita_omicron <- 1/5         # inverse of mean length of of asymptomatic/mild symptomatic period
    
    CONST_v_m_omicron <- 0.98906     # fraction of cases never hospitalized (-> mild)
    CONST_v_c_omicron <- 0.001641      # fraction of cases hospitalized in ICU for part of stay
    CONST_v_h_omicron <- 0.009299       # fraction of cases hospitalized but never in ICU
    
    # CONST_v_m + CONST_v_c + CONST_v_h must be 1
    
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
    E(disease_model, path = c("00_pre_symptomatic_non_isolated_omicron", "00_mild_non_isolated_omicron") )$weight <- CONST_v_m_omicron * (1 - CONST_p_omicron) * CONST_KAPPA_omicron
    E(disease_model, path = c("00_pre_symptomatic_non_isolated_omicron", "00_mild_non_isolated_omicron") )$label  <- paste0("(3 → 5) = ", CONST_v_m_omicron * (1 - CONST_p_omicron) * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("00_pre_symptomatic_non_isolated_omicron", "00_mild_isolated_omicron") )$weight <- CONST_v_m_omicron * CONST_p_omicron * CONST_KAPPA_omicron
    E(disease_model, path = c("00_pre_symptomatic_non_isolated_omicron", "00_mild_isolated_omicron") )$label <- paste0("(3 → 6) = ", CONST_v_m_omicron * CONST_p_omicron * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("00_pre_symptomatic_non_isolated_omicron", "00_hospitalized_omicron") )$weight <- CONST_v_h_omicron * CONST_KAPPA_omicron
    E(disease_model, path = c("00_pre_symptomatic_non_isolated_omicron", "00_hospitalized_omicron") )$label <- paste0("(3 → 7) = ", CONST_v_h_omicron  * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("00_pre_symptomatic_non_isolated_omicron", "00_hospitalized_ICU_omicron") )$weight <- CONST_v_c_omicron * CONST_KAPPA_omicron
    E(disease_model, path = c("00_pre_symptomatic_non_isolated_omicron", "00_hospitalized_ICU_omicron") )$label <- paste0("(3 → 8) = ", CONST_v_c_omicron  * CONST_KAPPA_omicron)
  
  
    E(disease_model, path = c("00_pre_symptomatic_isolated_omicron", "00_mild_isolated_omicron") )$weight <- CONST_v_m_omicron * CONST_KAPPA_omicron
    E(disease_model, path = c("00_pre_symptomatic_isolated_omicron", "00_mild_isolated_omicron") )$label <- paste0("(4 → 6) = ", CONST_v_m * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("00_pre_symptomatic_isolated_omicron", "00_hospitalized_omicron") )$weight <- CONST_v_h_omicron * CONST_KAPPA_omicron
    E(disease_model, path = c("00_pre_symptomatic_isolated_omicron", "00_hospitalized_omicron") )$label <- paste0("(4 → 7) = ", CONST_v_h_omicron  * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("00_pre_symptomatic_isolated_omicron", "00_hospitalized_ICU_omicron") )$weight <- CONST_v_c * CONST_KAPPA_omicron
    E(disease_model, path = c("00_pre_symptomatic_isolated_omicron", "00_hospitalized_ICU_omicron") )$label <- paste0("(4 → 8) = ", CONST_v_c_omicron  * CONST_KAPPA_omicron)
  
  
  
    # # From mild
    E(disease_model, path = c("00_mild_isolated_omicron", "00_recovered_omicron") )$weight <- CONST_ita_omicron
    E(disease_model, path = c("00_mild_isolated_omicron", "00_recovered_omicron") )$label <- paste0("(6 → r) = ", CONST_ita_omicron)
  
    E(disease_model, path = c("00_mild_non_isolated_omicron", "00_recovered_omicron") )$weight <- CONST_ita_omicron
    E(disease_model, path = c("00_mild_non_isolated_omicron", "00_recovered_omicron") )$label <- paste0("(5 → r) = ", CONST_ita_omicron)
  
  
  
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
    E(disease_model, path = c("01_pre_symptomatic_non_isolated_omicron", "01_mild_non_isolated_omicron") )$weight <- ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_01 * (1 - CONST_v_m_omicron)  )* (1 - CONST_p_omicron) * CONST_KAPPA_omicron
    E(disease_model, path = c("01_pre_symptomatic_non_isolated_omicron", "01_mild_non_isolated_omicron") )$label  <- paste0("(3 → 5) = ", ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_01 * (1 - CONST_v_m_omicron)  )* (1 - CONST_p_omicron) * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("01_pre_symptomatic_non_isolated_omicron", "01_mild_isolated_omicron") )$weight <- ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_01 * (1 - CONST_v_m_omicron)  ) * CONST_p_omicron * CONST_KAPPA_omicron
    E(disease_model, path = c("01_pre_symptomatic_non_isolated_omicron", "01_mild_isolated_omicron") )$label <- paste0("(3 → 6) = ", ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_01 * (1 - CONST_v_m_omicron)  ) * CONST_p_omicron * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("01_pre_symptomatic_non_isolated_omicron", "01_hospitalized_omicron") )$weight <- CONST_v_h_omicron * (1 - CONST_epsilon_serious_omicron_01) * CONST_KAPPA_omicron
    E(disease_model, path = c("01_pre_symptomatic_non_isolated_omicron", "01_hospitalized_omicron") )$label <- paste0("(3 → 7) = ", CONST_v_h_omicron * (1 - CONST_epsilon_serious_omicron_01)  * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("01_pre_symptomatic_non_isolated_omicron", "01_hospitalized_ICU_omicron") )$weight <- CONST_v_c * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
    E(disease_model, path = c("01_pre_symptomatic_non_isolated_omicron", "01_hospitalized_ICU_omicron") )$label <- paste0("(3 → 8) = ", CONST_v_c_omicron  * (1 - CONST_epsilon_serious_omicron_01) * CONST_KAPPA_omicron)
  
  
  
    E(disease_model, path = c("01_pre_symptomatic_isolated_omicron", "01_mild_isolated_omicron") )$weight <- ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_01 * (1 - CONST_v_m_omicron)  ) * CONST_KAPPA_omicron
    E(disease_model, path = c("01_pre_symptomatic_isolated_omicron", "01_mild_isolated_omicron") )$label <- paste0("(4 → 6) = ", ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_01 * (1 - CONST_v_m_omicron)  ) * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("01_pre_symptomatic_isolated_omicron", "01_hospitalized_omicron") )$weight <- CONST_v_h_omicron * (1 - CONST_epsilon_serious_omicron_01) * CONST_KAPPA_omicron
    E(disease_model, path = c("01_pre_symptomatic_isolated_omicron", "01_hospitalized_omicron") )$label <- paste0("(4 → 7) = ", CONST_v_h_omicron  * (1 - CONST_epsilon_serious_omicron_01) * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("01_pre_symptomatic_isolated_omicron", "01_hospitalized_ICU_omicron") )$weight <- CONST_v_c_omicron * (1 - CONST_epsilon_serious_omicron_01) * CONST_KAPPA_omicron
    E(disease_model, path = c("01_pre_symptomatic_isolated_omicron", "01_hospitalized_ICU_omicron") )$label <- paste0("(4 → 8) = ", CONST_v_c_omicron * (1 - CONST_epsilon_serious_omicron_01) * CONST_KAPPA_omicron)
  
  
  
  
    # From mild
    E(disease_model, path = c("01_mild_isolated_omicron", "01_recovered_omicron") )$weight <- CONST_ita_omicron
    E(disease_model, path = c("01_mild_isolated_omicron", "01_recovered_omicron") )$label <- paste0("(6 → r) = ", CONST_ita_omicron)
  
    E(disease_model, path = c("01_mild_non_isolated_omicron", "01_recovered_omicron") )$weight <- CONST_ita_omicron
    E(disease_model, path = c("01_mild_non_isolated_omicron", "01_recovered_omicron") )$label <- paste0("(5 → r) = ", CONST_ita_omicron)
  
  
  
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
    E(disease_model, path = c("02_pre_symptomatic_non_isolated_omicron", "02_mild_non_isolated_omicron") )$weight <- ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_02 * (1 - CONST_v_m_omicron)  )* (1 - CONST_p_omicron) * CONST_KAPPA_omicron
    E(disease_model, path = c("02_pre_symptomatic_non_isolated_omicron", "02_mild_non_isolated_omicron") )$label  <- paste0("(3 → 5) = ", ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_02 * (1 - CONST_v_m_omicron)  )* (1 - CONST_p_omicron) * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("02_pre_symptomatic_non_isolated_omicron", "02_mild_isolated_omicron") )$weight <- ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_02 * (1 - CONST_v_m_omicron)  ) * CONST_p_omicron * CONST_KAPPA_omicron
    E(disease_model, path = c("02_pre_symptomatic_non_isolated_omicron", "02_mild_isolated_omicron") )$label <- paste0("(3 → 6) = ", ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_02 * (1 - CONST_v_m_omicron)  ) * CONST_p_omicron * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("02_pre_symptomatic_non_isolated_omicron", "02_hospitalized_omicron") )$weight <- CONST_v_h_omicron * (1 - CONST_epsilon_serious_omicron_02) * CONST_KAPPA_omicron
    E(disease_model, path = c("02_pre_symptomatic_non_isolated_omicron", "02_hospitalized_omicron") )$label <- paste0("(3 → 7) = ", CONST_v_h_omicron * (1 - CONST_epsilon_serious_omicron_02)  * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("02_pre_symptomatic_non_isolated_omicron", "02_hospitalized_ICU_omicron") )$weight <- CONST_v_c * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
    E(disease_model, path = c("02_pre_symptomatic_non_isolated_omicron", "02_hospitalized_ICU_omicron") )$label <- paste0("(3 → 8) = ", CONST_v_c_omicron  * (1 - CONST_epsilon_serious_omicron_02) * CONST_KAPPA_omicron)
  
  
  
    E(disease_model, path = c("02_pre_symptomatic_isolated_omicron", "02_mild_isolated_omicron") )$weight <- ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_02 * (1 - CONST_v_m_omicron)  ) * CONST_KAPPA_omicron
    E(disease_model, path = c("02_pre_symptomatic_isolated_omicron", "02_mild_isolated_omicron") )$label <- paste0("(4 → 6) = ", ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_02 * (1 - CONST_v_m_omicron)  ) * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("02_pre_symptomatic_isolated_omicron", "02_hospitalized_omicron") )$weight <- CONST_v_h_omicron * (1 - CONST_epsilon_serious_omicron_02) * CONST_KAPPA_omicron
    E(disease_model, path = c("02_pre_symptomatic_isolated_omicron", "02_hospitalized_omicron") )$label <- paste0("(4 → 7) = ", CONST_v_h_omicron  * (1 - CONST_epsilon_serious_omicron_02) * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("02_pre_symptomatic_isolated_omicron", "02_hospitalized_ICU_omicron") )$weight <- CONST_v_c_omicron * (1 - CONST_epsilon_serious_omicron_02) * CONST_KAPPA_omicron
    E(disease_model, path = c("02_pre_symptomatic_isolated_omicron", "02_hospitalized_ICU_omicron") )$label <- paste0("(4 → 8) = ", CONST_v_c_omicron * (1 - CONST_epsilon_serious_omicron_02) * CONST_KAPPA_omicron)
  
  
  
  
    # From mild
    E(disease_model, path = c("02_mild_isolated_omicron", "02_recovered_omicron") )$weight <- CONST_ita_omicron
    E(disease_model, path = c("02_mild_isolated_omicron", "02_recovered_omicron") )$label <- paste0("(6 → r) = ", CONST_ita_omicron)
  
    E(disease_model, path = c("02_mild_non_isolated_omicron", "02_recovered_omicron") )$weight <- CONST_ita_omicron
    E(disease_model, path = c("02_mild_non_isolated_omicron", "02_recovered_omicron") )$label <- paste0("(5 → r) = ", CONST_ita_omicron)
  
  
  
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
    E(disease_model, path = c("03_pre_symptomatic_non_isolated_omicron", "03_mild_non_isolated_omicron") )$weight <- ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_03 * (1 - CONST_v_m_omicron)  )* (1 - CONST_p_omicron) * CONST_KAPPA_omicron
    E(disease_model, path = c("03_pre_symptomatic_non_isolated_omicron", "03_mild_non_isolated_omicron") )$label  <- paste0("(3 → 5) = ", ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_03 * (1 - CONST_v_m_omicron)  )* (1 - CONST_p_omicron) * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("03_pre_symptomatic_non_isolated_omicron", "03_mild_isolated_omicron") )$weight <- ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_03 * (1 - CONST_v_m_omicron)  ) * CONST_p_omicron * CONST_KAPPA_omicron
    E(disease_model, path = c("03_pre_symptomatic_non_isolated_omicron", "03_mild_isolated_omicron") )$label <- paste0("(3 → 6) = ", ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_03 * (1 - CONST_v_m_omicron)  ) * CONST_p_omicron * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("03_pre_symptomatic_non_isolated_omicron", "03_hospitalized_omicron") )$weight <- CONST_v_h_omicron * (1 - CONST_epsilon_serious_omicron_03) * CONST_KAPPA_omicron
    E(disease_model, path = c("03_pre_symptomatic_non_isolated_omicron", "03_hospitalized_omicron") )$label <- paste0("(3 → 7) = ", CONST_v_h_omicron * (1 - CONST_epsilon_serious_omicron_03)  * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("03_pre_symptomatic_non_isolated_omicron", "03_hospitalized_ICU_omicron") )$weight <- CONST_v_c * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
    E(disease_model, path = c("03_pre_symptomatic_non_isolated_omicron", "03_hospitalized_ICU_omicron") )$label <- paste0("(3 → 8) = ", CONST_v_c_omicron  * (1 - CONST_epsilon_serious_omicron_03) * CONST_KAPPA_omicron)
  
  
  
    E(disease_model, path = c("03_pre_symptomatic_isolated_omicron", "03_mild_isolated_omicron") )$weight <- ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_03 * (1 - CONST_v_m_omicron)  ) * CONST_KAPPA_omicron
    E(disease_model, path = c("03_pre_symptomatic_isolated_omicron", "03_mild_isolated_omicron") )$label <- paste0("(4 → 6) = ", ( CONST_v_m_omicron + CONST_epsilon_serious_omicron_03 * (1 - CONST_v_m_omicron)  ) * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("03_pre_symptomatic_isolated_omicron", "03_hospitalized_omicron") )$weight <- CONST_v_h_omicron * (1 - CONST_epsilon_serious_omicron_03) * CONST_KAPPA_omicron
    E(disease_model, path = c("03_pre_symptomatic_isolated_omicron", "03_hospitalized_omicron") )$label <- paste0("(4 → 7) = ", CONST_v_h_omicron  * (1 - CONST_epsilon_serious_omicron_03) * CONST_KAPPA_omicron)
  
    E(disease_model, path = c("03_pre_symptomatic_isolated_omicron", "03_hospitalized_ICU_omicron") )$weight <- CONST_v_c_omicron * (1 - CONST_epsilon_serious_omicron_03) * CONST_KAPPA_omicron
    E(disease_model, path = c("03_pre_symptomatic_isolated_omicron", "03_hospitalized_ICU_omicron") )$label <- paste0("(4 → 8) = ", CONST_v_c_omicron * (1 - CONST_epsilon_serious_omicron_03) * CONST_KAPPA_omicron)
  
  
  
  
    # From mild
    E(disease_model, path = c("03_mild_isolated_omicron", "03_recovered_omicron") )$weight <- CONST_ita_omicron
    E(disease_model, path = c("03_mild_isolated_omicron", "03_recovered_omicron") )$label <- paste0("(6 → r) = ", CONST_ita_omicron)
  
    E(disease_model, path = c("03_mild_non_isolated_omicron", "03_recovered_omicron") )$weight <- CONST_ita_omicron
    E(disease_model, path = c("03_mild_non_isolated_omicron", "03_recovered_omicron") )$label <- paste0("(5 → r) = ", CONST_ita_omicron)
  
  
  
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
                                  colnames(transition_matrix) == "02_mild_non_isolated")
  
  # Separated to setup proportions of Omicron and Non-omicron infection events
  STATELIST_INFECTIOUS_OMICRON <- which(colnames(transition_matrix) == "00_pre_symptomatic_non_isolated_omicron" |
                                          colnames(transition_matrix) == "00_mild_non_isolated_omicron" |
                                          colnames(transition_matrix) == "01_pre_symptomatic_non_isolated_omicron" |
                                          colnames(transition_matrix) == "01_mild_non_isolated_omicron" |
                                          colnames(transition_matrix) == "02_pre_symptomatic_non_isolated_omicron" |
                                          colnames(transition_matrix) == "02_mild_non_isolated_omicron" |
                                          colnames(transition_matrix) == "03_pre_symptomatic_non_isolated_omicron" |
                                          colnames(transition_matrix) == "03_mild_non_isolated_omicron")
  
  STATELIST_NEW_CASES <- which(colnames(transition_matrix) == "00_pre_symptomatic_isolated" |
                                 colnames(transition_matrix) == "00_mild_isolated" |
                                 colnames(transition_matrix) == "01_pre_symptomatic_isolated" |
                                 colnames(transition_matrix) == "01_mild_isolated" |
                                 colnames(transition_matrix) == "02_pre_symptomatic_isolated" |
                                 colnames(transition_matrix) == "02_mild_isolated" |
                                 
                                 colnames(transition_matrix) == "00_pre_symptomatic_isolated_omicron" |
                                 colnames(transition_matrix) == "00_mild_isolated_omicron" |
                                 colnames(transition_matrix) == "01_pre_symptomatic_isolated_omicron" |
                                 colnames(transition_matrix) == "01_mild_isolated_omicron" |
                                 colnames(transition_matrix) == "02_pre_symptomatic_isolated_omicron" |
                                 colnames(transition_matrix) == "02_mild_isolated_omicron" |
                                 colnames(transition_matrix) == "03_pre_symptomatic_isolated_omicron" |
                                 colnames(transition_matrix) == "03_mild_isolated_omicron" )
  
  
  
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
  # CONSTANT_REMOVAL_RATE <- 0.1 # gamma
  CONSTANT_REMOVAL_RATE <- 1/9 # gamma, maybe this offsets for evolving first and checking for infection next?
  
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
  
  
}




# If-else ladder of disease model construction ends
# *************************************************************

plot(disease_model, layout=layout_as_tree)

# Interactive plot
# tkplot(disease_model, layout=layout_as_tree,
#        canvas.height = 800, canvas.width = 1600,
#        vertex.color="#77c98d", edge.arrow.size=0.75)

# Debug output graph
E(disease_model)
edge_attr(disease_model)

# Memory Expensive approach to reduce total I/O
#
# Reading all the matrices in a list
#

list_of_contact_matrix <- list()
matrix_index <- 1

library(Matrix)

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

  matrix_index <- matrix_index + 1
}

COUNT_CONTACT_MATRIX <- length(list_of_contact_matrix)


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
infection_hist_mat <- matrix (ncol = 6, nrow = TOTAL_SIMULATED_PERSONS)
infection_hist_mat_colnames <- c("variant", "infected on", "infected by", "dose 1 on", "dose 2 on", "dose 3 on")
colnames(infection_hist_mat) <- infection_hist_mat_colnames



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
cat("Sampled Y3: ", sampled_y2, "\n")





DAILY_NEW_CASES <- array( rep(0, TOTAL_SIMULATION_DAYS), dim = TOTAL_SIMULATION_DAYS )
DAILY_ACTIVE_CASES <- array( rep(0, TOTAL_SIMULATION_DAYS), dim = TOTAL_SIMULATION_DAYS )

# Set first day new cases to COUNT_FIRST_DAY_DISEASE_IMPORT for bookkeeping
DAILY_NEW_CASES[1] <- COUNT_FIRST_DAY_DISEASE_IMPORT

# Randomly distribute the infection import
# first_disease_import <- sample(seq_len(length(STATE[ , 1])), COUNT_FIRST_DAY_DISEASE_IMPORT) # Worked when there were 0 vaccinated on the 1st day

# first_disease_import <- sample(init_list_of_person_with_0_dose, COUNT_FIRST_DAY_DISEASE_IMPORT)
# first_disease_import <- sample(init_list_of_person_with_1_dose, COUNT_FIRST_DAY_DISEASE_IMPORT)
# first_disease_import <- sample(init_list_of_person_with_2_dose, COUNT_FIRST_DAY_DISEASE_IMPORT)
# first_disease_import <- sample(init_list_of_person_with_3_dose, COUNT_FIRST_DAY_DISEASE_IMPORT)

# Freezing disease import to agent 1, 2, 3 and 4 as decided with Robert
first_disease_import <- c(1, 2, 3, 4)


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

  present_state = STATE[person_id , 1]
  infected_state = "UNKNOWN"
  
  if (CONST_CHOSEN_DISEASE_MODEL == "Complex") {
    # If 0 vaccination dose
    if(present_state == which(colnames(transition_matrix) == "susceptible")){
  
      #Check if Omicron
      if(toString(infecting_variant) == "B.1.1.529"){
        infected_state <- sample( c( which(colnames(transition_matrix) == "00_pre_symptomatic_non_isolated_omicron"),
                                     which(colnames(transition_matrix) == "00_mild_non_isolated_omicron") ), 1)
      }
      # For all other Variants of Concern
      else {
        infected_state <- sample( c( which(colnames(transition_matrix) == "00_pre_symptomatic_non_isolated"),
                                     which(colnames(transition_matrix) == "00_mild_non_isolated") ), 1)
      }
    }
  
    # If 1 vaccination dose
    else if(present_state == which(colnames(transition_matrix) == "received_dose1")){
  
      #Check if Omicron
      if(toString(infecting_variant) == "B.1.1.529"){
        infected_state <- sample( c( which(colnames(transition_matrix) == "01_pre_symptomatic_non_isolated_omicron"),
                                     which(colnames(transition_matrix) == "01_mild_non_isolated_omicron") ), 1)
      }
      # For all other Variants of Concern
      else {
        infected_state <- sample( c( which(colnames(transition_matrix) == "01_pre_symptomatic_non_isolated"),
                                     which(colnames(transition_matrix) == "01_mild_non_isolated") ), 1)
      }
    }
  
    # If 2 vaccination dose
    else if(present_state == which(colnames(transition_matrix) == "received_dose2")){
  
      #Check if Omicron
      if(toString(infecting_variant) == "B.1.1.529"){
        infected_state <- sample( c( which(colnames(transition_matrix) == "02_pre_symptomatic_non_isolated_omicron"),
                                     which(colnames(transition_matrix) == "02_mild_non_isolated_omicron") ), 1)
      }
      # For all other Variants of Concern
      else {
        infected_state <- sample( c( which(colnames(transition_matrix) == "02_pre_symptomatic_non_isolated"),
                                     which(colnames(transition_matrix) == "02_mild_non_isolated") ), 1)
      }
    }
  
    # If 3 vaccination dose
    else if(present_state == which(colnames(transition_matrix) == "received_dose3")){
  
      #Check if Omicron
      if(toString(infecting_variant) == "B.1.1.529"){
        infected_state <- sample( c( which(colnames(transition_matrix) == "03_pre_symptomatic_non_isolated_omicron"),
                                     which(colnames(transition_matrix) == "03_mild_non_isolated_omicron") ), 1)
      }
      # For all other Variants of Concern, data not available yet
      # else {
      #   infected_state <- sample( c( which(colnames(transition_matrix) == "02_pre_symptomatic_non_isolated"),
      #                                which(colnames(transition_matrix) == "02_mild_non_isolated") ), 1)
      # }
    } 
  } else if (CONST_CHOSEN_DISEASE_MODEL == "SIR"){
    
    if(present_state == which(colnames(transition_matrix) == "susceptible")){
      infected_state <- which(colnames(transition_matrix) == "infected")
      
    }
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
  }
}

# cat("\nPre-simulation breakpoint")


STATELIST_ALL_INFECTIOUS <- union(STATELIST_INFECTIOUS, STATELIST_INFECTIOUS_OMICRON)

# Helper function to check if a particular state is infectious or not
isInfectiousState <- function(state_id){
  if(sum(sapply(STATELIST_ALL_INFECTIOUS, FUN = function(x) x == state_id)) == 0) { return (FALSE) }

  else { return (TRUE) }
}

sim_setup_time <- proc.time() - start_time
cat("\n Sim-setup time for preloading no. of. contact-matrices:  ", COUNT_CONTACT_MATRIX, " and observed vaccination rates for: ", TOTAL_DAYS_OF_OBSERVATION, " days", sep = "")
cat("\n", sim_setup_time)



# Re-Start the clock!
sim_time <- proc.time()

# Simulate from day 2 to TOTAL_SIMULATION_DAYS
for(day_index in 2:TOTAL_SIMULATION_DAYS){

  # Debug:
  # cat("\n")
  # cat("  Sim day: ", day_index, sep = "")
  # cat("\n")

  previous_day_infected_person_ids <- which(sapply(STATE[ , day_index - 1], FUN = isInfectiousState) == TRUE)

  COUNT_NEW_INFECTIONS_TODAY <- 0 # flush the counter

  for(person_id in 1:TOTAL_SIMULATED_PERSONS){

    # Get state of this person from previous day
    PREV_STATE_INDEX <- as.numeric(STATE[person_id, day_index - 1])
    PREV_STATE_NAME <- STATE_NAMES[PREV_STATE_INDEX] # Note: At this point STATE_NAME is a matrix, by the end of execution it's converted to df for CSV IO

    # for no transitions
    next_state_id <- PREV_STATE_INDEX
    next_state_name <- PREV_STATE_NAME

    # Debug:
    # cat("    \n...............\n")
    # cat("    Previous state of Person no: ", person_id, ", state id: ", PREV_STATE_INDEX, ", state name: ", PREV_STATE_NAME, "\n", sep = "")


    # Vaccination state changes during simulation time
    # ------------------- 
    # Check if the person may have been vaccinated last simulation day
    # i.e. is in succeptible or received_dose_1 state
    # NO_OF_VACCINATION_DATA_BASED_TRANSITION <- length(which(POSSIBLE_TRANSITIONS$transition_weights == "CALCULATE_FROM_VACCINATION_DATA"))
    # if(PREV_STATE_NAME == "susceptible" || PREV_STATE_NAME == "received_dose1"){
    #
    #   # Get vaccination data and roll dice for possible vaccination last day
    #   vaccination_day_index <- day_index - 1
    #
    #   # Re-use last availble vaccination rates
    #   if(vaccination_day_index > nrow(vaccination_perday_mat)) {
    #     vaccination_day_index <- nrow(vaccination_perday_mat)
    #   }
    #
    #   if(PREV_STATE_NAME == "susceptible" &&
    #      vaccination_perday_mat[vaccination_day_index , "dose1"] > 0){
    #
    #     dose1_prob <- vaccination_perday_mat[vaccination_day_index , "dose1"] / CONST_NB_POP
    #
    #     # Force negative fractions to 0, to counteract artifacts in the data
    #     if(dose1_prob < 0) { dose1_prob = 0 }
    #
    #     if( runif(1) <= dose1_prob){
    #       prev_state_id <- which( colnames(transition_matrix) == "received_dose1" )
    #
    #       # Set state
    #       STATE[person_id, day_index - 1] <- prev_state_id
    #
    #       # Book keeping
    #       infection_hist_mat[person_id, "dose 1 on"] <- day_index - 1
    #     }
    #   }
    #
    #   if(PREV_STATE_NAME == "received_dose1" &&
    #      vaccination_perday_mat[vaccination_day_index , "dose2"] > 0 &&
    #      (day_index - strtoi(infection_hist_mat[[person_id, "dose 1 on"]])) >= CONST_MIN_DAYS_TILL_SECOND_DOSE){
    #
    #     dose2_prob <- vaccination_perday_mat[vaccination_day_index , "dose2"] / CONST_NB_POP
    #
    #     # Force negative fractions to 0, to counteract artifacts in the data
    #     if(dose2_prob < 0) { dose2_prob = 0 }
    #
    #     if( runif(1) <= dose2_prob){
    #       prev_state_id <- which( colnames(transition_matrix) == "received_dose2" )
    #
    #       # Set state
    #       STATE[person_id, day_index - 1] <- prev_state_id
    #
    #       # Book keeping
    #       infection_hist_mat[person_id, "dose 2 on"] <- day_index - 1
    #     }
    #   }
    #
    #
    #   # Reload states
    #   PREV_STATE_INDEX <- STATE[person_id, day_index - 1]
    #   PREV_STATE_NAME <- STATE_NAMES[PREV_STATE_INDEX]
    #
    # } # End of last days vaccination prob



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
      list_of_contacts <- which(contact_matrix[person_id, ] > 0)

      # Check if person 2 was infectious on day_index - 1
      list_of_infectious_contacts <- intersect(list_of_contacts, previous_day_infected_person_ids)

      COUNT_OF_POSSIBLE_INFECTIOUS_CONTACTS <- length(list_of_infectious_contacts)

      next_state_id <- PREV_STATE_INDEX
      next_state_name <- STATE_NAMES[next_state_id]

      if(COUNT_OF_POSSIBLE_INFECTIOUS_CONTACTS > 0) {

        # Debug:
        # cat("       Person:, ", person_id, " may be infected by: ", COUNT_OF_POSSIBLE_INFECTIOUS_CONTACTS, " infected neighbours\n", sep = "")

        # Perform each Coin toss and check for possible infection till first occurs
        # @"Tag: Sequence of probabilities"
        ALREADY_INFECTED <- FALSE
        for(contact_index in list_of_infectious_contacts){

          if( ALREADY_INFECTED ) { break }

          # Get relative infectivity of variant
          relative_infectivity <- as.numeric(active_voc_mat[which(active_voc_mat[ , "variant"] == infection_hist_mat[contact_index, "variant"]), "Relative infectivity"])

          contact_time <- contact_matrix[person_id, contact_index]

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
  
                # [NOT IMPLEMENTED YET] Check if Wild, Alpha, Beta or Delta
                # else if(infection_WHO_label == "Wild" ||
                #         infection_WHO_label == "Alpha" ||
                #         infection_WHO_label == "Beta" ||
                #         infection_WHO_label == "Gamma" ||
                #         infection_WHO_label == "Delta" ){
                #   next_state_id <- sample( c( which(STATE_NAMES == "03_latent_infections_not_isolated"), which(STATE_NAMES == "03_latent_infections_isolated") ),
                #                            1,
                #                            prob = c((1 - CONST_q), CONST_q))
                # }
  
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
            
            }

            ALREADY_INFECTED <- TRUE




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
          }
        }
      }

      STATE[person_id, day_index] <- next_state_id
      next_state_name <- STATE_NAMES[next_state_id]

    }

    # TODO: Not looking out for transitions that may be a mix of both (event based and static)

    # Non-susceptible (or already infected), static parameter based transition
    else{

      # Check if already in a final state
      # if(nrow(POSSIBLE_TRANSITIONS) == 0) {
      if(nrow(POSSIBLE_TRANSITIONS_mat) == 0) {
        next_state_id <- PREV_STATE_INDEX
        next_state_name <- STATE_NAMES[next_state_id]
      }

      # Calculate possible transitions for non-final states
      else{

        # cat("Parametrised transitions for person: ", person_id, "in state: ", PREV_STATE_NAME, "\n")

        # There should not be any non numeric transition weights for this state
        # Coerce into double type

        STAY_IN_STATE_WEIGHT <- min(1.0, abs(1.0 - sum( as.double(POSSIBLE_TRANSITIONS_mat[ , "transition_weights"]) )))

        # Create temp loop-back state

        complete_state_ids <- append(as.numeric(POSSIBLE_TRANSITIONS_mat[ , "state_id"]), PREV_STATE_INDEX)
        complete_state_weights <- append(POSSIBLE_TRANSITIONS_mat[ , "transition_weights"], STAY_IN_STATE_WEIGHT)

        # Choose a state w.r.t weight
        next_state_id <- sample(complete_state_ids, 1, prob = complete_state_weights)
      }

      # Set state
      STATE[person_id, day_index] <- next_state_id
      next_state_name <- STATE_NAMES[next_state_id]

      # Debug:
      # if(PREV_STATE_INDEX != next_state_id){
        # cat("Changed Person id: ", person_id, ":  ", STATE_NAMES[PREV_STATE_INDEX], "to ", STATE_NAMES[next_state_id], "\n")
      # }


    }

    # For each person check if transition occurred
    if(PREV_STATE_INDEX != next_state_id){
      # cat("Changed Person id: ", person_id, ":  ", STATE_NAMES[PREV_STATE_INDEX], "to ", STATE_NAMES[next_state_id], "\n")
      if( next_state_id %in% STATELIST_NEW_CASES) {

        # Check for correct transitions to "NEW CASES" (interpret as person has been tested and confirmed)

        if(CONST_CHOSEN_DISEASE_MODEL == "Complex"){
          # Edge: "xx_latent_infections_isolated", "xx_pre_symptomatic_isolated", # π(2 → 4)
  
          if(
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
          }
  
          # Edge: "xx_pre_symptomatic_non_isolated", "xx_mild_isolated", # π(3 → 6)
          else if(
  
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
        } else if(CONST_CHOSEN_DISEASE_MODEL == "SIR"){
          # Edge: "susceptible", "infected"
          if((PREV_STATE_NAME == "susceptible") && (next_state_name == "infected")){
            COUNT_NEW_INFECTIONS_TODAY <- COUNT_NEW_INFECTIONS_TODAY + 1
          }
        }


      
        
      }
    }

    # cat("    \n...............\n")

  } # End of person iterator




  # Tabulate new infections
  DAILY_NEW_CASES[day_index] <- COUNT_NEW_INFECTIONS_TODAY

  # Check if active cases today were beyond the threshold for school closures
  current_day_df <- data.frame(STATE[ , day_index])
  colnames(current_day_df) <- c("state_id")
  COUNT_ACTIVE_CASES <- nrow(filter(current_day_df, state_id %in% STATELIST_ACTIVE_CASES))
  # Book keeping
  DAILY_ACTIVE_CASES [day_index] <- COUNT_ACTIVE_CASES

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

# setting Time as unique run identifier
UUID <- Sys.time()

# Run config and metadata 
run_config_mat <- matrix (ncol = 16, nrow = 2)
run_config_mat_colnames <- c("CONST_CHOSEN_DISEASE_MODEL", 
                             "CONST_MAX_CHANCE", 
                             "CONST_TIME_TILL_76p_CHANCE", 
                             "TOTAL_SIMULATION_DAYS",
                             "TOTAL_SIMULATED_PERSONS",
                             "sampled_y0",
                             "sampled_y1",
                             "sampled_y2",
                             "sampled_y3",
                             "COUNT_FIRST_DAY_DISEASE_IMPORT",
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

run_config_mat[1, "CONST_MAX_CHANCE"] <- CONST_MAX_CHANCE
run_config_mat[2, "CONST_MAX_CHANCE"] <- "default amplitude for infectivity tanh function"

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

STATE_NAMES <- data.frame(STATE_NAMES)
STATE_NAMES$state_id <- seq.int(nrow(STATE_NAMES))

# write all state_id -> state name mapping as CSV
write.table(STATE_NAMES, sep=",", OUTPUT_STATENAMES_FILEPATH_CSV, row.names = FALSE, col.names = FALSE)



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






# Debug:

# Initial infections
# cat("\n Initial counts: \n")
# cat(table(day_1_variants_list))
# table(day_1_varaint_list)

# Final infections
cat("\n\nPost simulation counts: \n")
cat("No. of Wild infections: ", length(which(infection_hist_mat[, "variant"] == "A")), "\n")
cat("No. of Alpha infections: ", length(which(infection_hist_mat[, "variant"] == "B.1.1.7")), "\n")
cat("No. of Delta infections: ", length(which(infection_hist_mat[, "variant"] == "B.1.617.2")), "\n")
cat("No. of Omicron infections: ", length(which(infection_hist_mat[, "variant"] == "B.1.1.529")), "\n")
cat("-------------------\n")
cat("No. of random samples drawn to check for infection success: ", COUNT_NO_OF_INFECTION_COIN_FLIPS, "\n", sep = "")
cat("No. of positive infections: ", COUNT_NO_OF_INFECTION_EVENTS, "\n", sep = "")

# cat("No. of Possiible infection events: ", random_sample_iterator, "\n")

# Stop the clock
cat("\n Sim time for no. of. days: ", TOTAL_SIMULATION_DAYS, ", with no. of. agents: ", TOTAL_SIMULATED_PERSONS, sep = "")
cat("\n", proc.time() - sim_time)

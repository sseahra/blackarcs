# Start the clock!
ptm <- proc.time()

library(dplyr) # for dataframe utitlities
library(iterators)

# Setup large pool of random samples outside the script
# Note: Introduced about 8 GB of memory and increasd overall runtime 
#
# TODO; Understand root cause, possibilities include lack of default optimisations when sharing a global data-structure 
#
# COUNT_RANDOM_SAMPLE_LIST <- 2^30 # = 1073741824, when divided by (5000 C 2) ~> 85 (should hold pessimistacly for 5000 agents for 86 days)
# random_unifrom_sample_list <- runif(COUNT_RANDOM_SAMPLE_LIST)
# random_sample_iterator <- 1 # Counter to re-initialise when exhausted 

# SIM_SCRIPT_NAME = 'complex_model_v00_025_for_365d_1case'
SIM_SCRIPT_NAME = 'complex_model_vcalib00_010_for_90d_Nov19'

# Calibration mode: 
# TOTAL_SIMULATION_DAYS <- 10

# Debug mode: 
TOTAL_SIMULATION_DAYS <- 90

# TOTAL_SIMULATION_DAYS <- 30
# TOTAL_SIMULATION_DAYS <- 60
# TOTAL_SIMULATION_DAYS <- 365


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
                  "Proportion")

CONST_mu_1 = 1.0 # infectiousness of wild variant treated as default  
CONST_mu_2 = 1.5 # infectiousness of Alpha w.r.t wild variant 
CONST_mu_3 = 1.8 # infectiousness of Delta w.r.t Alpha variant

# For people who are vaccinated efficacy odds parameters (for wild variant)
CONST_epsilon_inf_wild_01 <- 0.6  # 1 - CONST_epsilon_if_wild_01 * prob of infection without vaccine = Prob infection from wild variant with 1 dose 
CONST_epsilon_inf_wild_02 <- 0.95 # 1 - CONST_epsilon_if_wild_02 * prob of infection without vaccine = Prob infection from wild variant with 2 dose      

voc_default <- data.frame("Wild", 
                          "A", 
                          CONST_mu_1, 
                          CONST_epsilon_inf_wild_01,
                          CONST_epsilon_inf_wild_02, 
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
                     NA)

colnames(voc_01) <- VOC_COLNAMES
voc_df <- rbind(voc_df, voc_01)


voc_02 <- data.frame("Beta", 
                     list(c("B.1.351", "B.1.351.1", "B.1.351.2", "B.1.351.3", "B.1.351.4")),
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
                     NA)

colnames(voc_04) <- VOC_COLNAMES
voc_df <- rbind(voc_df, voc_04)

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
  
  # Calibration mode: 
  voc_df <- voc_df %>% mutate_cond(`WHO label` == "Wild" & `variant` == "A", 'Proportion' = 100.0)
  
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

# Calibration mode: 
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
# disease_model <- graph( edges = c("susceptible","received_dose1",  
#                                   "received_dose1","received_dose2"), directed = TRUE)
# 
# # Vaccination/day parameters, look up @Vaccination Rates
# E(disease_model, path = c("susceptible", "received_dose1") )$weight <- 0.0
# E(disease_model, path = c("susceptible", "received_dose1") )$label <- paste0("BASED ON VACCINE DISBURSAL DATA")
# 
# E(disease_model, path = c("received_dose1", "received_dose2") )$weight <- 0.0
# E(disease_model, path = c("received_dose1", "received_dose2") )$label <- paste0("BASED ON VACCINE DISBURSAL DATA")

# Adding weight value to edge labels for visual inspection
# E(disease_model)$label <- paste(E(disease_model)$weight, "/day")

# plot(disease_model)


{
# Infection states for 0 doses: 
  # disease_model <- disease_model+  
  #                  graph( edges = c("00_latent_infections_not_isolated", "00_pre_symptomatic_non_isolated", # π(1 → 3)
  #                                   "00_latent_infections_isolated", "00_pre_symptomatic_isolated", # π(2 → 4)
  #                                   
  #                                   "00_pre_symptomatic_non_isolated", "00_mild_non_isolated", # π(3 → 5)
  #                                   "00_pre_symptomatic_non_isolated", "00_mild_isolated", # π(3 → 6)
  #                                   "00_pre_symptomatic_non_isolated", "00_hospitalized", # π(3 → 7)
  #                                   "00_pre_symptomatic_non_isolated", "00_hospitalized_ICU", # π(3 → 8)
  #                                   
  #                                   "00_pre_symptomatic_isolated", "00_mild_isolated", # π(4 → 6)
  #                                   "00_pre_symptomatic_isolated", "00_hospitalized", # π(4 → 7)
  #                                   "00_pre_symptomatic_isolated", "00_hospitalized_ICU", # π(4 → 8)
  #                                   
  #                                   "00_mild_isolated", "00_recovered", # π(5 → r)
  #                                   "00_mild_non_isolated", "00_recovered", # π(6 → r)
  #                                   "00_hospitalized", "00_recovered", # π(7 → r)
  #                                   "00_hospitalized_ICU", "00_recovered", # π(8 → r)
  #                                   
  #                                   "00_hospitalized", "00_dead", # π(7 → d)
  #                                   "00_hospitalized_ICU", "00_dead"), # π(8 → d)
  #                         directed = TRUE)

# For people who are not vaccinated: Infection transition/day parameters 
# CONSTANT_NOT_VACCINATED <- 0.1 

# For people who are not vaccinated: Infection transition/day parameters 

# Note: change to a matrix format with const_label, received_dose_1, received_dose_2, ... 
  
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

# Adding weight value to edge labels for visual inspection
# E(disease_model)$label <- paste(E(disease_model)$weight, "/day")

}

# Infection parameters for people with 1st dose of vaccination
# {
#   # Infection states for 1 doses:
#   disease_model <- disease_model+
#     graph( edges = c("01_latent_infections_not_isolated", "01_pre_symptomatic_non_isolated", # π(1 → 3)
#                      "01_latent_infections_isolated", "01_pre_symptomatic_isolated", # π(2 → 4)
# 
#                      "01_pre_symptomatic_non_isolated", "01_mild_non_isolated", # π(3 → 5)
#                      "01_pre_symptomatic_non_isolated", "01_mild_isolated", # π(3 → 6)
#                      "01_pre_symptomatic_non_isolated", "01_hospitalized", # π(3 → 7)
#                      "01_pre_symptomatic_non_isolated", "01_hospitalized_ICU", # π(3 → 8)
# 
#                      "01_pre_symptomatic_isolated", "01_mild_isolated", # π(4 → 6)
#                      "01_pre_symptomatic_isolated", "01_hospitalized", # π(4 → 7)
#                      "01_pre_symptomatic_isolated", "01_hospitalized_ICU", # π(4 → 8)
# 
#                      "01_mild_isolated", "01_recovered", # π(5 → r)
#                      "01_mild_non_isolated", "01_recovered", # π(6 → r)
#                      "01_hospitalized", "01_recovered", # π(7 → r)
#                      "01_hospitalized_ICU", "01_recovered", # π(8 → r)
# 
#                      "01_hospitalized", "01_dead", # π(7 → d)
#                      "01_hospitalized_ICU", "01_dead"), # π(8 → d)
#            directed = TRUE)
# 
#   # From Latent
#   E(disease_model, path = c("01_latent_infections_not_isolated", "01_pre_symptomatic_non_isolated") )$weight <- CONST_CHI
#   E(disease_model, path = c("01_latent_infections_not_isolated", "01_pre_symptomatic_non_isolated") )$label <- paste0("(1 → 3) = ", CONST_CHI)
# 
#   E(disease_model, path = c("01_latent_infections_isolated", "01_pre_symptomatic_isolated") )$weight <- CONST_CHI
#   E(disease_model, path = c("01_latent_infections_isolated", "01_pre_symptomatic_isolated") )$label <- paste0("(2 → 4) = ", CONST_CHI)
# 
# 
#   # From Pre-symptomatic
#   E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_mild_non_isolated") )$weight <- ( CONST_v_m + CONST_epsilon_serious_01 * (1 - CONST_v_m)  )* (1 - CONST_p) * CONST_KAPPA
#   E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_mild_non_isolated") )$label  <- paste0("(3 → 5) = ", ( CONST_v_m + CONST_epsilon_serious_01 * (1 - CONST_v_m)  )* (1 - CONST_p) * CONST_KAPPA)
# 
#   E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_mild_isolated") )$weight <- ( CONST_v_m + CONST_epsilon_serious_01 * (1 - CONST_v_m)  ) * CONST_p * CONST_KAPPA
#   E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_mild_isolated") )$label <- paste0("(3 → 6) = ", ( CONST_v_m + CONST_epsilon_serious_01 * (1 - CONST_v_m)  ) * CONST_p * CONST_KAPPA)
# 
#   E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_hospitalized") )$weight <- CONST_v_h * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
#   E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_hospitalized") )$label <- paste0("(3 → 7) = ", CONST_v_h * (1 - CONST_epsilon_serious_01)  * CONST_KAPPA)
# 
#   E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_hospitalized_ICU") )$weight <- CONST_v_c * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
#   E(disease_model, path = c("01_pre_symptomatic_non_isolated", "01_hospitalized_ICU") )$label <- paste0("(3 → 8) = ", CONST_v_c  * (1 - CONST_epsilon_serious_01) * CONST_KAPPA)
# 
# 
# 
#   E(disease_model, path = c("01_pre_symptomatic_isolated", "01_mild_isolated") )$weight <- ( CONST_v_m + CONST_epsilon_serious_01 * (1 - CONST_v_m)  ) * CONST_KAPPA
#   E(disease_model, path = c("01_pre_symptomatic_isolated", "01_mild_isolated") )$label <- paste0("(4 → 6) = ", ( CONST_v_m + CONST_epsilon_serious_01 * (1 - CONST_v_m)  ) * CONST_KAPPA)
# 
#   E(disease_model, path = c("01_pre_symptomatic_isolated", "01_hospitalized") )$weight <- CONST_v_h * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
#   E(disease_model, path = c("01_pre_symptomatic_isolated", "01_hospitalized") )$label <- paste0("(4 → 7) = ", CONST_v_h  * (1 - CONST_epsilon_serious_01) * CONST_KAPPA)
# 
#   E(disease_model, path = c("01_pre_symptomatic_isolated", "01_hospitalized_ICU") )$weight <- CONST_v_c * (1 - CONST_epsilon_serious_01) * CONST_KAPPA
#   E(disease_model, path = c("01_pre_symptomatic_isolated", "01_hospitalized_ICU") )$label <- paste0("(4 → 8) = ", CONST_v_c * (1 - CONST_epsilon_serious_01) * CONST_KAPPA)
# 
# 
# 
# 
#   # From mild
#   E(disease_model, path = c("01_mild_isolated", "01_recovered") )$weight <- CONST_ita
#   E(disease_model, path = c("01_mild_isolated", "01_recovered") )$label <- paste0("(6 → r) = ", CONST_ita)
# 
#   E(disease_model, path = c("01_mild_non_isolated", "01_recovered") )$weight <- CONST_ita
#   E(disease_model, path = c("01_mild_non_isolated", "01_recovered") )$label <- paste0("(5 → r) = ", CONST_ita)
# 
# 
# 
#   # From hospitalized
#   E(disease_model, path = c("01_hospitalized", "01_recovered") )$weight <- CONST_rho * (1 - CONST_f_h)
#   E(disease_model, path = c("01_hospitalized", "01_recovered") )$label <- paste0("(7 → r) = ", CONST_rho * (1 - CONST_f_h))
# 
#   E(disease_model, path = c("01_hospitalized", "01_dead") )$weight <- CONST_rho * CONST_f_h
#   E(disease_model, path = c("01_hospitalized", "01_dead") )$label <- paste0("(7 → d) = ", CONST_rho * CONST_f_h)
# 
# 
#   # From hospitalized with ICU usage
#   E(disease_model, path = c("01_hospitalized_ICU", "01_recovered") )$weight <- CONST_sigma * (1 - CONST_f_c)
#   E(disease_model, path = c("01_hospitalized_ICU", "01_recovered") )$label <- paste0("(8 → r) = ", CONST_sigma * (1 - CONST_f_c))
# 
#   E(disease_model, path = c("01_hospitalized_ICU", "01_dead") )$weight <- CONST_sigma * CONST_f_c
#   E(disease_model, path = c("01_hospitalized_ICU", "01_dead") )$label <- paste0("(8 → d) = ", CONST_sigma * CONST_f_c)
# 
# }



# Infection parameters for people with 2nd dose of vaccination

# {
#   # Infection states for 2 doses: 
#   disease_model <- disease_model+  
#     graph( edges = c("02_latent_infections_not_isolated", "02_pre_symptomatic_non_isolated", # π(1 → 3)
#                      "02_latent_infections_isolated", "02_pre_symptomatic_isolated", # π(2 → 4)
#                      
#                      "02_pre_symptomatic_non_isolated", "02_mild_non_isolated", # π(3 → 5)
#                      "02_pre_symptomatic_non_isolated", "02_mild_isolated", # π(3 → 6)
#                      "02_pre_symptomatic_non_isolated", "02_hospitalized", # π(3 → 7)
#                      "02_pre_symptomatic_non_isolated", "02_hospitalized_ICU", # π(3 → 8)
#                      
#                      "02_pre_symptomatic_isolated", "02_mild_isolated", # π(4 → 6)
#                      "02_pre_symptomatic_isolated", "02_hospitalized", # π(4 → 7)
#                      "02_pre_symptomatic_isolated", "02_hospitalized_ICU", # π(4 → 8)
#                      
#                      "02_mild_isolated", "02_recovered", # π(5 → r)
#                      "02_mild_non_isolated", "02_recovered", # π(6 → r)
#                      "02_hospitalized", "02_recovered", # π(7 → r)
#                      "02_hospitalized_ICU", "02_recovered", # π(8 → r)
#                      
#                      "02_hospitalized", "02_dead", # π(7 → d)
#                      "02_hospitalized_ICU", "02_dead"), # π(8 → d)
#            directed = TRUE)
#   
#   # From Latent
#   E(disease_model, path = c("02_latent_infections_not_isolated", "02_pre_symptomatic_non_isolated") )$weight <- CONST_CHI
#   E(disease_model, path = c("02_latent_infections_not_isolated", "02_pre_symptomatic_non_isolated") )$label <- paste0("(1 → 3) = ", CONST_CHI)
#   
#   E(disease_model, path = c("02_latent_infections_isolated", "02_pre_symptomatic_isolated") )$weight <- CONST_CHI
#   E(disease_model, path = c("02_latent_infections_isolated", "02_pre_symptomatic_isolated") )$label <- paste0("(2 → 4) = ", CONST_CHI)
#   
#   
#   # From Pre-symptomatic
#   E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_mild_non_isolated") )$weight <- ( CONST_v_m + CONST_epsilon_serious_02 * (1 - CONST_v_m)  )* (1 - CONST_p) * CONST_KAPPA
#   E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_mild_non_isolated") )$label  <- paste0("(3 → 5) = ", ( CONST_v_m + CONST_epsilon_serious_02 * (1 - CONST_v_m)  )* (1 - CONST_p) * CONST_KAPPA)
#   
#   E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_mild_isolated") )$weight <- ( CONST_v_m + CONST_epsilon_serious_02 * (1 - CONST_v_m)  ) * CONST_p * CONST_KAPPA
#   E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_mild_isolated") )$label <- paste0("(3 → 6) = ", ( CONST_v_m + CONST_epsilon_serious_02 * (1 - CONST_v_m)  ) * CONST_p * CONST_KAPPA)
#   
#   E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_hospitalized") )$weight <- CONST_v_h * (1 - CONST_epsilon_serious_02) * CONST_KAPPA
#   E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_hospitalized") )$label <- paste0("(3 → 7) = ", CONST_v_h * (1 - CONST_epsilon_serious_02)  * CONST_KAPPA)
#   
#   E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_hospitalized_ICU") )$weight <- CONST_v_c * (1 - CONST_epsilon_serious_02) * CONST_KAPPA
#   E(disease_model, path = c("02_pre_symptomatic_non_isolated", "02_hospitalized_ICU") )$label <- paste0("(3 → 8) = ", CONST_v_c  * (1 - CONST_epsilon_serious_02) * CONST_KAPPA)
#   
#   
#   
#   E(disease_model, path = c("02_pre_symptomatic_isolated", "02_mild_isolated") )$weight <- ( CONST_v_m + CONST_epsilon_serious_02 * (1 - CONST_v_m)  ) * CONST_KAPPA
#   E(disease_model, path = c("02_pre_symptomatic_isolated", "02_mild_isolated") )$label <- paste0("(4 → 6) = ", ( CONST_v_m + CONST_epsilon_serious_02 * (1 - CONST_v_m)  ) * CONST_KAPPA)
#   
#   E(disease_model, path = c("02_pre_symptomatic_isolated", "02_hospitalized") )$weight <- CONST_v_h * (1 - CONST_epsilon_serious_02) * CONST_KAPPA
#   E(disease_model, path = c("02_pre_symptomatic_isolated", "02_hospitalized") )$label <- paste0("(4 → 7) = ", CONST_v_h  * (1 - CONST_epsilon_serious_02) * CONST_KAPPA)
#   
#   E(disease_model, path = c("02_pre_symptomatic_isolated", "02_hospitalized_ICU") )$weight <- CONST_v_c * (1 - CONST_epsilon_serious_02) * CONST_KAPPA
#   E(disease_model, path = c("02_pre_symptomatic_isolated", "02_hospitalized_ICU") )$label <- paste0("(4 → 8) = ", CONST_v_c * (1 - CONST_epsilon_serious_02) * CONST_KAPPA)
#   
#   
#   
#   
#   # From mild
#   E(disease_model, path = c("02_mild_isolated", "02_recovered") )$weight <- CONST_ita
#   E(disease_model, path = c("02_mild_isolated", "02_recovered") )$label <- paste0("(6 → r) = ", CONST_ita)
#   
#   E(disease_model, path = c("02_mild_non_isolated", "02_recovered") )$weight <- CONST_ita
#   E(disease_model, path = c("02_mild_non_isolated", "02_recovered") )$label <- paste0("(5 → r) = ", CONST_ita)
#   
#   
#   
#   # From hospitalized 
#   E(disease_model, path = c("02_hospitalized", "02_recovered") )$weight <- CONST_rho * (1 - CONST_f_h)
#   E(disease_model, path = c("02_hospitalized", "02_recovered") )$label <- paste0("(7 → r) = ", CONST_rho * (1 - CONST_f_h))
#   
#   E(disease_model, path = c("02_hospitalized", "02_dead") )$weight <- CONST_rho * CONST_f_h
#   E(disease_model, path = c("02_hospitalized", "02_dead") )$label <- paste0("(7 → d) = ", CONST_rho * CONST_f_h)
#   
#   
#   # From hospitalized with ICU usage
#   E(disease_model, path = c("02_hospitalized_ICU", "02_recovered") )$weight <- CONST_sigma * (1 - CONST_f_c)
#   E(disease_model, path = c("02_hospitalized_ICU", "02_recovered") )$label <- paste0("(8 → r) = ", CONST_sigma * (1 - CONST_f_c))
#   
#   E(disease_model, path = c("02_hospitalized_ICU", "02_dead") )$weight <- CONST_sigma * CONST_f_c
#   E(disease_model, path = c("02_hospitalized_ICU", "02_dead") )$label <- paste0("(8 → d) = ", CONST_sigma * CONST_f_c)
#   
# }






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




plot(disease_model, layout=layout_as_tree)

# tkplot(disease_model, layout=layout_as_tree, 
#        canvas.height = 800, canvas.width = 1600,
#        vertex.color="#77c98d", edge.arrow.size=0.75)

# Debug output graph 
E(disease_model)
edge_attr(disease_model)

# Generate transition matrix 
transition_matrix <- as_adjacency_matrix(disease_model, attr="weight", sparse = FALSE)

# generate list with indices 
# transition_list <- which(transition_matrix != "", arr.ind = TRUE, useNames = FALSE)


# Infection probability function: MAX_CHANCE * tanh( <contact_time> / TIME_TILL_76p_CHANCE )

# CONST_MAX_CHANCE <- 0.045 # About 3.4% chance by 20 minutes, to max 4.5% by Inf
# CONST_MAX_CHANCE <- 0.035 
# CONST_MAX_CHANCE <- 0.025
# CONST_MAX_CHANCE <- 0.015
CONST_MAX_CHANCE <- 0.010
# CONST_MAX_CHANCE <- 0.005
# CONST_MAX_CHANCE <- 0.002
# CONST_MAX_CHANCE <- 0.001

CONST_TIME_TILL_76p_CHANCE <- 1200 # seconds (20 minutes)

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
    return_prob <- 1 - VACCINE_ODDS * infection_prob_without_vaccine
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



# Memory Expensive approach to reduce total I/O
#
# Reading all the matrices in a list
# 

list_of_contact_matrix <- list()
matrix_index <- 1

library(Matrix)

for(CONTACT_MATRIX_AS_PAIRLIST_FILENAME in LIST_OF_CONTACT_MATRIX_FILEPATH){
  # Debug: 
  # cat(CONTACT_MATRIX_AS_PAIRLIST_FILENAME, "\n")
  
  # Read contact matrix 
  contact_matrix_as_pairlist <- read.table(CONTACT_MATRIX_AS_PAIRLIST_FILENAME, sep=",")
  
  # Adding readable column names 
  colnames(contact_matrix_as_pairlist) <- c("person_id_1", "person_id_2", "contact_in_seconds")
  
  COUNT_MAX_PERSON_ID <- max( max(contact_matrix_as_pairlist$person_id_1), max(contact_matrix_as_pairlist$person_id_2))
  
  # Converting to sparse matrices
  sparse_contact_matrix <- sparseMatrix(i = contact_matrix_as_pairlist$person_id_1, 
                                    j = contact_matrix_as_pairlist$person_id_2, 
                                    # x = getInfection_probability(contact_matrix_as_pairlist$contact_in_seconds)), infection prorbability is now w.r.t viral strain and vaccination status
                                    x = contact_matrix_as_pairlist$contact_in_seconds)
  
  # convert to array
  contact_matrix <- array( data = sparse_contact_matrix, 
                           dim = c(COUNT_MAX_PERSON_ID, COUNT_MAX_PERSON_ID))
  
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

# Calibration mode: 
# TOTAL_SIMULATION_DAYS <- 1

# TOTAL_SIMULATION_DAYS <- 30
# TOTAL_SIMULATION_DAYS <- 60
# TOTAL_SIMULATION_DAYS <- 365

TOTAL_SIMULATED_PERSONS <- max(contact_matrix_as_pairlist[1:2])

STATE <- array(rep(NA, TOTAL_SIMULATED_PERSONS * TOTAL_SIMULATION_DAYS), 
               dim = c(TOTAL_SIMULATED_PERSONS, TOTAL_SIMULATION_DAYS) )



# Set up simulation 

# Set every person to susceptible for the first day
STATE[ , 1] <- which( colnames(transition_matrix) == "susceptible" )





STATE_NAMES = colnames(transition_matrix)

# Book keeping

# Calibration mode: 
STATELIST_SUSCEPTIBLE <- which(colnames(transition_matrix) == "susceptible")

STATELIST_INFECTIOUS <- which(colnames(transition_matrix) == "00_pre_symptomatic_non_isolated" | 
                              colnames(transition_matrix) == "00_mild_non_isolated")

STATELIST_NEW_CASES <- which(colnames(transition_matrix) == "00_pre_symptomatic_isolated" | 
                               colnames(transition_matrix) == "00_mild_isolated")

STATELIST_ACTIVE_CASES <- which(colnames(transition_matrix) == "00_pre_symptomatic_isolated" | 
                                  colnames(transition_matrix) == "00_mild_isolated" | 
                                  colnames(transition_matrix) == "00_hospitalized" | 
                                  colnames(transition_matrix) == "00_hospitalized_ICU") 


# STATELIST_SUSCEPTIBLE <- which(colnames(transition_matrix) == "susceptible" | 
#                                  colnames(transition_matrix) == "received_dose1" |
#                                  colnames(transition_matrix) == "received_dose2")
# 
# STATELIST_LATENT <- which(colnames(transition_matrix) == "00_latent_infections_isolated" | 
#                              colnames(transition_matrix) == "00_latent_infections_not_isolated" | 
#                              colnames(transition_matrix) == "01_latent_infections_isolated" | 
#                              colnames(transition_matrix) == "01_latent_infections_not_isolated" | 
#                             colnames(transition_matrix) == "02_latent_infections_isolated" | 
#                             colnames(transition_matrix) == "02_latent_infections_not_isolated")
# 
# STATELIST_INFECTIOUS <- which(colnames(transition_matrix) == "00_pre_symptomatic_non_isolated" | 
#                                 colnames(transition_matrix) == "00_mild_non_isolated" | 
#                                 colnames(transition_matrix) == "01_pre_symptomatic_non_isolated" | 
#                                 colnames(transition_matrix) == "01_mild_non_isolated" | 
#                                 colnames(transition_matrix) == "02_pre_symptomatic_non_isolated" | 
#                                 colnames(transition_matrix) == "02_mild_non_isolated"
#                               )
# 
# # TODO
# STATELIST_REMOVED <- which(  colnames(transition_matrix) == "00_recovered" | 
#                              colnames(transition_matrix) == "01_recovered" | 
#                              colnames(transition_matrix) == "02_recovered" | 
#                              colnames(transition_matrix) == "00_dead" | 
#                              colnames(transition_matrix) == "01_dead" |
#                              colnames(transition_matrix) == "02_dead" )
# 
# STATELIST_NEW_CASES <- which(colnames(transition_matrix) == "00_latent_infections_isolated" | 
#                              colnames(transition_matrix) == "01_latent_infections_isolated" | 
#                              colnames(transition_matrix) == "02_latent_infections_isolated" | 
#                              colnames(transition_matrix) == "00_mild_isolated" | 
#                              colnames(transition_matrix) == "01_mild_isolated" | 
#                              colnames(transition_matrix) == "02_mild_isolated" )



# COUNT_FIRST_DAY_DISEASE_IMPORT <- 5 # Default parameter
# COUNT_FIRST_DAY_DISEASE_IMPORT <- 1000 # Checking relative proportions (works)
# COUNT_FIRST_DAY_DISEASE_IMPORT <- 100 # Checking for relative infectivity 

# Calibration mode: 
COUNT_FIRST_DAY_DISEASE_IMPORT <- 1 # To try and mimic Campbellton

DAILY_NEW_CASES <- array( rep(0, TOTAL_SIMULATION_DAYS), dim = TOTAL_SIMULATION_DAYS )
DAILY_ACTIVE_CASES <- array( rep(0, TOTAL_SIMULATION_DAYS), dim = TOTAL_SIMULATION_DAYS )

# Set first day new cases to COUNT_FIRST_DAY_DISEASE_IMPORT for bookkeeping
DAILY_NEW_CASES[1] <- COUNT_FIRST_DAY_DISEASE_IMPORT

# Randomly distribute the infection import
first_disease_import <- sample(seq_len(length(STATE[ , 1])), COUNT_FIRST_DAY_DISEASE_IMPORT)

active_voc_df <- filter(voc_df, Proportion > 0)


# Setup infection history for each person in a matrix, where each row is a person id
infection_hist_mat <- matrix (ncol = 5, nrow = TOTAL_SIMULATED_PERSONS)
infection_hist_mat_colnames <- c("variant", "infected on", "infected by", "dose 1 on", "dose 2 on")
colnames(infection_hist_mat) <- infection_hist_mat_colnames

# Book keeping initial variant counts
# variant_string_csv <- toString(active_voc_df$variant)
# varaint_list <- as.list(strsplit(variant_string_csv, ", ")[[1]])

# day_1_variants_mat <- matrix(varaint_list, ncol = 1, nrow = nrow(active_voc_df))
# day_1_variants_mat <- cbind(day_1_variants_mat, 0)
# colnames(day_1_variants_mat) <- c("variant", "count")

day_1_varaint_list <- list()

cat("Setting first day disease import to: ", COUNT_FIRST_DAY_DISEASE_IMPORT, "\n")
for(person_id in first_disease_import){
  
  # Starting with infectious: may be changed to exposed
  STATE[person_id , 1] <- sample(STATELIST_INFECTIOUS, 1)
  
  # Setting infection history 
  # Sample based on proportions
  infecting_variant <- sample(active_voc_df$variant, 1, prob = active_voc_df$Proportion)
  
  
  infection_hist_mat[person_id, "variant"] <- toString(infecting_variant)
  infection_hist_mat[person_id, "infected on"] <- 1 # first day disease import
  
  # Book keeping 
  # day_1_variants_mat[toString(infecting_variant), "count"] <- day_1_variants_mat[toString(infecting_variant), "count"] + 1
  day_1_varaint_list <- append(day_1_varaint_list, toString(infecting_variant))
  
  # Debug
  # cat("person id - infected: ", person_id, "with variant: ", infection_hist_mat[person_id, "variant"], "\n")
  
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


# Simulate from day 2 to TOTAL_SIMULATION_DAYS
for(day_index in 2:TOTAL_SIMULATION_DAYS){
  
  # Debug: 
  # cat("\n")
  # cat("  Sim day: ", day_index, sep = "")
  # cat("\n")
  
  # Slice yesterday's data
  previous_day_df <- data.frame(STATE[ , day_index - 1])
  colnames(previous_day_df) <- c("state_id")
  
  previous_day_df$person_id <- seq.int(nrow(previous_day_df))
  
  previous_day_infectious_df <- filter(previous_day_df, state_id %in% STATELIST_INFECTIOUS)
  
  # previous_day_latent_df <- filter(previous_day_df, state_id %in% STATELIST_LATENT)
  
  previous_day_susceptible_df <- filter(previous_day_df, state_id %in% which( colnames(transition_matrix) == "susceptible" ))
  
  previous_day_dose1_df <- filter(previous_day_df, state_id %in% which( colnames(transition_matrix) == "received_dose1" ))
  
  
  # colnames(previous_day_df) <- c("person_id_1", "state id")
  
  COUNT_NEW_INFECTIONS_TODAY <- 0 # flush the counter
  
  for(person_id in 1:TOTAL_SIMULATED_PERSONS){
    
    # Get state of this person from previous day
    PREV_STATE_INDEX <- STATE[person_id, day_index - 1]
    PREV_STATE_NAME <- STATE_NAMES[PREV_STATE_INDEX]
    
    # for no transitions
    next_state_id <- PREV_STATE_INDEX
    next_state_name <- PREV_STATE_NAME
    
    # Debug: 
    # cat("    \n...............\n")
    # cat("    Previous state of Person no: ", person_id, ", state id: ", PREV_STATE_INDEX, ", state name: ", PREV_STATE_NAME, "\n", sep = "")
    
    
    
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
    POSSIBLE_TRANSITIONS <- which(transition_matrix[PREV_STATE_INDEX, ] != "")
    
    
    POSSIBLE_TRANSITIONS <- data.frame(POSSIBLE_TRANSITIONS)
    colnames(POSSIBLE_TRANSITIONS) <- c("state_id")
    POSSIBLE_TRANSITIONS$transition_weights <- transition_matrix[PREV_STATE_INDEX, which(transition_matrix[PREV_STATE_INDEX, ] != "")]
    
    # Get weights 
    # Check which of the transition weights are CALCULATE_FROM_CONTACT_MATRIX
    # Debug: 
    # print(POSSIBLE_TRANSITIONS
    
    # Hacky embedded stochastic infection possibility in state-evolution matrix
    NO_OF_CONTACT_MATRIX_BASED_TRANSITION <- length(which(POSSIBLE_TRANSITIONS$transition_weights == "CALCULATE_FROM_CONTACT_MATRIX"))
    
    # Get cumulative probability of person_id to be infected by 
    # any of it's neighbour (another person with time,space overlap)
    
    # TOTAL_PROBABILITY_OF_INFECTION <- 0
    
    if(NO_OF_CONTACT_MATRIX_BASED_TRANSITION > 0){
      # contacts_df <- filter(contact_matrix_as_pairlist, person_id_1 == person_id)
      # cat("Contacts of ", person_id)
      # print(contacts_df$person_id_2)
      list_of_contacts <- which(contact_matrix[person_id, ] > 0)
      
      # Check if person 2 was infectious on day_index - 1
      # infectious_contacts_df <- filter(contacts_df, person_id_2 %in% previous_day_infectious_df$person_id)
      # cat("Infectious ontacts of ", person_id)
      # print(infectious_contacts_df$person_id_2)
      list_of_infectious_contacts <- intersect(list_of_contacts, previous_day_infectious_df$person_id)
      
      # COUNT_OF_POSSIBLE_INFECTIOUS_CONTACTS <- nrow(infectious_contacts_df)
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
          
          # same as 
          # rbinom(n = 1, size = 1, prob = infectious_contacts_df[contact_index, ]$infection_prob) == TRUE
          
          # Get relative infectivity of variant
          # Debug: 
          # cat("Variant of contact: ", contact_index, "  is: ", infection_hist_mat[contact_index, "variant"], "\n")
          
          relative_infectivity <- filter(active_voc_df, variant == infection_hist_mat[contact_index, "variant"])$'Relative infectivity'
          contact_time <- contact_matrix[person_id, contact_index]
          
          # Get vaccine efficacy w.r.t contact's variant 
          vaccine_odds <- NA # Assume person_id is without vaccine
          
          if(PREV_STATE_NAME == "received_dose1"){
            vaccine_odds <- filter(active_voc_df, variant == infection_hist_mat[contact_index, "variant"])$'Vaccine dose 1 efficacy'
            
            # Scale w.r.t vaccination day
            dose1_received_on <- strtoi(infection_hist_mat[person_id, "dose 1 on"])
            vaccine_odds <- scaleVaccineOddsLinear(day_index - dose1_received_on) * vaccine_odds
          }
          
          else if(PREV_STATE_NAME == "received_dose2"){
            vaccine_odds1 <- filter(active_voc_df, variant == infection_hist_mat[contact_index, "variant"])$'Vaccine dose 1 efficacy'
            
            # Scale w.r.t vaccination day of first dose
            dose1_received_on <- strtoi(infection_hist_mat[person_id, "dose 1 on"])
            vaccine_odds1 <- scaleVaccineOddsLinear(day_index - dose1_received_on) * vaccine_odds1
            
            
            vaccine_odds2 <- filter(active_voc_df, variant == infection_hist_mat[contact_index, "variant"])$'Vaccine dose 2 efficacy'
            
            # Scale w.r.t vaccination day of 2nd dose
            dose2_received_on <- strtoi(infection_hist_mat[person_id, "dose 2 on"])
            vaccine_odds2 <- scaleVaccineOddsLinear(day_index - dose2_received_on) * vaccine_odds2
            
            vaccine_odds <- max(vaccine_odds1, vaccine_odds2)
            
          }
          
          
          # Note: Introduced about 8 GB of memory and increasd overall runtime 
          # random_coin_toss <- random_unifrom_sample_list[random_sample_iterator]
          # 
          # random_sample_iterator <- random_sample_iterator + 1 
          # # Check overflow 
          # if(random_sample_iterator > COUNT_RANDOM_SAMPLE_LIST){
          #   random_unifrom_sample_list <- runif(COUNT_RANDOM_SAMPLE_LIST)
          #   random_sample_iterator <- 1 # Counter to re-initialise when exhausted
          #   
          #   cat(" >>>>> Reinitialised random sample pool")
          # }
          
          random_coin_toss <- runif(1)
          
          
          # Debug: 
          # cat("           For neighbour: ", contact_index," unifrom_random_sample(0,1): ", random_coin_toss, " , infection probability: ", getInfection_probability(contact_time), " => infection :", random_coin_toss <= getInfection_probability(contact_time), "\n", sep = "")
          
          # Calibration mode: 
          if( random_coin_toss <= getInfection_probability(contact_time) ){
          
          # if( random_coin_toss <= contact_matrix[person_id, contact_index]){
          # if( random_coin_toss <= getInfection_probability_w_variant(relative_infectivity, contact_time)){
          # if( random_coin_toss <= getInfection_probability_w_variant_w_vaccine(vaccine_odds, relative_infectivity, contact_time)){
            
            # Infection event for unvaccinated
            if (PREV_STATE_NAME == "susceptible"){
              next_state_id <- sample( c( which(STATE_NAMES == "00_latent_infections_not_isolated"), which(STATE_NAMES == "00_latent_infections_isolated") ),
                                       1,
                                       prob = c((1 - CONST_q), CONST_q))
            }
            
            # Infection event for person with 1 dose
            else if (PREV_STATE_NAME == "received_dose1"){
              next_state_id <- sample( c( which(STATE_NAMES == "01_latent_infections_not_isolated"), which(STATE_NAMES == "01_latent_infections_isolated") ),
                                       1,
                                       prob = c((1 - CONST_q), CONST_q))
            }
            
            
            # Infection event for person with 2 doses
            else if (PREV_STATE_NAME == "received_dose2"){
              next_state_id <- sample( c( which(STATE_NAMES == "02_latent_infections_not_isolated"), which(STATE_NAMES == "02_latent_infections_isolated") ),
                                       1,
                                       prob = c((1 - CONST_q), CONST_q))
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
    
    # TODO: Not looking out for transitions that may be a mix of both yet
    
    # Non-infectious, static parameter based transition
    else{
      
      # Check if already in a final state
      if(nrow(POSSIBLE_TRANSITIONS) == 0) {
        next_state_id <- PREV_STATE_INDEX
        next_state_name <- STATE_NAMES[next_state_id]
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
        
        
        # Edge: "xx_latent_infections_isolated", "xx_pre_symptomatic_isolated", # π(2 → 4)
        if( (PREV_STATE_NAME == "00_latent_infections_isolated") && (next_state_name == "00_pre_symptomatic_isolated")  || 
            (PREV_STATE_NAME == "01_latent_infections_isolated") && (next_state_name == "01_pre_symptomatic_isolated")  || 
            (PREV_STATE_NAME == "02_latent_infections_isolated") && (next_state_name == "01_pre_symptomatic_isolated")  ) {
            
          COUNT_NEW_INFECTIONS_TODAY <- COUNT_NEW_INFECTIONS_TODAY + 1
        }
        
        # Edge: "xx_pre_symptomatic_non_isolated", "xx_mild_isolated", # π(3 → 6)
        else if( (PREV_STATE_NAME == "00_pre_symptomatic_non_isolated") && (next_state_name == "00_mild_isolated")  || 
                 (PREV_STATE_NAME == "01_pre_symptomatic_non_isolated") && (next_state_name == "01_mild_isolated")  || 
                 (PREV_STATE_NAME == "02_pre_symptomatic_non_isolated") && (next_state_name == "02_mild_isolated")  ) { 
          
          COUNT_NEW_INFECTIONS_TODAY <- COUNT_NEW_INFECTIONS_TODAY + 1
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
write.table(infection_hist_mat, sep=",", OUTPUT_INFECTION_HISTORY_FILEPATH_CSV, row.names = TRUE, col.names = TRUE)



# Stop the clock
cat("\n", proc.time() - ptm)


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

# cat("No. of Possiible infection events: ", random_sample_iterator, "\n")


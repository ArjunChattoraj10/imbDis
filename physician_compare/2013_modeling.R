library(data.table)
library(dplyr)
library(tidyr)
library(caret)

# reading in the datasets
# use of fread since datasets are very large
# argument specified to read data as data.frame

# 2013 dataset with subset of drug_names
prescriber = fread("../data/Medicare_Provider_Utilization_and_Payment_Data__2013_Part_D_Prescriber.csv.gz",
                   na.strings = "NA", data.table = FALSE)

# 2013 dataset with subset of speciality to "Gastroenterology" or "Internal Medicine"
pres_summary = fread("../data/Medicare_Provider_Utilization_and_Payment_Data__Part_D_Prescriber_Summary_Table_CY2013.csv.gz",
                     na.strings = "NA", data.table = FALSE)

 # subset the data to only include NEXIUM with providers from New York
pres_nex_ny = prescriber[prescriber$drug_name == "NEXIUM" & 
                         prescriber$nppes_provider_state == "NY",]

# define the response to be a 1 for specialized (Gastroenterology)
# and 0 for general (Internal Medicine)
# pres_nex_ny_gastro = pres_nex_ny[pres_nex_ny$specialty_description == "Gastroenterology" |
#                                  pres_nex_ny$specialty_description == "Internal Medicine",]
pres_summary$specialty = ifelse(pres_summary$specialty_description == "Gastroenterology", 1, 0)

# npi is the joining id
# the drug name, generic name and specialty (response) are the only variables
# unique to the presriber dataset
joined = inner_join(pres_nex_ny[c("npi", "drug_name", "generic_name")], 
                    pres_summary, by = "npi")

joined$zip3 = factor(substr(joined$nppes_provider_zip5, 1, 3)) #set as factor, 100 set as base
joined$nppes_provider_gender = factor(joined$nppes_provider_gender) #set as factor, F as base

summary(joined$beneficiary_average_risk_score) # 13 NAs
joined_clean = joined[!is.na(joined$beneficiary_average_risk_score),] #removes all rows with NA risk score

summary(joined_clean$beneficiary_dual_count) # 446 NAs -- may need a better way to proceed
joined_clean$beneficiary_dual_count = replace_na(joined_clean$beneficiary_dual_count, 0)
joined_clean$beneficiary_nondual_count = replace_na(joined_clean$beneficiary_nondual_count, 0)

# train-test split - 70/30
set.seed(625)
p = 0.7 
train_i = sample(nrow(joined_clean), p*nrow(joined_clean))
train_dat = joined_clean[train_i,]
test_dat = joined_clean[-train_i,]

# define logistic model
LR = glm(specialty ~ beneficiary_average_risk_score + average_age_of_beneficiaries +
             zip3 + beneficiary_dual_count + beneficiary_nondual_count + total_drug_cost +
             nppes_provider_gender + total_claim_count + bene_count, 
    data = train_dat, family = "binomial")

preds = predict(LR, test_dat, type = "response")

# use simMetric class
source("../src/class_def.R")

SM_LR = simMetric(test_dat$specialty, preds, 1, seq(0.05,0.5,0.05))

auc.simMetric(SM_LR)
brier.simMetric(SM_LR)


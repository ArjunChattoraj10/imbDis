library(data.table)
library(dplyr)
library(tidyr)
library(caret)

# reading in the datasets
# use of fread since datasets are very large
# argument specified to read data as data.frame
prescriber = fread("https://data.cms.gov/api/views/465c-49pb/rows.csv?accessType=DOWNLOAD",
                   na.strings = "NA", data.table = FALSE)
pres_summary = fread("https://data.cms.gov/api/views/mxq9-aiiw/rows.csv?accessType=DOWNLOAD",
                     na.strings = "NA", data.table = FALSE)

 # subset the data to only include NEXIUM with providers from New York
pres_nex_ny = prescriber[prescriber$drug_name == "NEXIUM" & 
                         prescriber$nppes_provider_state == "NY",]

# define the response to be a 1 for specialized (Gastroenterology)
# and 0 for general (Internal Medicine)
pres_nex_ny_gastro = pres_nex_ny[pres_nex_ny$specialty_description == "Gastroenterology" |
                                 pres_nex_ny$specialty_description == "Internal Medicine",]
pres_nex_ny_gastro$specialty = ifelse(pres_nex_ny_gastro$specialty_description == "Gastroenterology", 1, 0)

length(unique(pres_nex_ny_gastro$npi))
nrow(pres_nex_ny_gastro)

# npi is the joining id
# the drug name, generic name and specialty (response) are the only variables
# unique to the presriber dataset
joined = inner_join(pres_nex_ny_gastro[c("npi", "drug_name", "generic_name", "specialty")], 
                    pres_summary, by = "npi")

joined$zip3 = factor(substr(joined$nppes_provider_zip5, 1, 3)) #set as factor, 100 set as base
joined$nppes_provider_gender = factor(joined$nppes_provider_gender) #set as factor, F as base

summary(joined$beneficiary_average_risk_score) # 12 NAs
joined_clean = joined[!is.na(joined$beneficiary_average_risk_score),] #removes all rows with NA risk score

summary(joined_clean$beneficiary_dual_count) # 392 NAs -- may need a better way to proceed
joined_clean$beneficiary_dual_count = replace_na(joined_clean$beneficiary_dual_count, 0)
joined_clean$beneficiary_nondual_count = replace_na(joined_clean$beneficiary_nondual_count, 0)

# train-test split - 70/30
set.seed(625)
p = 0.7 
train_i = sample(nrow(joined_clean), p*nrow(joined_clean))
train_dat = joined_clean[train_i,]
test_dat = joined_clean[-train_i,]




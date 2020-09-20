# TN Data Cleaning
# Begining info: row count = 389728, col count = 99

# Get info about the TN Dataset
library(Hmisc)
library(tidyr)
contents(X2019publicTN_allColumns)
describe(X2019publicTN_allColumns)
summary(X2019publicTN_allColumns)

# remove columns that have info we don't need
TN2019_init <- subset(X2019publicTN_allColumns, select=-activity_year)
TN2019_init <- subset(TN2019_init, select=-state_code)

# remove columns with low data density or no data 
TN2019_init <- subset(TN2019_init, select=-applicant_ethnicity_2)
TN2019_init <- subset(TN2019_init, select=-applicant_ethnicity_3)
TN2019_init <- subset(TN2019_init, select=-applicant_ethnicity_4)
TN2019_init <- subset(TN2019_init, select=-applicant_ethnicity_5)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_2)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_3)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_4)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_5)
TN2019_init <- subset(TN2019_init, select=-applicant_race_2)
TN2019_init <- subset(TN2019_init, select=-applicant_race_3)
TN2019_init <- subset(TN2019_init, select=-applicant_race_4)
TN2019_init <- subset(TN2019_init, select=-applicant_race_5)
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_2)
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_3)
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_4)
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_5)
TN2019_init <- subset(TN2019_init, select=-aus_2)
TN2019_init <- subset(TN2019_init, select=-aus_3)
TN2019_init <- subset(TN2019_init, select=-aus_4)
TN2019_init <- subset(TN2019_init, select=-aus_5)
TN2019_init <- subset(TN2019_init, select=-denial_reason_2)
TN2019_init <- subset(TN2019_init, select=-denial_reason_3)
TN2019_init <- subset(TN2019_init, select=-denial_reason_4)

# remove columns with duplicated age information
# use applicant_age and co_applicant_age columns
TN2019_init <- subset(TN2019_init, select=-applicant_age_above_62)
TN2019_init <- subset(TN2019_init, select=-co_applicant_age_above_62)

#   row count = 389728, col count = 72

##########################################################
# begin evaluation of the race, ethnicity, sex columns  ##
##########################################################
# 'Not applicable' values in race, ethnicity, sex and age columns indicate  
# purchaser is not a 'natural person' or the info was not reported by
# the loan originator, cannot use these for bias analysis

# remove rows with missing values
TN2019_init <- subset(TN2019_init, co_applicant_race_1!='')
TN2019_init <- subset(TN2019_init, applicant_race_1!='')
TN2019_init <- subset(TN2019_init, co_applicant_ethnicity_1!='')
TN2019_init <- subset(TN2019_init, applicant_ethnicity_1!='')

#   row count = 389592, col count = 72

# remove records for loans that were neither approved nor denied and purchased loans
# where the entity did not review the loan application or make the credit decision
TN2019_init <- subset(TN2019_init, action_taken != 6)  # purchased loan
TN2019_init <- subset(TN2019_init, action_taken != 5)  # file closed for incompleteness
TN2019_init <- subset(TN2019_init, action_taken != 4)  # application withdrawn by applicant

#   row count = 272973, col count = 72

# remove purchaser_type - identifies type of entity that purchased the loan
# that was originated by the lender for this record; not useful for this analysis
TN2019_init <- subset(TN2019_init, select=-purchaser_type)

# remove rows where race/ethnicity/sex has NA or info was not provided by applicant
TN2019_init <- subset(TN2019_init, applicant_ethnicity_1 != 4) # Not applicable
TN2019_init <- subset(TN2019_init, applicant_ethnicity_1 != 3) # info not provided by applicant 
TN2019_init <- subset(TN2019_init, co_applicant_ethnicity_1 != 4) # Not applicable
TN2019_init <- subset(TN2019_init, co_applicant_ethnicity_1 != 3) # info not provided by applicant 
TN2019_init <- subset(TN2019_init, applicant_sex != 4)  # Not applicable
TN2019_init <- subset(TN2019_init, applicant_sex != 3)  # info not provided by applicant
TN2019_init <- subset(TN2019_init, co_applicant_sex != 4)  # Not applicable
TN2019_init <- subset(TN2019_init, co_applicant_sex != 3)  # info not provided by applicant
TN2019_init <- subset(TN2019_init, applicant_race_1 != 7)  # Not applicable
TN2019_init <- subset(TN2019_init, applicant_race_1 != 6)  # info not provided by applicant
TN2019_init <- subset(TN2019_init, co_applicant_race_1 != 7)  # Not applicable
TN2019_init <- subset(TN2019_init, co_applicant_race_1 != 6)  # info not provided by applicant
TN2019_init <- subset(TN2019_init, applicant_age != 8888) # age not reported

#   row count = 229883, col count = 71

# remove rows for applications made primarily for business purposes
TN2019_init <- subset(TN2019_init, business_or_commercial_purpose == 2)
# remove business_or_commercial_purpose column
TN2019_init <- subset(TN2019_init, select=-business_or_commercial_purpose)

# keep only rows for applications for only one dwelling
TN2019_init <- subset(TN2019_init, total_units == 1)
# remove total_units column
TN2019_init <- subset(TN2019_init, select=-total_units)

# remove column that has only NA or Exempt values
TN2019_init <- subset(TN2019_init, select=-multifamily_affordable_units)
# exclude rows where loan purpose is NA
# purchased loans where origination took place before Jan 1 2018
TN2019_init <- subset(TN2019_init, loan_purpose != 5)

# remove the 'observed' columns - only provides context of how the demo data was collected
# not useful for this analysis
TN2019_init <- subset(TN2019_init, select=-applicant_ethnicity_observed)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_observed)
TN2019_init <- subset(TN2019_init, select=-applicant_race_observed)
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_observed)
TN2019_init <- subset(TN2019_init, select=-applicant_sex_observed)
TN2019_init <- subset(TN2019_init, select=-co_applicant_sex_observed)

#   row count = 210690, col count = 62

################################################################################
# evaluate the 'derived' column level frequencies
################################################################################
summary(TN2019_init$derived_dwelling_category)
# keep construction_method
# remove derived_dwelling_category
TN2019_init <- subset(TN2019_init, select=-derived_dwelling_category)

# remove location columns - not meaningful for this analysis
TN2019_init <- subset(TN2019_init, select=-derived_msa_md)
TN2019_init <- subset(TN2019_init, select=-census_tract)
TN2019_init <- subset(TN2019_init, select=-county_code)

################################################################################
#  remove columns that cannot or will not be used for this analysis
################################################################################

TN2019_init <- subset(TN2019_init, select=-lei)  # Legal Entity Identifier, too many levels to be useful
TN2019_init <- subset(TN2019_init, select=-conforming_loan_limit)  # not meaningful for this analysis
TN2019_init <- subset(TN2019_init, select=-tract_population)  # corresponds to census_tract
TN2019_init <- subset(TN2019_init, select=-tract_minority_population_percent)  # corresponds to census_tract
TN2019_init <- subset(TN2019_init, select=-ffiec_msa_md_median_family_income)  # not meaningful for this analysis
TN2019_init <- subset(TN2019_init, select=-tract_to_msa_income_percentage)  # corresponds to census_tract
TN2019_init <- subset(TN2019_init, select=-tract_owner_occupied_units) # corresponds to census_tract
TN2019_init <- subset(TN2019_init, select=-tract_one_to_four_family_homes)  # corresponds to census_tract
TN2019_init <- subset(TN2019_init, select=-tract_median_age_of_housing_units)  # corresponds to census_tract
TN2019_init <- subset(TN2019_init, select=-manufactured_home_secured_property_type)  # corresponds to construction method
TN2019_init <- subset(TN2019_init, select=-manufactured_home_land_property_interest)  # corresponds to construction method

#   row count = 210690, col count = 47

##########################################################################
# convert action_taken levels from numbers to descriptives
convert_action_taken_descr <- function(action_taken){
  if (action_taken == 1){  return('Loan originated')  }
  else if (action_taken == 2){  return('Application approved but not accepted')  }
  else if (action_taken == 3){  return('Application denied')  }
  else if (action_taken == 4){  return('Application withdrawn by applicant')  }
  else if (action_taken == 5){  return('File closed for incompleteness')  }
  else if (action_taken == 6){  return('Purchased loan')  }
  else if (action_taken == 7){  return('Preapproval request denied')  }
  else if (action_taken == 8){  return('Preapproval request approved but not accepted')  }
}
TN2019_init$Action_Taken_Description <- sapply(TN2019_init$action_taken,convert_action_taken_descr)
TN2019_init$Action_Taken_Description <- as.factor(TN2019_init$Action_Taken_Description)

# map action_taken levels to application_status 
convert_application_status <- function(action_taken){
  if (action_taken == 1){  return('approved')  }
  else if (action_taken == 2){  return('approved')  }
  else if (action_taken == 3){  return('denied')  }
  else if (action_taken == 4){  return('withdrawn')  } 
  else if (action_taken == 5){  return('incomplete')  }
  else if (action_taken == 6){  return('Purchased loan')  }
  else if (action_taken == 7){  return('denied')  }
  else if (action_taken == 8){  return('approved')  }
}
TN2019_init$Application_Status <- sapply(TN2019_init$action_taken,convert_application_status)
TN2019_init$Application_Status <- as.factor(TN2019_init$Application_Status)
# drop action_taken (lower case) column 
TN2019_init <- subset(TN2019_init, select= -action_taken)

#   row count = 210690, col count = 48

################################################################################
#  convert numeric levels to descriptive terms for easier visual interpretation
################################################################################

# convert applicant_sex levels from numbers to descriptives
convert_applicant_sex <- function(applicant_sex){
  if (applicant_sex == 1){  return('Male')  }
  else if (applicant_sex == 2){  return('Female')  }
  else if (applicant_sex == 3){  return('Information not provided by applicant')  }
  else if (applicant_sex == 4){  return('Not applicable')  }
  else if (applicant_sex == 6){  return('Both')  }  
}
TN2019_init$Applicant_Sex <- sapply(TN2019_init$applicant_sex,convert_applicant_sex)
TN2019_init$Applicant_Sex <- as.factor(TN2019_init$Applicant_Sex)
# drop applicant_sex (lower case) column 
TN2019_init <- subset(TN2019_init, select= -applicant_sex)

# convert co_applicant_sex levels from numbers to descriptives
convert_co_applicant_sex <- function(co_applicant_sex){
  if (co_applicant_sex == 1){  return('Male')  }
  else if (co_applicant_sex == 2){  return('Female')  }
  else if (co_applicant_sex == 3){  return('Information not provided by applicant')  }
  else if (co_applicant_sex == 4){  return('Not applicable')  }
  else if (co_applicant_sex == 5){  return('No co-applicant')  }
  else if (co_applicant_sex == 6){  return('Both')  }
}
TN2019_init$Co_Applicant_Sex <- sapply(TN2019_init$co_applicant_sex,convert_co_applicant_sex)
TN2019_init$Co_Applicant_Sex <- as.factor(TN2019_init$Co_Applicant_Sex)
# drop applicant_sex (lower case) column 
TN2019_init <- subset(TN2019_init, select= -co_applicant_sex)

# convert applicant_ethnicity_1 levels from numbers to descriptives
convert_applicant_ethnicity_1 <- function(applicant_ethnicity_1){
  if (applicant_ethnicity_1 == 1){  return('Hispanic or Latino')  }
  else if (applicant_ethnicity_1 == 11){  return('Mexican')  }
  else if (applicant_ethnicity_1 == 12){  return('Puerto Rican')  }
  else if (applicant_ethnicity_1 == 13){  return('Cuban')  }
  else if (applicant_ethnicity_1 == 14){  return('Other Hispanic or Latino')  }
  else if (applicant_ethnicity_1 == 2){  return('Not Hispanic or Latino')  }
  else if (applicant_ethnicity_1 == 3){  return('Information not provided by applicant')  }
  else if (applicant_ethnicity_1 == 4){  return('Not applicable')  }
}
TN2019_init$Applicant_Ethnicity <- sapply(TN2019_init$applicant_ethnicity_1,convert_applicant_ethnicity_1)
TN2019_init$Applicant_Ethnicity <- as.factor(TN2019_init$Applicant_Ethnicity)
# drop applicant_ethnicity_1 (lower case) column 
TN2019_init <- subset(TN2019_init, select= -applicant_ethnicity_1)

# convert co_applicant_ethnicity_1 levels from numbers to descriptives
convert_co_applicant_ethnicity_1<- function(co_applicant_ethnicity_1){
  if (co_applicant_ethnicity_1 == 1){  return('Hispanic or Latino')  }
  else if (co_applicant_ethnicity_1 == 11){  return('Mexican')  }
  else if (co_applicant_ethnicity_1 == 12){  return('Puerto Rican')  }
  else if (co_applicant_ethnicity_1 == 13){  return('Cuban')  }
  else if (co_applicant_ethnicity_1 == 14){  return('Other Hispanic or Latino')  }
  else if (co_applicant_ethnicity_1 == 2){  return('Not Hispanic or Latino')  }
  else if (co_applicant_ethnicity_1 == 3){  return('Information not provided by applicant')  }
  else if (co_applicant_ethnicity_1 == 4){  return('Not applicable')  }
  else if (co_applicant_ethnicity_1 == 5){  return('No co-applicant')  }
}
TN2019_init$Co_Applicant_Ethnicity <- sapply(TN2019_init$co_applicant_ethnicity_1,convert_co_applicant_ethnicity_1)
TN2019_init$Co_Applicant_Ethnicity <- as.factor(TN2019_init$Co_Applicant_Ethnicity)
# drop applicant_ethnicity_1 (lower case) column 
TN2019_init <- subset(TN2019_init, select= -co_applicant_ethnicity_1)

# convert applicant_race_1 levels from numbers to descriptives
convert_applicant_race_1<- function(applicant_race_1){
  if (applicant_race_1 == 1){  return('American Indian or Alaska Native')  }
  else if (applicant_race_1 == 2){  return('Asian')  }
  else if (applicant_race_1 == 21){  return('Asian Indian')  }
  else if (applicant_race_1 == 22){  return('Chinese')  }
  else if (applicant_race_1 == 23){  return('Filipino')  }
  else if (applicant_race_1 == 24){  return('Japanese')  }
  else if (applicant_race_1 == 25){  return('Korean')  }
  else if (applicant_race_1 == 26){  return('Vietnamese')  }
  else if (applicant_race_1 == 27){  return('Other Asian')  }
  else if (applicant_race_1 == 3){  return('Black or African American')  }
  else if (applicant_race_1 == 4){  return('Native Hawaiian or Other Pacific Islander')  }
  else if (applicant_race_1 == 41){  return('Native Hawaiian')  }
  else if (applicant_race_1 == 42){  return('Guamanian or Chamorro')  }
  else if (applicant_race_1 == 43){  return('Samoan')  }
  else if (applicant_race_1 == 44){  return('Other Pacific Islander')  }
  else if (applicant_race_1 == 5){  return('White')  }
  else if (applicant_race_1 == 6){  return('Information not provided by applicant')  }
  else if (applicant_race_1 == 7){  return('Not applicable')  }
}
TN2019_init$Applicant_Race <- sapply(TN2019_init$applicant_race_1,convert_applicant_race_1)
TN2019_init$Applicant_Race <- as.factor(TN2019_init$Applicant_Race)
# drop applicant_race_1 (lower case) column 
TN2019_init <- subset(TN2019_init, select= -applicant_race_1)

# convert applicant_race_1 levels from numbers to descriptives
convert_co_applicant_race_1<- function(co_applicant_race_1){
  if (co_applicant_race_1 == 1){  return('American Indian or Alaska Native')  }
  else if (co_applicant_race_1 == 2){  return('Asian')  }
  else if (co_applicant_race_1 == 21){  return('Asian Indian')  }
  else if (co_applicant_race_1 == 22){  return('Chinese')  }
  else if (co_applicant_race_1 == 23){  return('Filipino')  }
  else if (co_applicant_race_1 == 24){  return('Japanese')  }
  else if (co_applicant_race_1 == 25){  return('Korean')  }
  else if (co_applicant_race_1 == 26){  return('Vietnamese')  }
  else if (co_applicant_race_1 == 27){  return('Other Asian')  }
  else if (co_applicant_race_1 == 3){  return('Black or African American')  }
  else if (co_applicant_race_1 == 4){  return('Native Hawaiian or Other Pacific Islander')  }
  else if (co_applicant_race_1 == 41){  return('Native Hawaiian')  }
  else if (co_applicant_race_1 == 42){  return('Guamanian or Chamorro')  }
  else if (co_applicant_race_1 == 43){  return('Samoan')  }
  else if (co_applicant_race_1 == 44){  return('Other Pacific Islander')  }
  else if (co_applicant_race_1 == 5){  return('White')  }
  else if (co_applicant_race_1 == 6){  return('Information not provided by applicant')  }
  else if (co_applicant_race_1 == 7){  return('Not applicable')  }
  else if (co_applicant_race_1 == 8){  return('No co-applicant')  }
}
TN2019_init$Co_Applicant_Race <- sapply(TN2019_init$co_applicant_race_1,convert_co_applicant_race_1)
TN2019_init$Co_Applicant_Race <- as.factor(TN2019_init$Co_Applicant_Race)
# drop applicant_race_1 (lower case) column 
TN2019_init <- subset(TN2019_init, select= -co_applicant_race_1)


################################################################################
# convert remaining column levels with numeric representation to descriptives
################################################################################

# convert loan_purpose levels from numbers to descriptives
convert_loan_purpose <- function(loan_purpose){
  if (loan_purpose == 1){  return('Home purchase')  }
  else if (loan_purpose == 2){  return('Home improvement')  }
  else if (loan_purpose == 31){  return('Refinancing')  }
  else if (loan_purpose == 32){  return('Cash out refinancing')  }
  else if (loan_purpose == 4){ return('Other purpose') }
  else if (loan_purpose == 5){ return('Not applicable') }
}
TN2019_init$Loan_Purpose <- sapply(TN2019_init$loan_purpose,convert_loan_purpose)
TN2019_init$Loan_Purpose <- as.factor(TN2019_init$Loan_Purpose)
# drop loan_purpose (lower case) column 
TN2019_init <- subset(TN2019_init, select= -loan_purpose)

##########################################################################
# convert preapproval levels from numbers to descriptives
convert_preapproval<- function(preapproval){
  if (preapproval== 1){  return('Preapproval requested')  }
  else if (preapproval  == 2){  return('Preapproval not requested')  }
}
TN2019_init$Preapproval <- sapply(TN2019_init$preapproval,convert_preapproval)
TN2019_init$Preapproval <- as.factor(TN2019_init$Preapproval)
# drop preapproval (lower case) column 
TN2019_init <- subset(TN2019_init, select= -preapproval)

##########################################################################
# convert reverse_mortgage levels from numbers to descriptives
convert_reverse_mortgage <- function(reverse_mortgage){
  if (reverse_mortgage == 1){  return('Reverse Mortgage')  }
  else if (reverse_mortgage == 2){  return('Not a reverse mortgage')  }
  else if (reverse_mortgage == 1111){  return('Exempt')  }
}
TN2019_init$Reverse_Mortgage <- sapply(TN2019_init$reverse_mortgage,convert_reverse_mortgage)
TN2019_init$Reverse_Mortgage <- as.factor(TN2019_init$Reverse_Mortgage)
# drop reverse_mortgage (lower case) column 
TN2019_init <- subset(TN2019_init, select= -reverse_mortgage)

##########################################################################
# convert open_end_line_of_credit levels from numbers to descriptives
convert_open_LOC <- function(open_end_line_of_credit){
  if (open_end_line_of_credit == 1){  return('Open-end line of credit')  }
  else if (open_end_line_of_credit == 2){  return('Not an open-end line of credit')  }
  else if (open_end_line_of_credit == 1111){  return('Exempt')  }
}
TN2019_init$Open_End_Line_of_Credit <- sapply(TN2019_init$open_end_line_of_credit,convert_open_LOC)
TN2019_init$Open_End_Line_of_Credit <- as.factor(TN2019_init$Open_End_Line_of_Credit)
# drop open_end_line_of_credit (lower case) column 
TN2019_init <- subset(TN2019_init, select= -open_end_line_of_credit)

##########################################################################
# convert hoepa_status levels from numbers to descriptives
convert_hoepa_status <- function(hoepa_status){
  if (hoepa_status == 1){  return('High-cost mortgage')  }
  else if (hoepa_status == 2){  return('Not a high-cost mortgage')  }
  else if (hoepa_status == 3){  return('Not applicable')  }
}
TN2019_init$Hoepa_Status <- sapply(TN2019_init$hoepa_status,convert_hoepa_status)
TN2019_init$Hoepa_Status <- as.factor(TN2019_init$Hoepa_Status)
# drop hoepa_status (lower case) column 
TN2019_init <- subset(TN2019_init, select= -hoepa_status)

##########################################################################
# convert negative_amortization levels from numbers to descriptives
convert_negative_amortization <- function(negative_amortization){
  if (negative_amortization == 1){  return('Negative amortization')  }
  else if (negative_amortization == 2){  return('No negative amortization')  }
  else if (negative_amortization == 1111){  return('Exempt')  }
}
TN2019_init$Negative_Amortization <- sapply(TN2019_init$negative_amortization,convert_negative_amortization)
TN2019_init$Negative_Amortization <- as.factor(TN2019_init$Negative_Amortization)
# drop negative_amortization (lower case) column 
TN2019_init <- subset(TN2019_init, select= -negative_amortization)

##########################################################################
# convert interest_only_payment levels from numbers to descriptives
convert_interest_only_payment <- function(interest_only_payment){
  if (interest_only_payment == 1){  return('Interest-only payments')  }
  else if (interest_only_payment == 2){  return('No interest-only payments')  }
  else if (interest_only_payment == 1111){  return('Exempt')  }
}
TN2019_init$Interest_Only_Payment <- sapply(TN2019_init$interest_only_payment,convert_interest_only_payment)
TN2019_init$Interest_Only_Payment <- as.factor(TN2019_init$Interest_Only_Payment)
# drop interest_only_payment (lower case) column 
TN2019_init <- subset(TN2019_init, select= -interest_only_payment)

##########################################################################
# convert balloon_payment levels from numbers to descriptives
convert_balloon_payment <- function(balloon_payment){
  if (balloon_payment == 1){  return('Balloon payment')  }
  else if (balloon_payment == 2){  return('Not a balloon payment')  }
  else if (balloon_payment == 1111){ return('Exempt')  }
}
TN2019_init$Balloon_Payment <- sapply(TN2019_init$balloon_payment,convert_balloon_payment)
TN2019_init$Balloon_Payment <- as.factor(TN2019_init$Balloon_Payment)
# drop balloon_payment (lower case) column 
TN2019_init <- subset(TN2019_init, select= -balloon_payment)

##########################################################################
# convert other_nonamortizing_features levels from numbers to descriptives
convert_other_nonamortizing_features <- function(other_nonamortizing_features){
  if (other_nonamortizing_features == 1){  return('Other non-fully amortizing features')  }
  else if (other_nonamortizing_features == 2){  return('No other non-fully amortizing features')  }
  else if (other_nonamortizing_features == 1111){ return('Exempt')  }
}
TN2019_init$Other_Nonamortizing_Features <- 
    sapply(TN2019_init$other_nonamortizing_features,convert_other_nonamortizing_features)
TN2019_init$Other_Nonamortizing_Features <- as.factor(TN2019_init$Other_Nonamortizing_Features)
# drop other_nonamortizing_features (lower case) column 
TN2019_init <- subset(TN2019_init, select= -other_nonamortizing_features)

##########################################################################
# convert construction_method levels from numbers to descriptives
convert_construction_method <- function(construction_method){
  if (construction_method == 1){  return('Site-built')  }
  else if (construction_method == 2){  return('Manufactured home')  }
}
TN2019_init$Construction_Method <- sapply(TN2019_init$construction_method,convert_construction_method)
TN2019_init$Construction_Method <- as.factor(TN2019_init$Construction_Method)
# drop occupancy_type (lower case) column 
TN2019_init <- subset(TN2019_init, select= -construction_method)

##########################################################################
# convert loan_type levels from numbers to descriptives
convert_loan_type <- function(loan_type){
  if (loan_type == 1){  return('Conventional')  }
  else if (loan_type == 2){  return('FHA')  }
  else if (loan_type == 3){  return('VA')  }
  else if (loan_type == 4){  return('RHS or FSA')  }
}
TN2019_init$Loan_Type <- sapply(TN2019_init$loan_type,convert_loan_type)
TN2019_init$Loan_Type <- as.factor(TN2019_init$Loan_Type)
# drop occupancy_type (lower case) column 
TN2019_init <- subset(TN2019_init, select= -loan_type)

##########################################################################
# convert lien_status levels from numbers to descriptives
convert_lien_status <- function(lien_status){
  if (lien_status == 1){  return('First lien')  }
  else if (lien_status == 2){  return('Subordinate lien')  }
}
TN2019_init$Lien_Status <- sapply(TN2019_init$lien_status,convert_lien_status)
TN2019_init$Lien_Status <- as.factor(TN2019_init$Lien_Status)
# drop occupancy_type (lower case) column 
TN2019_init <- subset(TN2019_init, select= -lien_status)

##########################################################################
# convert occupancy_type levels from numbers to descriptives
convert_occupancy_type <- function(occupancy_type){
  if (occupancy_type == 1){  return('Principal residence')  }
  else if (occupancy_type == 2){  return('Second residence')  }
  else if (occupancy_type == 3){  return('Investment property')  }
}
TN2019_init$Occupancy_Type <- sapply(TN2019_init$occupancy_type,convert_occupancy_type)
TN2019_init$Occupancy_Type <- as.factor(TN2019_init$Occupancy_Type)
# drop occupancy_type (lower case) column 
TN2019_init <- subset(TN2019_init, select= -occupancy_type)

##########################################################################
# convert submission_of_application levels from numbers to descriptives
convert_submission_of_application <- function(submission_of_application){
  if (submission_of_application == 1){  return('Submitted directly to your institution')  }
  else if (submission_of_application == 2){  return('Not submitted directly to your institution')  }
  else if (submission_of_application == 3){  return('Not applicable')  }
  else if (submission_of_application == 1111){  return('Exempt')  }
}
TN2019_init$Submission_Of_Application <-
    sapply(TN2019_init$submission_of_application,convert_submission_of_application)
TN2019_init$Submission_Of_Application <- as.factor(TN2019_init$Submission_Of_Application)
# drop submission_of_application (lower case) column 
TN2019_init <- subset(TN2019_init, select= -submission_of_application)

##########################################################################
# convert initially_payable_to_institution levels from numbers to descriptives
convert_initially_payable_to_institution<- function(initially_payable_to_institution){
  if (initially_payable_to_institution == 1){  return('Initially payable to your institution')  }
  else if (initially_payable_to_institution == 2){  return('Not initially payable to your institution')  }
  else if (initially_payable_to_institution == 3){  return('Not applicable')  }
  else if (initially_payable_to_institution == 1111){  return('Exempt')  }
}
TN2019_init$Initially_Payable_To_Institution <- 
    sapply(TN2019_init$initially_payable_to_institution,convert_initially_payable_to_institution)
TN2019_init$Initially_Payable_To_Institution <- as.factor(TN2019_init$Initially_Payable_To_Institution)
# drop initially_payable_to_institution (lower case) column 
TN2019_init <- subset(TN2019_init, select= -initially_payable_to_institution)

##########################################################################
# convert aus_1 (automated underwriting system) levels from numbers to descriptives
convert_aus_1<- function(aus_1){
  if (aus_1 == 1){  return('Desktop Underwriter (DU)')  }
  else if (aus_1 == 2){  return('Loan Prospector (LP) or Loan Product Advisor')  }
  else if (aus_1 == 3){  return('Technology Open to Approved Lenders (TOTAL) Scorecard')  }
  else if (aus_1 == 4){  return('Guaranteed Underwriting System (GUS)')  }
  else if (aus_1 == 5){  return('Other')  }
  else if (aus_1 == 6){  return('Not applicable')  }
  else if (aus_1 == 1111){  return('Exempt')  }
}
TN2019_init$AUS <- sapply(TN2019_init$aus_1,convert_aus_1)
TN2019_init$AUS <- as.factor(TN2019_init$AUS)
# drop aus_1 (lower case) column 
TN2019_init <- subset(TN2019_init, select= -aus_1)

##########################################################################
# convert denial_reason_1 levels from numbers to descriptives
convert_denial_reason_1<- function(denial_reason_1){
  if (denial_reason_1 == 1){  return('Debt-to-income ratio')  }
  else if (denial_reason_1 == 2){  return('Employment history')  }
  else if (denial_reason_1 == 3){  return('Credit history')  }
  else if (denial_reason_1 == 4){  return('Collateral')  }
  else if (denial_reason_1 == 5){  return('Insufficient cash (downpayment, closing costs)')  }
  else if (denial_reason_1 == 6){  return('Unverifiable information')  }
  else if (denial_reason_1 == 7){  return('Credit application incomplete')  }
  else if (denial_reason_1 == 8){  return('Mortgage insurance denied')  }
  else if (denial_reason_1 == 9){  return('Other')  }
  else if (denial_reason_1 == 10){  return('Not applicable')  }
  else if (denial_reason_1 == 1111){  return('Exempt')  }
}
TN2019_init$Denial_Reason <- sapply(TN2019_init$denial_reason_1,convert_denial_reason_1)
TN2019_init$Denial_Reason <- as.factor(TN2019_init$Denial_Reason)
# drop denial_reason_1 (lower case) column 
TN2019_init <- subset(TN2019_init, select= -denial_reason_1)

##########################################################################
# convert applicant_credit_score_type levels from numbers to descriptives
convert_applicant_credit_score_type<- function(applicant_credit_score_type){
  if (applicant_credit_score_type == 1){  return('Equifax Beacon 5.0')  }
  else if (applicant_credit_score_type == 2){  return('Experian Fair Isaac')  }
  else if (applicant_credit_score_type == 3){  return('FICO Risk Score Classic 04')  }
  else if (applicant_credit_score_type == 4){  return('FICO Risk Score Classic 98')  }
  else if (applicant_credit_score_type == 5){  return('VantageScore 2.0')  }
  else if (applicant_credit_score_type == 6){  return('VantageScore 3.0')  }
  else if (applicant_credit_score_type == 7){  return('More than one credit scoring model')  }
  else if (applicant_credit_score_type == 8){  return('Other credit scoring model')  }
  else if (applicant_credit_score_type == 9){  return('Not applicable')  }
  else if (applicant_credit_score_type == 1111){  return('Exempt')  }
}
TN2019_init$Applicant_Credit_Score_Type <- 
    sapply(TN2019_init$applicant_credit_score_type,convert_applicant_credit_score_type)
TN2019_init$Applicant_Credit_Score_Type <- as.factor(TN2019_init$Applicant_Credit_Score_Type)
# drop applicant_credit_score_type (lower case) column 
TN2019_init <- subset(TN2019_init, select= -applicant_credit_score_type)

##########################################################################
# convert co_applicant_credit_score_type levels from numbers to descriptives
convert_co_applicant_credit_score_type<- function(co_applicant_credit_score_type){
  if (co_applicant_credit_score_type == 1){  return('Equifax Beacon 5.0')  }
  else if (co_applicant_credit_score_type == 2){  return('Experian Fair Isaac')  }
  else if (co_applicant_credit_score_type == 3){  return('FICO Risk Score Classic 04')  }
  else if (co_applicant_credit_score_type == 4){  return('FICO Risk Score Classic 98')  }
  else if (co_applicant_credit_score_type == 5){  return('VantageScore 2.0')  }
  else if (co_applicant_credit_score_type == 6){  return('VantageScore 3.0')  }
  else if (co_applicant_credit_score_type == 7){  return('More than one credit scoring model')  }
  else if (co_applicant_credit_score_type == 8){  return('Other credit scoring model')  }
  else if (co_applicant_credit_score_type == 9){  return('Not applicable')  }
  else if (co_applicant_credit_score_type == 10){  return('No co-applicant')  }
  else if (co_applicant_credit_score_type == 1111){  return('Exempt')  }
}
TN2019_init$Co_Applicant_Credit_Score_Type <- 
    sapply(TN2019_init$co_applicant_credit_score_type,convert_co_applicant_credit_score_type)
TN2019_init$Co_Applicant_Credit_Score_Type <- as.factor(TN2019_init$Co_Applicant_Credit_Score_Type)
# drop co_applicant_credit_score_type (lower case) column 
TN2019_init <- subset(TN2019_init, select= -co_applicant_credit_score_type)


# looking for levels w/ 0 obs and other data issues
contents(TN2019_init)
describe(TN2019_init)
# remove levels in factor variables with 0 observations
TN2019_init$derived_sex <- factor(TN2019_init$derived_sex)
TN2019_init$Applicant_Sex <- factor(TN2019_init$Applicant_Sex)
TN2019_init$derived_ethnicity <- factor(TN2019_init$derived_ethnicity)
TN2019_init$derived_race <- factor(TN2019_init$derived_race)
TN2019_init$Applicant_Ethnicity <- factor(TN2019_init$Applicant_Ethnicity)
TN2019_init$Applicant_Race <- factor(TN2019_init$Applicant_Race)
TN2019_init$applicant_age <- factor(TN2019_init$applicant_age)
TN2019_init$derived_loan_product_type <- factor(TN2019_init$derived_loan_product_type)
# change property_value to numeric column
TN2019_init$property_value <- as.double(TN2019_init$property_value)
# remove NAs for income and property_value
TN2019_init <- subset(TN2019_init, income !='')
TN2019_init <- subset(TN2019_init, property_value != '')

#   row count = 197926, col count = 48

###############################################################################
# subset the main analysis columns for further analysis
TN2019_init_backup <- TN2019_init

TN2019_regression <- TN2019_init
TN2019_dTree <- TN2019_init
TN2019_approved <- subset(TN2019_init, Application_Status == 'approved')
TN2019_denied <- subset(TN2019_init, Application_Status == 'denied')






# TN Data Cleaning
# Begining info: row count = 389728, col count = 99

# Get info about the TN Dataset
library(Hmisc)
describe(X2019publicTN_allColumns)

# remove columns that have info we don't need
TN2019 <- subset(X2019publicTN_allColumns, select=-activity_year)
TN2019 <- subset(TN2019, select=-state_code)

# remove columns with duplicated age information
TN2019 <- subset(TN2019, select=-applicant_age_above_62)
TN2019 <- subset(TN2019, select=-co_applicant_age_above_62)

# remove rows with action_taken in (4,5,6)
# action_taken = 4 (application withdrawn)
# action_taken = 5 (application closed as incomplete)
# action_taken = 6 (purchased loan, meaning an entity purchased the loan)
TN2019 <- subset(TN2019, action_taken!=4)
TN2019 <- subset(TN2019, action_taken!=5)
TN2019 <- subset(TN2019, action_taken!=6)

##############################################################
# remove columns with low data density or no data 
##############################################################
TN2019 <- subset(TN2019, select=-aus_2)
TN2019 <- subset(TN2019, select=-aus_3)
TN2019 <- subset(TN2019, select=-aus_4)
TN2019 <- subset(TN2019, select=-aus_5)
TN2019 <- subset(TN2019, select=-denial_reason_2)
TN2019 <- subset(TN2019, select=-denial_reason_3)
TN2019 <- subset(TN2019, select=-denial_reason_4)
TN2019 <- subset(TN2019, select=-co_applicant_race_2)
TN2019 <- subset(TN2019, select=-co_applicant_race_3)
TN2019 <- subset(TN2019, select=-co_applicant_race_4)
TN2019 <- subset(TN2019, select=-co_applicant_race_5)
TN2019 <- subset(TN2019, select=-applicant_race_2)
TN2019 <- subset(TN2019, select=-applicant_race_3)
TN2019 <- subset(TN2019, select=-applicant_race_4)
TN2019 <- subset(TN2019, select=-applicant_race_5)
TN2019 <- subset(TN2019, select=-co_applicant_ethnicity_2)
TN2019 <- subset(TN2019, select=-co_applicant_ethnicity_3)
TN2019 <- subset(TN2019, select=-co_applicant_ethnicity_4)
TN2019 <- subset(TN2019, select=-co_applicant_ethnicity_5)
TN2019 <- subset(TN2019, select=-applicant_ethnicity_2)
TN2019 <- subset(TN2019, select=-applicant_ethnicity_3)
TN2019 <- subset(TN2019, select=-applicant_ethnicity_4)
TN2019 <- subset(TN2019, select=-applicant_ethnicity_5)

##############################################################
# remove rows with NAs 
##############################################################
TN2019 <- subset(TN2019, co_applicant_race_1!='')
TN2019 <- subset(TN2019, applicant_race_1!='')
TN2019 <- subset(TN2019, co_applicant_ethnicity_1!='')
TN2019 <- subset(TN2019, applicant_ethnicity_1!='')

# 'Not applicable' values in race, ethnicity, sex columns indicate 
# purchaser is not a 'natural person' or the info was not reported by
# the loan originator, cannot use these for bias analysis
TN2019 <- subset(TN2019, applicant_ethnicity_1 != 4)
TN2019 <- subset(TN2019, Applicant_Sex != 'Not applicable')  # code = 4
TN2019 <- subset(TN2019, applicant_race_1 != 7)

# remove rows with NAs for county and census tract
TN2019 <- subset(TN2019, county_code!='')
TN2019 <- subset(TN2019, census_tract!='') 



####################################################################
# convert levels with numeric representation to descriptives
####################################################################

# convert purchaser_type levels from numbers to descriptives
convert_purchaser <- function(purchaser_type){
  if (purchaser_type == 0){  return('Not applicable')  }
  else if(purchaser_type == 1){  return('Fannie Mae')  }
  else if (purchaser_type == 2){  return('Ginnie Mae')  }
  else if (purchaser_type == 3){  return('Freddie Mac')  }
  else if (purchaser_type == 4){  return('Farmer Mac')  }
  else if (purchaser_type == 5){  return('Private securitizer')  }
  else if (purchaser_type == 6){  return('Commercial or savings bank or savings association')  }
  else if (purchaser_type == 71){  return('Credit union mortgage company or finance company')  }
  else if (purchaser_type == 72){  return('Life insurance company')  }
  else if (purchaser_type == 8){  return('Affiliate institution')  }
  else if (purchaser_type == 9){  return('Other type of purchaser')  }
}
TN2019$Purchaser_Type <- sapply(TN2019$purchaser_type,convert_purchaser)
TN2019$Purchaser_Type<- as.factor(TN2019$Purchaser_Type)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -purchaser_type)

##########################################################################
# convert loan_purpose levels from numbers to descriptives
convert_loan_purpose <- function(loan_purpose){
  if (loan_purpose == 1){  return('Home purchase')  }
  else if (loan_purpose == 2){  return('Home improvement')  }
  else if (loan_purpose == 31){  return('Refinancing')  }
  else if (loan_purpose == 32){  return('Cash out refinancing')  }
  else if (loan_purpose == 4){ return('Other purpose') }
  else if (loan_purpose == 5){ return('Not applicable') }
}
TN2019$Loan_Purpose <- sapply(TN2019$loan_purpose,convert_loan_purpose)
TN2019$Loan_Purpose <- as.factor(TN2019$Loan_Purpose)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -loan_purpose)

##########################################################################
# convert preapproval levels from numbers to descriptives
convert_preapproval<- function(preapproval){
  if (preapproval== 1){  return('Preapproval requested')  }
  else if (preapproval  == 2){  return('Preapproval not requested')  }
}
TN2019$Preapproval <- sapply(TN2019$preapproval,convert_preapproval)
TN2019$Preapproval <- as.factor(TN2019$Preapproval)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -preapproval)

##########################################################################
# convert reverse_mortgage levels from numbers to descriptives
convert_reverse_mortgage <- function(reverse_mortgage){
  if (reverse_mortgage == 1){  return('Reverse Mortgage')  }
  else if (reverse_mortgage == 2){  return('Not a reverse mortgage')  }
  else if (reverse_mortgage == 1111){  return('Exempt')  }
}
TN2019$Reverse_Mortgage <- sapply(TN2019$reverse_mortgage,convert_reverse_mortgage)
TN2019$Reverse_Mortgage <- as.factor(TN2019$Reverse_Mortgage)
# drop reverse_mortgage (lower case) column 
TN2019 <- subset(TN2019, select= -reverse_mortgage)

##########################################################################
# convert open_end_line_of_credit levels from numbers to descriptives
convert_open_LOC <- function(open_end_line_of_credit){
  if (open_end_line_of_credit == 1){  return('Open-end line of credit')  }
  else if (open_end_line_of_credit == 2){  return('Not an open-end line of credit')  }
  else if (open_end_line_of_credit == 1111){  return('Exempt')  }
}
TN2019$Open_End_Line_of_Credit <- sapply(TN2019$open_end_line_of_credit,convert_open_LOC)
TN2019$Open_End_Line_of_Credit <- as.factor(TN2019$Open_End_Line_of_Credit)
# drop open_end_line_of_credit (lower case) column 
TN2019 <- subset(TN2019, select= -open_end_line_of_credit)

##########################################################################
# convert business_or_commercial_purpose levels from numbers to descriptives
convert_business_purpose <- function(business_or_commercial_purpose){
  if (business_or_commercial_purpose == 1){  return('Primarily for a business or commercial purpose')  }
  else if (business_or_commercial_purpose == 2){  return('Not primarily for a business or commercial purpose')  }
  else if (business_or_commercial_purpose == 1111){  return('Exempt')  }
}
TN2019$Business_or_Commmercial_Purpose <- sapply(TN2019$business_or_commercial_purpose,convert_business_purpose)
TN2019$Business_or_Commmercial_Purpose <- as.factor(TN2019$Business_or_Commmercial_Purpose)
# drop business_or_commercial_purpose (lower case) column 
TN2019 <- subset(TN2019, select= -business_or_commercial_purpose)

##########################################################################
# convert hoepa_status levels from numbers to descriptives
convert_hoepa_status <- function(hoepa_status){
  if (hoepa_status == 1){  return('High-cost mortgage')  }
  else if (hoepa_status == 2){  return('Not a high-cost mortgage')  }
  else if (hoepa_status == 3){  return('Not applicable')  }
}
TN2019$Hoepa_Status <- sapply(TN2019$hoepa_status,convert_hoepa_status)
TN2019$Hoepa_Status <- as.factor(TN2019$Hoepa_Status)
# drop hoepa_status (lower case) column 
TN2019 <- subset(TN2019, select= -hoepa_status)

##########################################################################
# convert negative_amortization levels from numbers to descriptives
convert_negative_amortization <- function(negative_amortization){
  if (negative_amortization == 1){  return('Negative amortization')  }
  else if (negative_amortization == 2){  return('No negative amortization')  }
  else if (negative_amortization == 1111){  return('Exempt')  }
}
TN2019$Negative_Amortization <- sapply(TN2019$negative_amortization,convert_negative_amortization)
TN2019$Negative_Amortization <- as.factor(TN2019$Negative_Amortization)
# drop negative_amortization (lower case) column 
TN2019 <- subset(TN2019, select= -negative_amortization)

##########################################################################
# convert interest_only_payment levels from numbers to descriptives
convert_interest_only_payment <- function(interest_only_payment){
  if (interest_only_payment == 1){  return('Interest-only payments')  }
  else if (interest_only_payment == 2){  return('No interest-only payments')  }
  else if (interest_only_payment == 1111){  return('Exempt')  }
}
TN2019$Interest_Only_Payment <- sapply(TN2019$interest_only_payment,convert_interest_only_payment)
TN2019$Interest_Only_Payment <- as.factor(TN2019$Interest_Only_Payment)
# drop interest_only_payment (lower case) column 
TN2019 <- subset(TN2019, select= -interest_only_payment)

##########################################################################
# convert balloon_payment levels from numbers to descriptives
convert_balloon_payment <- function(balloon_payment){
  if (balloon_payment == 1){  return('Balloon payment')  }
  else if (balloon_payment == 2){  return('Not a balloon payment')  }
  else if (balloon_payment == 1111){ return('Exempt')  }
}
TN2019$Balloon_Payment <- sapply(TN2019$balloon_payment,convert_balloon_payment)
TN2019$Balloon_Payment <- as.factor(TN2019$Balloon_Payment)
# drop balloon_payment (lower case) column 
TN2019 <- subset(TN2019, select= -balloon_payment)

##########################################################################
# convert other_nonamortizing_features levels from numbers to descriptives
convert_other_nonamortizing_features <- function(other_nonamortizing_features){
  if (other_nonamortizing_features == 1){  return('Other non-fully amortizing features')  }
  else if (other_nonamortizing_features == 2){  return('No other non-fully amortizing features')  }
  else if (other_nonamortizing_features == 1111){ return('Exempt')  }
}
TN2019$Other_Nonamortizing_Features <- sapply(TN2019$other_nonamortizing_features,convert_other_nonamortizing_features)
TN2019$Other_Nonamortizing_Features <- as.factor(TN2019$Other_Nonamortizing_Features)
# drop other_nonamortizing_features (lower case) column 
TN2019 <- subset(TN2019, select= -other_nonamortizing_features)

##########################################################################
# convert applicant_sex levels from numbers to descriptives
convert_applicant_sex <- function(applicant_sex){
  if (applicant_sex == 1){  return('Male')  }
  else if (applicant_sex == 2){  return('Female')  }
  else if (applicant_sex == 3){  return('Information not provided by applicant')  }
  else if (applicant_sex == 4){  return('Not applicable')  }
  else if (applicant_sex == 6){  return('Applicant selected both male and female')  }
}
TN2019$Applicant_Sex <- sapply(TN2019$applicant_sex,convert_applicant_sex)
TN2019$Applicant_Sex <- as.factor(TN2019$Applicant_Sex)
# drop applicant_sex (lower case) column 
TN2019 <- subset(TN2019, select= -applicant_sex)

##########################################################################
# convert co_applicant_sex levels from numbers to descriptives
convert_co_applicant_sex <- function(co_applicant_sex){
  if (co_applicant_sex == 1){  return('Male')  }
  else if (co_applicant_sex == 2){  return('Female')  }
  else if (co_applicant_sex == 3){  return('Information not provided by applicant')  }
  else if (co_applicant_sex == 4){  return('Not applicable')  }
  else if (co_applicant_sex == 5){  return('No co-applicant')  }
  else if (co_applicant_sex == 6){  return('Applicant selected both male and female')  }
}
TN2019$Co_Applicant_Sex <- sapply(TN2019$co_applicant_sex,convert_co_applicant_sex)
TN2019$Co_Applicant_Sex <- as.factor(TN2019$Co_Applicant_Sex)
# drop co_applicant_sex (lower case) column 
TN2019 <- subset(TN2019, select= -co_applicant_sex)

##########################################################################
# convert applicant_sex_observed levels from numbers to descriptives
convert_applicant_sex_observed <- function(applicant_sex_observed){
  if (applicant_sex_observed == 1){  return('Collected on the basis of visual observation or surname')  }
  else if (applicant_sex_observed == 2){  return('Not collected on the basis of visual observation or surname')  }
  else if (applicant_sex_observed == 3){  return('Not applicable')  }
}
TN2019$Applicant_Sex_Observed <- sapply(TN2019$applicant_sex_observed,convert_applicant_sex)
TN2019$Applicant_Sex_Observed <- as.factor(TN2019$Applicant_Sex_Observed)
# drop applicant_sex_observed (lower case) column 
TN2019 <- subset(TN2019, select= -applicant_sex_observed)

##########################################################################
# convert co_applicant_sex_observed levels from numbers to descriptives
convert_co_applicant_sex_observed <- function(co_applicant_sex_observed){
  if (co_applicant_sex_observed == 1){  return('Collected on the basis of visual observation or surname')  }
  else if (co_applicant_sex_observed == 2){  return('Not collected on the basis of visual observation or surname')  }
  else if (co_applicant_sex_observed == 3){  return('Not applicable')  }
  else if (co_applicant_sex_observed == 4){  return('No co-applicant')  }
}
TN2019$Co_Applicant_Sex_Observed <- sapply(TN2019$co_applicant_sex_observed,convert_co_applicant_sex)
TN2019$Co_Applicant_Sex_Observed <- as.factor(TN2019$Co_Applicant_Sex_Observed)
# drop co_applicant_sex_observed (lower case) column 
TN2019 <- subset(TN2019, select= -co_applicant_sex_observed)

##########################################################################
# convert occupancy_type levels from numbers to descriptives
convert_occupancy_type <- function(occupancy_type){
  if (occupancy_type == 1){  return('Principal residence')  }
  else if (occupancy_type == 2){  return('Second residence')  }
  else if (occupancy_type == 3){  return('Investment property')  }
}
TN2019$Occupancy_Type <- sapply(TN2019$occupancy_type,convert_occupancy_type)
TN2019$Occupancy_Type <- as.factor(TN2019$Occupancy_Type)
# drop occupancy_type (lower case) column 
TN2019 <- subset(TN2019, select= -occupancy_type)

##########################################################################
# convert construction_method levels from numbers to descriptives
convert_construction_method <- function(construction_method){
  if (construction_method == 1){  return('Site-built')  }
  else if (construction_method == 2){  return('Manufactured home')  }
}
TN2019$Construction_Method <- sapply(TN2019$construction_method,convert_construction_method)
TN2019$Construction_Method <- as.factor(TN2019$Construction_Method)
# drop construction_method (lower case) column 
TN2019 <- subset(TN2019, select= -construction_method)

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
TN2019$Action_Taken_Description <- sapply(TN2019$action_taken,convert_action_taken_descr)
TN2019$Action_Taken_Description <- as.factor(TN2019$Action_Taken_Description)

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
TN2019$Application_Status <- sapply(TN2019$action_taken,convert_application_status)
TN2019$Application_Status <- as.factor(TN2019$Application_Status)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -action_taken)

##########################################################################
# convert loan_type levels from numbers to descriptives
convert_loan_type<- function(loan_type){
  if (loan_type == 1){  return('Conventional (not insured or guaranteed by FHA, VA, RHS, or FSA)')  }
  else if (loan_type == 2){  return('Federal Housing Administration insured (FHA)')  }
  else if (loan_type == 3){  return('Veterans Affairs guaranteed (VA)')  }
  else if (loan_type == 4){  return('USDA Rural Housing Service or Farm Service Agency guaranteed (RHS or FSA)')  }
}
TN2019$Loan_Type <- sapply(TN2019$loan_type,convert_loan_type)
TN2019$Loan_Type <- as.factor(TN2019$Loan_Type)
# drop loan_type (lower case) column 
TN2019 <- subset(TN2019, select= -loan_type)

##########################################################################
# convert lien_status levels from numbers to descriptives
convert_lien_status<- function(lien_status){
  if (lien_status == 1){  return('Secured by a first lien')  }
  else if (lien_status == 2){  return('Secured by a subordinate lien')  }
}
TN2019$Lien_Status <- sapply(TN2019$lien_status,convert_lien_status)
TN2019$Lien_Status <- as.factor(TN2019$Lien_Status)
# drop lien_status (lower case) column 
TN2019 <- subset(TN2019, select= -lien_status)


# TN Data Cleaning Part 1
# Begining info: row count = 389728, col count = 99

# Get info about the TN Dataset
library(Hmisc)
describe(X2019publicTN_allColumns)

# remove columns that have info we don't need
TN2019_init <- subset(X2019publicTN_allColumns, select=-activity_year)
TN2019_init <- subset(TN2019_init, select=-state_code)

# remove columns with duplicated age information
# use applicant_age and co_applicant_age columns
TN2019_init <- subset(TN2019_init, select=-applicant_age_above_62)
TN2019_init <- subset(TN2019_init, select=-co_applicant_age_above_62)

# remove columns that are covered in 'derived' columns that will be used in the analysis
# derived_loan_product_type
TN2019_init <- subset(TN2019_init, select=-loan_type)
TN2019_init <- subset(TN2019_init, select=-lien_status)
# derived_dwelling_category
TN2019_init <- subset(TN2019_init, select=-construction_method)
TN2019_init <- subset(TN2019_init, select=-total_units)
# derived_msa_md
TN2019_init <- subset(TN2019_init, select=-county_code)
TN2019_init <- subset(TN2019_init, select=-census_tract)

# remove columns with low data density or no data 
TN2019_init <- subset(TN2019_init, select=-total_points_and_fees)
TN2019_init <- subset(TN2019_init, select=-discount_points)
TN2019_init <- subset(TN2019_init, select=-lender_credits)
TN2019_init <- subset(TN2019_init, select=-prepayment_penalty_term)
TN2019_init <- subset(TN2019_init, select=-intro_rate_period)
TN2019_init <- subset(TN2019_init, select=-multifamily_affordable_units)
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_2)
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_3)
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_4)
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_5)
TN2019_init <- subset(TN2019_init, select=-applicant_race_2)
TN2019_init <- subset(TN2019_init, select=-applicant_race_3)
TN2019_init <- subset(TN2019_init, select=-applicant_race_4)
TN2019_init <- subset(TN2019_init, select=-applicant_race_5)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_2)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_3)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_4)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_5)
TN2019_init <- subset(TN2019_init, select=-applicant_ethnicity_2)
TN2019_init <- subset(TN2019_init, select=-applicant_ethnicity_3)
TN2019_init <- subset(TN2019_init, select=-applicant_ethnicity_4)
TN2019_init <- subset(TN2019_init, select=-applicant_ethnicity_5)
TN2019_init <- subset(TN2019_init, select=-aus_2)
TN2019_init <- subset(TN2019_init, select=-aus_3)
TN2019_init <- subset(TN2019_init, select=-aus_4)
TN2019_init <- subset(TN2019_init, select=-aus_5)
TN2019_init <- subset(TN2019_init, select=-denial_reason_2)
TN2019_init <- subset(TN2019_init, select=-denial_reason_3)
TN2019_init <- subset(TN2019_init, select=-denial_reason_4)

# remove columns with 'observed' data - not relevant info
TN2019_init <- subset(TN2019_init, select=-applicant_ethnicity_observed)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_observed)
TN2019_init <- subset(TN2019_init, select=-applicant_race_observed)
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_observed)
TN2019_init <- subset(TN2019_init, select=-applicant_sex_observed)
TN2019_init <- subset(TN2019_init, select=-co_applicant_sex_observed)

# remove other columns not relevant to this analysis
TN2019_init <- subset(TN2019_init, select=-conforming_loan_limit)
TN2019_init <- subset(TN2019_init, select=-lei)

describe(TN2019_init)
# remove remaining co_applicant columns
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_1)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_1)
TN2019_init <- subset(TN2019_init, select=-co_applicant_age)
TN2019_init <- subset(TN2019_init, select=-co_applicant_sex)
TN2019_init <- subset(TN2019_init, select=-co_applicant_credit_score_type)

# row count = 389728, col count = 47
contents(TN2019_init)

# remove rows for demographics with missing values
TN2019_init <- subset(TN2019_init, applicant_race_1!='')
TN2019_init <- subset(TN2019_init, applicant_ethnicity_1!='')

# 'Not applicable' values in race, ethnicity, sex columns indicate  
# purchaser is not a 'natural person' or the info was not reported by
# the loan originator, cannot use these for bias analysis
TN2019_init <- subset(TN2019_init, applicant_ethnicity_1 != 4)
TN2019_init <- subset(TN2019_init, applicant_sex != 4)  
TN2019_init <- subset(TN2019_init, applicant_race_1 != 7)
TN2019_init <- subset(TN2019_init, applicant_age!='8888')

# remove rows in race, ethnicity, sex info columns 
# with codes that indicate information was not provided by the applicant
TN2019_init <- subset(TN2019_init, applicant_ethnicity_1 != 3)
TN2019_init <- subset(TN2019_init, applicant_sex != 3)  
TN2019_init <- subset(TN2019_init, applicant_race_1 != 6)



####################################################################
# convert levels with numeric representation to descriptives
####################################################################
# convert applicant_sex levels from numbers to descriptives
convert_applicant_sex <- function(applicant_sex){
  if (applicant_sex == 1){  return('Male')  }
  else if (applicant_sex == 2){  return('Female')  }
  else if (applicant_sex == 3){  return('Information not provided by applicant')  }
  else if (applicant_sex == 4){  return('Not applicable')  }
  else if (applicant_sex == 6){  return('Applicant selected both male and female')  }
}
TN2019_init$Applicant_Sex <- sapply(TN2019_init$applicant_sex,convert_applicant_sex)
TN2019_init$Applicant_Sex <- as.factor(TN2019_init$Applicant_Sex)
# drop applicant_sex (lower case) column 
TN2019_init <- subset(TN2019_init, select= -applicant_sex)



##########################################################################
# convert applicant_ethnicity_1 levels from numbers to descriptives
convert_applicant_ethnicity_1<- function(applicant_ethnicity_1){
  if (applicant_ethnicity_1 == 1){  return('Hispanic or Latino')  }
  else if (applicant_ethnicity_1 == 11){  return('Mexican')  }
  else if (applicant_ethnicity_1 == 12){  return('Puerto Rican')  }
  else if (applicant_ethnicity_1 == 13){  return('Cuban')  }
  else if (applicant_ethnicity_1 == 14){  return('Other Hispanic or Latino')  }
  else if (applicant_ethnicity_1 == 2){  return('Not Hispanic or Latino')  }
  else if (applicant_ethnicity_1 == 3){  return('Information not provided by applicant in mail, internet, or telephone application')  }
  else if (applicant_ethnicity_1 == 4){  return('Not applicable')  }
}
TN2019_init$Applicant_Ethnicity_1 <- sapply(TN2019_init$applicant_ethnicity_1,convert_applicant_ethnicity_1)
TN2019_init$Applicant_Ethnicity_1 <- as.factor(TN2019_init$Applicant_Ethnicity_1)
# drop applicant_ethnicity_1 (lower case) column 
TN2019_init <- subset(TN2019_init, select= -applicant_ethnicity_1)

##########################################################################
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
  else if (applicant_race_1 == 6){  return('Information not provided by applicant in mail, internet, or telephone application')  }
  else if (applicant_race_1 == 7){  return('Not applicable')  }
}
TN2019_init$Applicant_Race_1 <- sapply(TN2019_init$applicant_race_1,convert_applicant_race_1)
TN2019_init$Applicant_Race_1 <- as.factor(TN2019_init$Applicant_Race_1)
# drop applicant_race_1 (lower case) column 
TN2019_init <- subset(TN2019_init, select= -applicant_race_1)

#########################################################################
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
TN2019_init$Purchaser_Type <- sapply(TN2019_init$purchaser_type,convert_purchaser)
TN2019_init$Purchaser_Type<- as.factor(TN2019_init$Purchaser_Type)
# drop purchaser_type (lower case) column 
TN2019_init <- subset(TN2019_init, select= -purchaser_type)

# remove all rows where purchaser_type != 'Not applicable' 
# see 2019guide.pdf pg.61 for reference
TN2019_init <- subset(TN2019_init, Purchaser_Type == 'Not applicable' )

#######################################################################################
# map action_taken levels to application_status
convert_action_taken <- function(action_taken){
  if (action_taken == 1){  return('Loan originated')  }
  else if (action_taken == 2){  return('Application approved but not accepted')  }
  else if (action_taken == 3){  return('Application denied')  }
  else if (action_taken == 4){  return('Application withdrawn by applicant')  }
  else if (action_taken == 5){  return('File closed for incompleteness')  }
  else if (action_taken == 6){  return('Purchased loan')  }
  else if (action_taken == 7){  return('Preapproval request denied')  }
  else if (action_taken == 8){  return('Preapproval request approved but not accepted')  }
}
TN2019_init$Action_Taken <- sapply(TN2019_init$action_taken,convert_action_taken)
TN2019_init$Action_Taken <- as.factor(TN2019_init$Action_Taken)
# drop action_taken (lower case) column 
TN2019_init <- subset(TN2019_init, select= -action_taken)

# remove rows with action_taken that does not include an approval or denial 
TN2019_init <- subset(TN2019_init, Action_Taken!='Purchased loan')
TN2019_init <- subset(TN2019_init, Action_Taken!='Application withdawn by applicant')
TN2019_init <- subset(TN2019_init, Action_Taken!='File closed for incompleteness')
# remove unused levels from the factor column
TN2019_init$Action_Taken <- factor(TN2019_init$Action_Taken)

####################################################################################
# convert business_or_commercial_purpose levels from numbers to descriptives
convert_business_purpose <- function(business_or_commercial_purpose){
  if (business_or_commercial_purpose == 1){  return('Primarily for a business or commercial purpose')  }
  else if (business_or_commercial_purpose == 2){  return('Not primarily for a business or commercial purpose')  }
  else if (business_or_commercial_purpose == 1111){  return('Exempt')  }
}
TN2019_init$Business_or_Commercial_Purpose <- sapply(TN2019_init$business_or_commercial_purpose,convert_business_purpose)
TN2019_init$Business_or_Commercial_Purpose <- as.factor(TN2019_init$Business_or_Commercial_Purpose)
# drop business_or_commercial_purpose (lower case) column 
TN2019_init <- subset(TN2019_init, select= -business_or_commercial_purpose)

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
# convert manufactured_home_secured_property_type levels from numbers to descriptives
convert_manufactured_home_prop_type<- function(manufactured_home_secured_property_type){
  if (manufactured_home_secured_property_type == 1){  return('Manufactured home and land')  }
  else if (manufactured_home_secured_property_type == 2){  return('Manufactured home and not land')  }
  else if (manufactured_home_secured_property_type == 3){  return('Not applicable')  }
  else if (manufactured_home_secured_property_type == 1111){  return('Exempt')  }
}
TN2019_init$Manufactured_Home_Secured_Property_Type <- sapply(TN2019_init$manufactured_home_secured_property_type,convert_manufactured_home_prop_type)
TN2019_init$Manufactured_Home_Secured_Property_Type <- as.factor(TN2019_init$Manufactured_Home_Secured_Property_Type)
# drop manufactured_home_secured_property_type (lower case) column 
TN2019_init <- subset(TN2019_init, select= -manufactured_home_secured_property_type)

##########################################################################
# convert manufactured_home_land_property_interest levels from numbers to descriptives
convert_manufactured_home_land_prop_interest<- function(manufactured_home_land_property_interest){
  if (manufactured_home_land_property_interest == 1){  return('Direct ownership')  }
  else if (manufactured_home_land_property_interest == 2){  return('Indirect ownership')  }
  else if (manufactured_home_land_property_interest == 3){  return('Paid leasehold')  }
  else if (manufactured_home_land_property_interest == 4){  return('Unpaid leasehold')  }
  else if (manufactured_home_land_property_interest == 5){  return('Not applicable')  }
  else if (manufactured_home_land_property_interest == 1111){  return('Exempt')  }
}
TN2019_init$Manufactured_Home_Land_Property_Interest <- sapply(TN2019_init$manufactured_home_land_property_interest,convert_manufactured_home_land_prop_interest)
TN2019_init$Manufactured_Home_Land_Property_Interest <- as.factor(TN2019_init$Manufactured_Home_Land_Property_Interest)
# drop manufactured_home_land_property_interest (lower case) column 
TN2019_init <- subset(TN2019_init, select= -manufactured_home_land_property_interest)

##########################################################################
# convert other_nonamortizing_features levels from numbers to descriptives
convert_other_nonamortizing_features <- function(other_nonamortizing_features){
  if (other_nonamortizing_features == 1){  return('Other non-fully amortizing features')  }
  else if (other_nonamortizing_features == 2){  return('No other non-fully amortizing features')  }
  else if (other_nonamortizing_features == 1111){ return('Exempt')  }
}
TN2019_init$Other_Nonamortizing_Features <- sapply(TN2019_init$other_nonamortizing_features,convert_other_nonamortizing_features)
TN2019_init$Other_Nonamortizing_Features <- as.factor(TN2019_init$Other_Nonamortizing_Features)
# drop other_nonamortizing_features (lower case) column 
TN2019_init <- subset(TN2019_init, select= -other_nonamortizing_features)

##########################################################################
# convert submission_of_application levels from numbers to descriptives
convert_submission_of_application <- function(submission_of_application){
  if (submission_of_application == 1){  return('Submitted directly to your institution')  }
  else if (submission_of_application == 2){  return('Not submitted directly to your institution')  }
  else if (submission_of_application == 3){  return('Not applicable')  }
  else if (submission_of_application == 1111){  return('Exempt')  }
}
TN2019_init$Submission_Of_Application <- sapply(TN2019_init$submission_of_application,convert_submission_of_application)
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
TN2019_init$Initially_Payable_To_Institution <- sapply(TN2019_init$initially_payable_to_institution,convert_initially_payable_to_institution)
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
TN2019_init$AUS_1 <- sapply(TN2019_init$aus_1,convert_aus_1)
TN2019_init$AUS_1 <- as.factor(TN2019_init$AUS_1)
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
TN2019_init$Denial_Reason_1 <- sapply(TN2019_init$denial_reason_1,convert_denial_reason_1)
TN2019_init$Denial_Reason_1 <- as.factor(TN2019_init$Denial_Reason_1)
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
TN2019_init$Applicant_Credit_Score_Type <- sapply(TN2019_init$applicant_credit_score_type,convert_applicant_credit_score_type)
TN2019_init$Applicant_Credit_Score_Type <- as.factor(TN2019_init$Applicant_Credit_Score_Type)
# drop applicant_credit_score_type (lower case) column 
TN2019_init <- subset(TN2019_init, select= -applicant_credit_score_type)



describe(TN2019_init)
# remove Purchaser_Type column, all values are the same in the column
TN2019_init <- subset(TN2019_init, select=-Purchaser_Type)


TN2019_init <- subset(TN2019_init, Business_or_Commercial_Purpose == 'Not primarily for a business or commercial purpose')
TN2019_init <- subset(TN2019_init, select=-Business_or_Commercial_Purpose)
TN2019_init <- subset(TN2019_init, income !='')
TN2019_init <- subset(TN2019_init, income !='Exempt')
TN2019_init <- subset(TN2019_init, Occupancy_Type=='Principal residence')
TN2019_init <- subset(TN2019_init, select=-Occupancy_Type)
TN2019_init <- subset(TN2019_init, property_value!='Exempt')
################################################################################
TN2019_approved <- TN2019_init
TN2019_reg <- TN2019_init





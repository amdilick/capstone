# TN Data Cleaning
# Begining info: row count = 389728, col count = 99

# Get info about the TN Dataset
library(Hmisc)
describe(X2019publicTN_allColumns)

# remove columns that have info we don't need
TN2019 <- subset(X2019publicTN_allColumns, select=-activity_year)
TN2019 <- subset(TN2019, select=-state_code)
# col count = 97

# remove rows with action_taken in (4,5,6)
# action_taken = 4 (application withdrawn)
# action_taken = 5 (application closed as incomplete)
# action_taken = 6 (purchased loan, meaning an entity purchased the loan and this is not an individual borrower)
TN2019 <- subset(TN2019, action_taken!=4)
TN2019 <- subset(TN2019, action_taken!=5)
TN2019 <- subset(TN2019, action_taken!=6)
# row count = 273077

# remove rows with NAs for county and census tract
TN2019 <- subset(TN2019, county_code!='')
TN2019 <- subset(TN2019, census_tract!='') 
# row count = 268998

# remove columns with duplicated info for race, ethnicity, sex that is aggregated in 'derived_race', 'derived_ethnicity', 'derived_sex
TN2019 <- subset(TN2019, select=-applicant_ethnicity_1)
TN2019 <- subset(TN2019, select=-applicant_ethnicity_2)
TN2019 <- subset(TN2019, select=-applicant_ethnicity_3)
TN2019 <- subset(TN2019, select=-applicant_ethnicity_4)
TN2019 <- subset(TN2019, select=-applicant_ethnicity_5)
TN2019 <- subset(TN2019, select=-co_applicant_ethnicity_1)
TN2019 <- subset(TN2019, select=-co_applicant_ethnicity_2)
TN2019 <- subset(TN2019, select=-co_applicant_ethnicity_3)
TN2019 <- subset(TN2019, select=-co_applicant_ethnicity_4)
TN2019 <- subset(TN2019, select=-co_applicant_ethnicity_5)
TN2019 <- subset(TN2019, select=-applicant_race_1)
TN2019 <- subset(TN2019, select=-applicant_race_2)
TN2019 <- subset(TN2019, select=-applicant_race_3)
TN2019 <- subset(TN2019, select=-applicant_race_4)
TN2019 <- subset(TN2019, select=-applicant_race_5)
TN2019 <- subset(TN2019, select=-co_applicant_race_1)
TN2019 <- subset(TN2019, select=-co_applicant_race_2)
TN2019 <- subset(TN2019, select=-co_applicant_race_3)
TN2019 <- subset(TN2019, select=-co_applicant_race_4)
TN2019 <- subset(TN2019, select=-co_applicant_race_5)
TN2019 <- subset(TN2019, select=-applicant_sex)
TN2019 <- subset(TN2019, select=-co_applicant_sex)
# col count = 75

# remove columns with 'observed' information for race, ethnicity and sex
# these columns represent ...
TN2019 <- subset(TN2019, select=-applicant_ethnicity_observed)
TN2019 <- subset(TN2019, select=-co_applicant_ethnicity_observed)
TN2019 <- subset(TN2019, select=-applicant_race_observed)
TN2019 <- subset(TN2019, select=-co_applicant_race_observed)
TN2019 <- subset(TN2019, select=-applicant_sex_observed)
TN2019 <- subset(TN2019, select=-co_applicant_sex_observed)
# col count = 69

# remove columns with duplicated age information
TN2019 <- subset(TN2019, select=-applicant_age_above_62)
TN2019 <- subset(TN2019, select=-co_applicant_age_above_62)
# col count = 67

# remove columns denial_reason_X; analysis will use action_taken to reflect approved or denied status
TN2019 <- subset(TN2019, select=-denial_reason_1)
TN2019 <- subset(TN2019, select=-denial_reason_2)
TN2019 <- subset(TN2019, select=-denial_reason_3)
TN2019 <- subset(TN2019, select=-denial_reason_4)
# col count = 63

# remove low-information columns 
TN2019 <- subset(TN2019, select=-aus_2)
TN2019 <- subset(TN2019, select=-aus_3)
TN2019 <- subset(TN2019, select=-aus_4)
TN2019 <- subset(TN2019, select=-aus_5)
# col count = 59

# remove columns with duplicated loan and lien information 
TN2019 <- subset(TN2019, select=-loan_type)
TN2019 <- subset(TN2019, select=-lien_status)
# col count = 57

# remove columns with duplicated construction and units information
TN2019 <- subset(TN2019, select=-construction_method)
TN2019 <- subset(TN2019, select=-total_units)
# col count = 55

##########################################################################

# convert action_taken levels into 'approved' or 'denied' based on code
convert_action <- function(action_taken){
  if (action_taken == 1){  return('approved')  }
  else if(action_taken == 2){  return('approved')  }
  else if (action_taken == 8){  return('approved')  }
  else if (action_taken == 3){  return('denied')  }
  else if (action_taken == 7){ return('denied') }
}

TN2019$Action_Taken <- sapply(TN2019$action_taken,convert_action)
TN2019$Action_Taken <- as.factor(TN2019$Action_Taken)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -action_taken)


# convert purchaser_Type levels from numbers to descriptives
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


# convert preapproval levels from numbers to descriptives
convert_preapproval<- function(preapproval){
  if (preapproval== 1){  return('Preapproval requested')  }
  else if (preapproval  == 2){  return('Preapproval not requested')  }
 }

TN2019$Preapproval <- sapply(TN2019$preapproval,convert_preapproval)
TN2019$Preapproval <- as.factor(TN2019$Preapproval)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -preapproval)


# convert reverse_mortgage levels from numbers to descriptives
convert_reverse_mortgage <- function(reverse_mortgage){
  if (reverse_mortgage == 1){  return('Reverse Mortgage')  }
  else if (reverse_mortgage == 2){  return('Not a reverse mortgage')  }
  else if (reverse_mortgage == 1111){  return('Exempt')  }
}

TN2019$Reverse_Mortgage <- sapply(TN2019$reverse_mortgage,convert_reverse_mortgage)
TN2019$Reverse_Mortgage <- as.factor(TN2019$Reverse_Mortgage)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -reverse_mortgage)

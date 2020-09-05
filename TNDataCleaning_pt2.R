# TN Data Cleaning Part 2
# Demographics:  remove rows, convert numeric levels to descriptives
# only keep loans originated and not sold
# only keep approved/denied applications, not incomplete or withdrawn
# Beginning info: row count = 382444, col count = 68

library(Hmisc)
describe(TN2019_reg)

# remove columns w/ data for approved apps and NA in rows for denied apps
TN2019_reg <- subset(TN2019_reg, select=-combined_loan_to_value_ratio)
TN2019_reg <- subset(TN2019_reg, select=-interest_rate)
TN2019_reg <- subset(TN2019_reg, select=-rate_spread)
TN2019_reg <- subset(TN2019_reg, select=-total_loan_costs)
TN2019_reg <- subset(TN2019_reg, select=-origination_charges)
TN2019_reg <- subset(TN2019_reg, select=-debt_to_income_ratio)







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
TN2019$Applicant_Sex <- sapply(TN2019$applicant_sex,convert_applicant_sex)
TN2019$Applicant_Sex <- as.factor(TN2019$Applicant_Sex)
# drop applicant_sex (lower case) column 
TN2019 <- subset(TN2019, select= -applicant_sex)

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
TN2019$Applicant_Ethnicity_1 <- sapply(TN2019$applicant_ethnicity_1,convert_applicant_ethnicity_1)
TN2019$Applicant_Ethnicity_1 <- as.factor(TN2019$Applicant_Ethnicity_1)
# drop applicant_ethnicity_1 (lower case) column 
TN2019 <- subset(TN2019, select= -applicant_ethnicity_1)


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
TN2019$Applicant_Race_1 <- sapply(TN2019$applicant_race_1,convert_applicant_race_1)
TN2019$Applicant_Race_1 <- as.factor(TN2019$Applicant_Race_1)
# drop applicant_race_1 (lower case) column 
TN2019 <- subset(TN2019, select= -applicant_race_1)

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
TN2019$Purchaser_Type <- sapply(TN2019$purchaser_type,convert_purchaser)
TN2019$Purchaser_Type<- as.factor(TN2019$Purchaser_Type)
# drop purchaser_type (lower case) column 
TN2019 <- subset(TN2019, select= -purchaser_type)

# remove all rows where purchaser_type != 'Not applicable' 
# see 2019guide.pdf pg.61 for reference
TN2019 <- subset(TN2019, Purchaser_Type == 'Not applicable' )

f#######################################################################################

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
TN2019$Action_Taken <- sapply(TN2019$action_taken,convert_action_taken)
TN2019$Action_Taken <- as.factor(TN2019$Action_Taken)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -action_taken)


# remove rows with action_taken that does not include an approval or denial 
TN2019 <- subset(TN2019, Action_Taken!='Purchased loan')
TN2019 <- subset(TN2019, Action_Taken!='Application withdawn by applicant')
TN2019 <- subset(TN2019, Action_Taken!='File closed for incompleteness')
# removed unused levels from the factor column
TN2019$Action_Taken <- factor(TN2019$Action_Taken)


# remove Purchaser_Type column, all values are the same in the column
TN2019 <- subset(TN2019, select=-Purchaser_Type)

# Ending info: row count = 162454,  col count = 55
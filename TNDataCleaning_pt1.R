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


TN2019_init <- subset(TN2019_init, applicant_age!='8888')




####################################################################
# convert levels with numeric representation to descriptives
####################################################################








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





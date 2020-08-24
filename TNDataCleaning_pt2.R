# TN Data Cleaning Part 2
# Beginning info: row count = 382444, col count = 68

# remove rows with action_taken in (4,5,6)
# action_taken = 4 (application withdrawn)
# action_taken = 5 (application closed as incomplete)
# action_taken = 6 (purchased loan, meaning an entity purchased the loan)
TN2019 <- subset(TN2019, action_taken!=4)
TN2019 <- subset(TN2019, action_taken!=5)
TN2019 <- subset(TN2019, action_taken!=6)

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

####################################################################
####################################################################
# 'Not applicable' values in race, ethnicity, sex columns indicate 
# purchaser is not a 'natural person' or the info was not reported by
# the loan originator, cannot use these for bias analysis
TN2019 <- subset(TN2019, applicant_ethnicity_1 != 4)
TN2019 <- subset(TN2019, applicant_sex != 4)  
TN2019 <- subset(TN2019, applicant_race_1 != 7)
TN2019 <- subset(TN2019, applicant_age!='8888')
TN2019 <- subset(TN2019, co_applicant_age!='8888')

# remove rows in race, ethnicity, sex info columns 
# with codes that indicate information was not provided by the applicant
TN2019 <- subset(TN2019, applicant_ethnicity_1 != 3)
TN2019 <- subset(TN2019, applicant_sex != 3)  
TN2019 <- subset(TN2019, applicant_race_1 != 6)
TN2019 <- subset(TN2019, co_applicant_ethnicity_1 != 3)
TN2019 <- subset(TN2019, co_applicant_sex != 3)  
TN2019 <- subset(TN2019, co_applicant_race_1 != 6)

# Ending info: row count = 226911, col count = 68

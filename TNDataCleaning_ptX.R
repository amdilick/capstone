# TN Data Cleaning Part 4
# info: row count = 162454, col count = 55

# map action_taken levels to Application_Status
convert_application_status <- function(Action_Taken){
  if (Action_Taken == 'Loan originated'){  return('approved')  }
  else if (Action_Taken == 'Application approved but not accepted'){  return('approved')  }
  else if (Action_Taken == 'Application denied'){  return('denied')  }
  else if (Action_Taken == 'Application withdrawn by applicant'){  return('withdrawn')  }
  else if (Action_Taken == 'File closed for incompleteness'){  return('incomplete')  }
  else if (Action_Taken == 'Purchased loan'){  return('Purchased loan')  }
  else if (Action_Taken == 'Preapproval request denied'){  return('denied')  }
  else if (Action_Taken == 'Preapproval request approved but not accepted'){  return('approved')  }
}  
TN2019$Application_Status <- sapply(TN2019$Action_Taken,convert_application_status)
TN2019$Application_Status <- as.factor(TN2019$Application_Status)

# remove rows with action_taken in (4,5,6)
# action_taken = (application withdrawn)
# action_taken = (application closed as incomplete)
# action_taken = (purchased loan, meaning an entity purchased the loan)
TN2019 <- subset(TN2019, Application_Status!='withdrawn')
TN2019 <- subset(TN2019, Application_Status!='incomplete')
TN2019 <- subset(TN2019, Application_Status!='Purchased loan')

# remove rows where income is NA
TN2019 <- subset(TN2019, income!='')

# remove rows with 'Exempt' in property_value; not reporting info for many fields
TN2019 <- subset(TN2019, property_value!='Exempt')



# row count = 120843, col count = 53

###### regression subset
TN2019_reg <- subset(TN2019, select=c('Application_Status', 'derived_msa_md',
      'derived_loan_product_type', 'derived_dwelling_category', 'derived_ethnicity', 
      'derived_race', 'derived_sex', 'loan_amount', 'property_value', 'income',  
      'applicant_age',  'Applicant_Sex', 'Business_or_Commercial_Purpose',
      'Occupancy_Type', 'Loan_Purpose', 'Preapproval', 'AUS_1', 
      'Applicant_Credit_Score_Type'))






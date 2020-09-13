# TN Data Cleaning Part 4
# info: row count = 162454, col count = 55



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






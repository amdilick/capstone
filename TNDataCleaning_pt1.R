# TN Data Cleaning Part 1
# Begining info: row count = 389728, col count = 99

# Get info about the TN Dataset
library(Hmisc)
describe(X2019publicTN_allColumns)

# remove columns that have info we don't need
TN2019 <- subset(X2019publicTN_allColumns, select=-activity_year)
TN2019 <- subset(TN2019, select=-state_code)

# remove columns with duplicated age information
# use applicant_age and co_applicant_age columns
TN2019 <- subset(TN2019, select=-applicant_age_above_62)
TN2019 <- subset(TN2019, select=-co_applicant_age_above_62)

# remove columns that are covered in 'derived' columns
# derived_loan_product_type
TN2019 <- subset(TN2019, select=-loan_type)
TN2019 <- subset(TN2019, select=-lien_status)
# derived_dwelling_category
TN2019 <- subset(TN2019, select=-construction_method)
TN2019 <- subset(TN2019, select=-total_units)

# subset the demographics columns and view summary statistics
TN2019_gender <- subset(TN2019, select =c('derived_sex', 'applicant_sex', 
      'co_applicant_sex', 'applicant_sex_observed', 'co_applicant_sex_observed'))
describe(TN2019_gender)

TN2019_race <- subset(TN2019, select = c('derived_race', 'applicant_race_1',
      'applicant_race_2', 'applicant_race_3', 'applicant_race_4', 'applicant_race_5',
      'applicant_race_observed', 'co_applicant_race_1', 'co_applicant_race_2', 
      'co_applicant_race_3', 'co_applicant_race_4', 'co_applicant_race_5',
      'co_applicant_race_observed'))
describe(TN2019_race)

TN2019_ethnicity<- subset(TN2019, select =c('derived_ethnicity', 'applicant_ethnicity_1', 
      'applicant_ethnicity_2', 'applicant_ethnicity_3', 'applicant_ethnicity_4', 
      'applicant_ethnicity_5', 'applicant_ethnicity_observed', 'co_applicant_ethnicity_1', 
      'co_applicant_ethnicity_2', 'co_applicant_ethnicity_3', 'co_applicant_ethnicity_4', 
      'co_applicant_ethnicity_5', 'co_applicant_ethnicity_observed'))
describe(TN2019_ethnicity)

# remove columns for demographics with low data density or no data 
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

# remove remaining co_applicant columns
TN2019 <- subset(TN2019, select=-co_applicant_race_1)
TN2019 <- subset(TN2019, select=-co_applicant_ethnicity_1)
TN2019 <- subset(TN2019, select=-co_applicant_age)
TN2019 <- subset(TN2019, select=-co_applicant_sex)
TN2019 <- subset(TN2019, select=-co_applicant_credit_score_type)

# remove rows for demographics with missing values
TN2019 <- subset(TN2019, applicant_race_1!='')
TN2019 <- subset(TN2019, applicant_ethnicity_1!='')

# remove other columns with low data density
TN2019 <- subset(TN2019, select=-aus_2)
TN2019 <- subset(TN2019, select=-aus_3)
TN2019 <- subset(TN2019, select=-aus_4)
TN2019 <- subset(TN2019, select=-aus_5)
TN2019 <- subset(TN2019, select=-denial_reason_2)
TN2019 <- subset(TN2019, select=-denial_reason_3)
TN2019 <- subset(TN2019, select=-denial_reason_4)
TN2019 <- subset(TN2019, select=-multifamily_affordable_units)

# removed columns with data only relevant to approved and accepted applications
TN2019 <- subset(TN2019, select=-total_points_and_fees)
TN2019 <- subset(TN2019, select=-interest_rate)
TN2019 <- subset(TN2019, select=-rate_spread)
TN2019 <- subset(TN2019, select=-total_loan_costs)
TN2019 <- subset(TN2019, select=-origination_charges)
TN2019 <- subset(TN2019, select=-discount_points)
TN2019 <- subset(TN2019, select=-lender_credits)
TN2019 <- subset(TN2019, select=-intro_rate_period)
TN2019 <- subset(TN2019, select=-prepayment_penalty_term)
                                      
# remove columns with 'observed' data - not relevant info
TN2019 <- subset(TN2019, select=-applicant_ethnicity_observed)
TN2019 <- subset(TN2019, select=-co_applicant_ethnicity_observed)
TN2019 <- subset(TN2019, select=-applicant_race_observed)
TN2019 <- subset(TN2019, select=-co_applicant_race_observed)
TN2019 <- subset(TN2019, select=-applicant_sex_observed)
TN2019 <- subset(TN2019, select=-co_applicant_sex_observed)


# remove rows with NAs for county and census tract
TN2019 <- subset(TN2019, county_code!='')
TN2019 <- subset(TN2019, census_tract!='') 

# Ending info: row count = 382460, col count = 47


# TN Data Cleaning
# Begining info: row count = 389728, col count = 99

# Get info about the TN Dataset
library(Hmisc)
describe(X2019publicTN_allColumns)

# remove columns that have info we don't need
TN2019 <- subset(X2019publicTN_allColumns, select=-activity_year)
TN2019 <- subset(TN2019, select=-state_code)
# col count = 97

##############################################################
# remove columns that have data derived from multiple columns
##############################################################
# remove columns with duplicated info for race, ethnicity, sex 
# that is derived 'derived_race', 'derived_ethnicity', 'derived_sex'
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
#TN2019 <- subset(TN2019, select=-applicant_sex)
#TN2019 <- subset(TN2019, select=-co_applicant_sex)

##############################################################
# remove columns that have data aggregated from 2 columns
##############################################################
# remove columns with loan and lien information 
# combined into derived_loan_product_type
TN2019 <- subset(TN2019, select=-loan_type)
TN2019 <- subset(TN2019, select=-lien_status)

# remove columns with construction and units information
# combined into derived_dwelling_category
TN2019 <- subset(TN2019, select=-construction_method)
TN2019 <- subset(TN2019, select=-total_units)

##############################################################
# remove columns that have duplicated information
##############################################################
# remove columns with duplicated age information
TN2019 <- subset(TN2019, select=-applicant_age_above_62)
TN2019 <- subset(TN2019, select=-co_applicant_age_above_62)

# remove columns with 'observed' information for race, ethnicity and sex
# these columns represent how the race, ethnicity and sex information was
# collected or determined at the time of the application 
TN2019 <- subset(TN2019, select=-applicant_ethnicity_observed)
TN2019 <- subset(TN2019, select=-co_applicant_ethnicity_observed)
TN2019 <- subset(TN2019, select=-applicant_race_observed)
TN2019 <- subset(TN2019, select=-co_applicant_race_observed)
TN2019 <- subset(TN2019, select=-applicant_sex_observed)
TN2019 <- subset(TN2019, select=-co_applicant_sex_observed)

# remove columns denial_reason_X; analysis will use action_taken to reflect approved or denied status
TN2019 <- subset(TN2019, select=-denial_reason_1)
TN2019 <- subset(TN2019, select=-denial_reason_2)
TN2019 <- subset(TN2019, select=-denial_reason_3)
TN2019 <- subset(TN2019, select=-denial_reason_4)

##############################################################
# remove columns with low data density
##############################################################
# remove low-information columns 
TN2019 <- subset(TN2019, select=-aus_2)
TN2019 <- subset(TN2019, select=-aus_3)
TN2019 <- subset(TN2019, select=-aus_4)
TN2019 <- subset(TN2019, select=-aus_5)

##############################################################
# remove rows 
##############################################################

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

# remove rows for derived_sex = 'Not available' 
# 'Not available' indicates that the applicant is not a natural person (i.e. business), or was not reported by the applicant
TN2019 <- subset(TN2019, derived_sex != 'Not applicable')

# Check for derived_ethnicity and derived_race also

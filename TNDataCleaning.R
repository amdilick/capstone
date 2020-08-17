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
#col_count = 67




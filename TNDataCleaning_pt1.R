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

# remove columns that are covered in 'derived' columns that will be used in the analysis
# derived_loan_product_type
TN2019 <- subset(TN2019, select=-loan_type)
TN2019 <- subset(TN2019, select=-lien_status)
# derived_dwelling_category
TN2019 <- subset(TN2019, select=-construction_method)
TN2019 <- subset(TN2019, select=-total_units)
# derived_msa_md
TN2019 <- subset(TN2019, select=-county_code)
TN2019 <- subset(TN2019, select=-census_tract)

# remove columns with low data density or no data 
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
TN2019 <- subset(TN2019, select=-aus_2)
TN2019 <- subset(TN2019, select=-aus_3)
TN2019 <- subset(TN2019, select=-aus_4)
TN2019 <- subset(TN2019, select=-aus_5)
TN2019 <- subset(TN2019, select=-denial_reason_2)
TN2019 <- subset(TN2019, select=-denial_reason_3)
TN2019 <- subset(TN2019, select=-denial_reason_4)
TN2019 <- subset(TN2019, select=-multifamily_affordable_units)

# remove remaining co_applicant columns
TN2019 <- subset(TN2019, select=-co_applicant_race_1)
TN2019 <- subset(TN2019, select=-co_applicant_ethnicity_1)
TN2019 <- subset(TN2019, select=-co_applicant_age)
TN2019 <- subset(TN2019, select=-co_applicant_sex)
TN2019 <- subset(TN2019, select=-co_applicant_credit_score_type)

# remove columns with 'observed' data - not relevant info
TN2019 <- subset(TN2019, select=-applicant_ethnicity_observed)
TN2019 <- subset(TN2019, select=-co_applicant_ethnicity_observed)
TN2019 <- subset(TN2019, select=-applicant_race_observed)
TN2019 <- subset(TN2019, select=-co_applicant_race_observed)
TN2019 <- subset(TN2019, select=-applicant_sex_observed)
TN2019 <- subset(TN2019, select=-co_applicant_sex_observed)

# remove other columns not relevant to this analysis
TN2019 <- subset(TN2019, select=-conforming_loan_limit)
TN2019 <- subset(TN2019, select=-lei)

# Ending info: row count = 389728, col count = 52

